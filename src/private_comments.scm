; Private Comments server
; This file contains the private comments local web server.
; This serves as the persistence layer to the various
; Private Comments editor plugins.
;
; By default the server will run on port
; If you define the PRIVATE_COMMENTS_PORT environment variable
; it will use that instead.
;
; If you have a global templatedir git directive that sets up
; a pre-commit hook it _will_ be disabled in the Private Comments
; repos _unless_ you set the PRIVATE_COMMENTS_ALLOW_PRE_COMMIT=true
;
; Data will be stored in ~/.config/private_comments
; If you define the PRIVATE_COMMENTS_DIR evironment variable
; it will use that instead.

(import scheme)
(import chicken.base)
(import chicken.syntax)
(import chicken.file)
(import chicken.file.posix)
(import chicken.format)
(import chicken.io)
(import chicken.irregex)
(import chicken.string)
(import chicken.port)
(import chicken.process-context)
(import chicken.time)
(import chicken.condition)
(import srfi-13)
(import srfi-18); multithreading support
(import srfi-1) ; (a)list library + first, and last
(import filepath)
(import intarweb)
(import medea)
(import simple-loops)
(import spiffy)
(import spiffy-request-vars)
(import uri-common)
(import shell)
(import masutils)
(import masufiles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make sure Prerequisets are met

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Figure out what port we're running on
(define listening-port
  (if (not (get-environment-variable "PRIVATE_COMMENTS_PORT"))
      5749
      (string->number (get-environment-variable "PRIVATE_COMMENTS_PORT"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(define base-directory
  (let ((home (get-environment-variable "HOME")))
    (if (not (get-environment-variable "PRIVATE_COMMENTS_DIR"))
      (list->path (list home ".config" "private_comments"))
      (get-environment-variable "PRIVATE_COMMENTS_DIR"))))
(print "Private Comments server VERSION_NUMBER_HERE")
(print (sprintf "Base Directory: ~A~%" base-directory) )
(print "  Details: https://github.com/masukomi/private_comments/")


(define (guarantee-dir dir-path)
  (if (not (file-exists? dir-path))
    (begin
      ; (format (current-error-port) "XXX didn't exist. creating: ~A~%" dir-path)
      ; (create-directory dir-path 'with-parents) ; <-- doesn't work!!
      (run ,(sprintf "mkdir -p ~A" dir-path))
      )

    )

  )
(define (guarantee-git-project project-dir-path)
  (let ((git-dir-path (list->path (list project-dir-path ".git"))))
    (guarantee-dir project-dir-path)
    (if (not (file-exists? git-dir-path))
          (begin
            (print (sprintf "initializing repo in ~A" project-dir-path) )
            (run ,(sprintf "cd ~A;git init" project-dir-path))
            (disable-pre-commits project-dir-path git-dir-path)
            ))))

; makes sure that if any pre-commit files came along for the
; ride in the `git init` phase they are disabled
; this could happen (and did) as the result of a global templatdir config
(define (disable-pre-commits project-dir-path git-dir-path)
  (if (not
       (equal? (get-environment-variable "PRIVATE_COMMENTS_ALLOW_PRE_COMMIT")
            "true"))
    ; If "true", I hope you know what you're doing. ;)
    (let ((pre-commit-path (list->path (list git-dir-path "hooks" "pre-commit"))))
        (if (and (file-exists? pre-commit-path) (file-executable? pre-commit-path))
            (begin
              (set-file-permissions! pre-commit-path 660)
              ; now disable the git warning about it not being executable
              ; _in this repo only_
              (run (sprintf "cd ~A; git config advice.ignoredHook false" project-dir-path))
              )))))

(define (add-note-to-git project-dir-path note-file)
  ; NOTE: if you push the same note twice git will have a non-zero exit code here.
  ; Intentionally ignoring it.
  ; (format (current-error-port) "XXX adding ~A to dir ~A~%" note-file project-dir-path)
  (run* ,(sprintf "cd ~A; git add ~A && git commit -m \"added note\"" project-dir-path note-file)))


(define (remove-note-from-git project-dir-path note-file)
  ; NOTE: Intentionally ignoring any complaints about file not existing
  ; & NOT checking if file exists because it may have been
  ; deleted from working dir manually but not removed from git.
  (run* ,(sprintf "cd ~A; git rm -f ~A && git commit -m \"deleting comment\"" project-dir-path note-file)))


(define (filename->path-hash filename)
  ; SHA 256 hashes are 65 chars long
  ; filenames are <sha 256 hash>-<line number>.json
  (if (> (string-length filename) 64)
      (substring filename 0 64)
      "NOT-A-COMMENT-FILE"))

(define (strip-redundant-data json-data)
  (alist-delete 'project_name_hash
      (alist-delete 'file_path_hash json-data)))



;TODO add error handling
;WARNING: will blow up if file doesn't contain json
(define (json-file-path->data file-path)
  (strip-redundant-data
    (read-json
      (read-file-contents file-path))))


(define (request->data request)
  (if (request-has-message-body?)
      (let ((raw-json (request-body request)))
        (read-json raw-json)
        )
      '()))

(define (method-with-body? method)
    (or (eq? 'POST method)
        (eq? 'PUT method)))

(define (request-body request)
  (if (method-with-body? (request-method request))
      (let (
              (p (request-port request))
              (len (header-value 'content-length (request-headers request)))
              )
          (read-string len p)
        )
    '()))
  ;WARNING: only supports url-encoded post body
  ;          not multipart


(define pc-headers
  (list (list 'content-type "application/json")))

(define (json-has-key? json key)
  (if (assoc key json) #t #f))

(define (json-has-keys? json keys)
  (reduce
    (lambda (a b)(and a b))
    #t
    (map (lambda (key)(begin

                        (json-has-key? json key))) keys)
    ))

(define (has-required-keys? json)
  (json-has-keys? json '(project_name_hash
                     file_path_hash
                     treeish
                     line_number
                     line_hash)))

(define (files-for-treeish treeish-dir)
  (if  (and (file-exists? treeish-dir) (file-readable? treeish-dir))
       (directory treeish-dir)
       '()))
; NOTE:
; files are stored in a folder with the same name as
; the project hash, in a subfolder named after the treeish
; with a file named after the file-path-hash + the line number
;
; .
; ├── project_hash_1
; │   ├── treeish_1
; │   │   ├── file_hash-line_no_1.json
; │   │   └── file_hash-line_no_2.json
; │   └── treeish_2
; │       └── file_hash-line_no_3.json
; └── project_hash_2
;     └── treeish_3
;         └── file_hash-line_no_1.json
(define (files-for-file project-hash file-path-hash treeishes )
  (let ((project-dir (list->path (list base-directory project-hash)))
        (comment-maps '()))
      (do-list treeish treeishes
        (let* ((treeish-dir (list->path (list project-dir treeish)))
               (files (files-for-treeish treeish-dir)))
          (do-list file files
           ; test if they start with file-path-hash
           ; there may be many: one for each line with a comment
           (let ((hash-from-filename (filename->path-hash file))
                 (file-path (list->path (list treeish-dir file))))
            (if (equal? file-path-hash hash-from-filename)

              (set! comment-maps
               (append comment-maps
                (list
                  (json-file-path->data file-path)))))))))

    comment-maps))

(define (comments-json project-hash file-path-hash comments-list)
  (json->string
    (list
        (cons 'project_name_hash project-hash)
        (cons 'file_path_hash file-path-hash)
        (cons 'comments (list->vector comments-list))
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start Handling Web Requests
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Store a new comment
;
; Expected JSON input (key order doesn't matter)
; {
;   project_name_hash: "<project name hash>",
;   file_path_hash: "<file path hash>",
;   line_number: <line number>,
;   treeish: "<treeish>",
;   comment: "comment text here"
; }
(define (handle-comments-post request)
    ; ( (project_name_hash . 1aabeb680a9ed12e4fb53d529513a2aa58341f5cfd0f7790c96388cbddbd5493)
    ;   (file_path_hash . 00c5f3f22a7c1dc3b2e377b650276b6027642ba5b21dc3bb132997f39065870a)
    ;   (treeish . #f)
    ;   (line_number . 4)
    ; )
      (handle-comment 'add (request->data request)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Delete a comment
; /comments
;
; Expected JSON input (key order doesn't matter)
; {
;   project_name_hash: "<project name hash>",
;   file_path_hash: "<file path hash>",
;   line_number: <line number>,
;   treeish: "<treeish>"
; }
(define (handle-comments-delete request)
  (handle-comment 'delete (uri-query (request-uri request)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Delete & Create & Update
;
(define (handle-comment add-or-delete params)
  ;TODO Update headers to specify
    (if (and
          (has-required-keys? params)
          (not (or
              (== (alist-ref 'treeish params) "00000000")
              (null? (alist-ref 'treeish params)))))

      (let ((project-hash      (alist-ref 'project_name_hash params))
            (file-path-hash    (alist-ref 'file_path_hash params))
            (line-no           (alist-ref 'line_number params))
            (treeish           (alist-ref 'treeish params))
            ; record the epoch time when the comment was added
            (dated-params      (cons (list 'saved_at (current-seconds)) params))
            )
        (let* (
              (project-dir (list->path (list base-directory project-hash)))
              (treeish-dir (list->path (list base-directory project-hash treeish)))
              (file-name (sprintf "~A-~A.json" file-path-hash line-no))
              (inter-repo-file-name (list->path (list treeish file-name)))
              (file-path (list->path
                           (list treeish-dir
                                 file-name)))
              )

          (guarantee-dir treeish-dir)
          (guarantee-git-project project-dir)
          ; TODO switch on add-or-delete
          (if (equal? add-or-delete 'add)
            (begin ; ADD
              ; (format (current-error-port) "XXX WRITING to ~A~%" file-path)
              (string->file (json->string dated-params) file-path)
              ; guarantees consistent formatting
              ; passes _everything_ to the filesystem
              ; including unexpected key value pairs
              (add-note-to-git project-dir inter-repo-file-name))
            ; else DELETE...
            (remove-note-from-git project-dir inter-repo-file-name)
          )
          (send-response
            headers: pc-headers
            status: 'ok
            body: (if (equal? add-or-delete 'add)
                    (sprintf "{\"status\": \"SUCCESS\", \"description\": \"~A written\"}" file-name )
                    "{\"status\": \"SUCCESS\", \"description\": \"comment removed\"}"  ))
          ) ; end let*
      ) ; end if true's let let
      ; else...
      (begin
        (send-response
          headers: pc-headers
          status: 'unprocessable-entity
          body: (sprintf
                  "{\"status\": \"ERROR\", \"description\": \"Missing required keys. Only had these: ~A\"}" params ))) ; end if false's begin
    ) ; end if

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Request a comment
; /comments?project_name_hash=<hash_here>&file_path_hash=<hash_here>&treeishes=a,b,c
(define (handle-comments-get request)
  (let*
    (
      (params (request-vars))
      (project-hash (params 'project_name_hash as-string))
      (file-path-hash (params 'file_path_hash as-string))
      (treeishes
        (delete-duplicates
          (string-split (params 'treeishes as-string) "," #t)) ))
      (send-response
        headers: pc-headers
        status: 'ok
        body: (comments-json
                project-hash
                file-path-hash
                (files-for-file
                  project-hash
                  file-path-hash
                  treeishes )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WTF is this you've sent me?
(define (handle-unknown-request request)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request))
         )

    (send-response
            headers: pc-headers
            status: 'not-found
            body: (sprintf "{\"status\": \"UNSUPPORTED\",
                           \"path\": \"~A\",
                           \"method\": \"~A\"}" path method ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; You want status? We got status.
; TODO add count of comment files
(define (status-page)

  (send-response
            headers: pc-headers
            status: 'ok
            body: (sprintf "{\"status\": \"ALIVE\"}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shutdown: I needed to rest anyway.
(define (shutdown)
  (send-response
        headers: pc-headers
        status: 'ok
        body: (sprintf "{\"status\": \"SUCCESS\"}" ))
  (format (current-error-port) "SHUTTING DOWN via /shutdown~%")
  (exit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; index request
; Many users will go here out of curiosity.
(define (handle-index request)

  (send-response
    status: 'ok
    body: "<h2>Hello</h2> <p>This is a Private Comments REST server. <br />Please see <a href='https://masukomi.github.io/private_comments'>the site</a> for usage instructions.</p>"))

(define (comment-handler continue)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request))
         )
    (cond
      ((equal? path '(/ "v1" "comments"))
        (if (equal? method 'GET)
            (handle-comments-get request)
            (if (equal? method 'DELETE)
              (handle-comments-delete request)
              (handle-comments-post request) ; PUT and POST and PATCH handled by same function
            )))
      ((equal? path '(/ "shutdown"))
        (shutdown))
      ((equal? path '(/ "status"))
        (status-page))
      ((equal? path '(/ ""))
        (handle-index request))
      (else (handle-unknown-request request)))
  ); end let
)

(vhost-map `((".*" . ,comment-handler))

           )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; START SERVING!
(server-port listening-port)
(print (sprintf "Loaded at http://localhost:~A" listening-port))
(print "To shut down the server either use ^C or vist /shutdown")
(start-server)
