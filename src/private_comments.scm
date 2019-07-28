
(import chicken.syntax)
(import chicken.file)
(import chicken.format)
(import chicken.io)
(import chicken.irregex)
(import chicken.string)
(import chicken.port)
(import chicken.process-context)
(import chicken.condition)
(import srfi-13)
(import srfi-18); multithreading support
(import srfi-1) ; (a)list library
(import filepath)
(import intarweb)
(import medea)
(import simple-loops)
(import spiffy)
(import spiffy-request-vars)
(import uri-common)
(import shell)

(define (list->path a-list)
  (string-join a-list (make-string 1 (filepath:path-separator))))

(define listening-port
  (if (not (get-environment-variable "PRIVATE_COMMENTS_PORT"))
      5749
      (string->number (get-environment-variable "PRIVATE_COMMENTS_PORT"))))

(define base-directory
  (let ((home (get-environment-variable "HOME")))
    (if (not (get-environment-variable "PRIVATE_COMMENTS_DIR"))
      (list->path (list home ".config" "private_comments"))
      (get-environment-variable "PRIVATE_COMMENTS_DIR"))))

(print (sprintf "Base Directory: ~A~%" base-directory) )


(define (guarantee-dir dir-path)
  (if (not (file-exists? dir-path))
    (create-directory dir-path #t))
  
  )
(define (guarantee-git-project project-dir-path)
  (let ((git-dir-path (list->path (list project-dir-path ".git"))))
    (if (not (file-exists? git-dir-path))
          (begin
            (print (sprintf "XXX initializing repo in ~A" project-dir-path) )
            (run ,(sprintf "cd ~A;git init" project-dir-path))
            )
          (print (sprintf "XXX is already a repo: ~A" project-dir-path))
          )))

(define (add-note-to-git project-dir-path note-file)
  ; NOTE: if you push the same note twice git will have a non-zero exit code here.
  ; Intentionally ignoring it.
  (run* ,(sprintf "cd ~A; git add ~A && git commit -m \"added note\"" project-dir-path note-file)))


(define (filename->path-hash filename)
  ; SHA 256 hashes are 65 chars long
  ; filenames are <sha 256 hash>-<line number>.json
  (if (> (string-length filename) 64)
      (substring filename 0 64)
      "NOT-A-COMMENT-FILE"))

(define (strip-redundant-data json-data)
  (alist-delete 'project_name_hash
      (alist-delete 'file_path_hash json-data)))

; returns a string containing the contents
; of the file at the specified file path
; TODO: check if file exists
(define (file-path->string file)
  (let ((file-contents ""))
    (let ((fh (open-input-file file)))
         (let loop((c (read-line fh)))
            (if (eof-object? c)
              (close-input-port fh)
              (begin
                (set! file-contents
                  (string-append file-contents c))
                (loop (read-line fh))))) )
    file-contents))

;TODO add error handling
;WARNING: will blow up if file doesn't contain json
(define (json-file-path->data file-path)
  (strip-redundant-data
    (read-json
      (file-path->string file-path))))


(define (request->data request)
  (if (request-has-message-body?)
      (let ((raw-json (caar (read-urlencoded-request-data request))))
        (read-json (symbol->string raw-json) )
        )
      '()))

(define (pc-headers)
  (list (list 'content-type "application/json")))

(define (has-required-keys? json)
  (let (
         (proj (assoc 'project_name_hash json))
         (file (assoc 'file_path_hash json))
         (treeish (assoc 'treeish json))
         (line-no (assoc 'line_number json))
        )
      (if (and proj file treeish line-no) #t #f)))

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

(define (handle-comments-post request)
  ; TODO
  ; * nice error if invalid json
  ; * test if project dir exists
  ;   * create if needed 
  ; * test if treeish dir exists
  ;   * create if needed
  (let ((json (request->data request)))
    (if (has-required-keys? json)
        
      (let ((project-hash (cdr (assoc 'project_name_hash json)))
            (file-path-hash    (cdr (assoc 'file_path_hash json)))
            (line-no           (cdr (assoc 'line_number json)))
            (treeish           (cdr (assoc 'treeish json))))
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
          (write-string (json->string json) ;guarantees consistent formatting
                        #f (open-output-file file-path))
          (add-note-to-git project-dir inter-repo-file-name)
          (send-response
            headers: (pc-headers)
            status: 'ok
            body: (sprintf "{\"status\": \"SUCCESS\", \"description\": \"~A written\"}" file-name ))
              )
        )
      (send-response 
        headers: (pc-headers)
        status: 'unprocessable-entity
        body: "{\"status\": \"ERROR\", \"description\": \"missing required keys\"}"))))

; /comments?project_name_hash=<hash_here>&file_path_hash=<hash_here>&treeishes=a,b,c
(define (handle-comments-get request)
  (let* ((params (request-vars))
          (project-hash (params 'project_name_hash as-string))
          (file-path-hash (params 'file_path_hash as-string))
          (treeishes
            (delete-duplicates
              (string-split (params 'treeishes as-string) "," #t)) ))
        (send-response
                headers: (pc-headers)
                status: 'ok
                body: (comments-json
                        project-hash
                        file-path-hash
                        (files-for-file
                          project-hash
                          file-path-hash
                          treeishes )))))

(define (handle-unknown-request request)

  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request))
         )
    (send-response
            headers: (pc-headers)
            status: 'not-found
            body: (sprintf "{\"status\": \"UNSUPPORTED\", 
                           \"path\": \"~A\",
                           \"method\": \"~A\"}" path method ))))

(define (shutdown)
  (send-response
        headers: (pc-headers)
        status: 'ok
        body: (sprintf "{\"status\": \"SUCCESS\"}" ))
  (format (current-error-port) "SHUTTING DOWN via /shutdown~%")
  (exit))


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
            (handle-comments-post request)
            ))
      ((equal? path '(/ "shutdown"))
        (shutdown))
      ((equal? path '(/ ""))
        (handle-index request))
      (else (handle-unknown-request request)))
  ); end let
)

(vhost-map `((".*" . ,comment-handler))

           )

(server-port listening-port)
(print (sprintf "Loaded at http://localhost:~A" listening-port))
(print "To shut down the server either use ^C or vist /shutdown")
(start-server)
