#!/usr/bin/env csi -script

(import args)
(import chicken.file)
(import chicken.format)
(import chicken.io)
(import chicken.irregex)
(import chicken.port)
(import chicken.process-context)
(import chicken.sort)
(import chicken.string)
(import chicken.syntax)
(import filepath)
(import format)
(import http-client)
(import medea)
(import message-digest-byte-vector)
(import sha2)
(import shell)
(import simple-exceptions)
(import simple-loops)
(import srfi-1)
(import srfi-13)
(import srfi-69)

; local libs
(import comment-recording)
(import listicles)
(import masufiles)
(import masurequests)
(import masutils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cli-options
  `(
    (comments-file-path . ,#f)
    (pc-server-url . ,"http://0.0.0.0")
    (pc-server-port . 5749)
    (comment . ,#f)
    (line-number . -1)
    (kill-it . ,#f)
    (debug-it . ,#f)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN HANDLING COMMAND LINE ARGUMENTS
(define opts
  (list
    (args:make-option
      (f file) (required: "<path>")
        "Relative path from root of git repo. Ex. \"src/pc.scm\"")
    (args:make-option
      (c comment) (required: "<Comment>")
        "A comment to be stored")
    (args:make-option
      (d delete) #:none
        "Will delete comment at specified location (line & file)")
    (args:make-option
      (g line-hash) (required: "<Line Hash>")
        "The SHA 256 hash of the line being commented on.")
    (args:make-option
      (l line) (required: "<Line Number>")
        "The line number of the comment to be stored"
        (set! arg (string->number (or arg "-1"))))
    (args:make-option
      (p port) (required: "<Server Port>")
        "Private Comments Server Port [default: 5749]"
        (set! arg (string->number (or arg "5749"))))
    (args:make-option
      (s server) (required: "<Server URL>")
        "Private Comments Server Url [default: http://0.0.0.0]")
    (args:make-option
      (x debug) #:none
        "display comments with debugging output")
    (args:make-option
      (h help) #:none
        "Display this text"
        (usage)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; our Usage message
(define (usage)
 (with-output-to-port (current-error-port)
   (lambda ()
  (print "Private Comments client VERSION_NUMBER_HERE")
  (format #t "Usage: ~A -f file-path [-cdfglsp] [option values]~%" (car (argv)) )
  (print (parameterize (  (args:separator " ")
                          (args:indent    5)
                          (args:width     35))
             (args:usage opts)))
  (format #t "Report bugs at https://github.com/masukomi/private_comments/issues")
  (exit 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN PROCESSING COMMAND LINE ARGS

(receive (options operands)
    (args:parse (command-line-arguments) opts)
    (let (
           (file        (alist-ref 'file      options))
           (comment-arg (alist-ref 'comment   options))
           (line        (alist-ref 'line      options))
           (line-hash   (alist-ref 'line-hash options))
           (server      (alist-ref 'server    options))
           (port        (alist-ref 'port      options))
           (kill        (alist-ref 'delete    options))
           (debug       (alist-ref 'debug     options))
           )
      (if file
        (set-cdr! (assoc 'comments-file-path cli-options) file))
      (if line
        (set-cdr! (assoc 'line-number         cli-options) line))
      (if line-hash
        (set-cdr! (assoc 'line-hash           cli-options) line-hash))
      (if server
        (set-cdr! (assoc 'pc-server-url       cli-options) server))
      (if port
        (set-cdr! (assoc 'pc-server-port      cli-options) port))
      (if comment-arg
        (set-cdr! (assoc 'comment             cli-options) comment-arg))
      (if debug
          (set-cdr! (assoc 'debug-it          cli-options) debug))
      (if kill
          (set-cdr! (assoc 'kill-it           cli-options) kill))
      ))

;; VALIDATIONS
; have they specified the file we're dealing with?
(if (not (alist-ref 'comments-file-path cli-options))
  (usage))

; if we're not killing it
; and there is no comment
; but there is a line number
(if (and
      (not    (alist-ref 'kill-it     cli-options))
      (eq? #f (alist-ref 'comment     cli-options))
      (eq? #f (alist-ref 'line-hash   cli-options))
      (>      (alist-ref 'line-number cli-options) -1))
  (begin
    (format (current-error-port) "You must specify a comment to record on line ~A~%" (alist-ref 'line-number cli-options))
    (exit 13)))

; if there's a comment but no line number
(if (and
      (not (eq? #f (alist-ref 'comment     cli-options)))
      (or (eq?         (alist-ref 'line-number cli-options) -1)
          (eq? #f (alist-ref 'line-hash cli-options))
          ))
  (begin
    (format (current-error-port) "You must specify a line number to comment on & its hash~%")
    (exit 14)))

; if there's no line_hash
(if
      (eq? #f (alist-ref 'line-hash     cli-options))
  (begin
    (format (current-error-port) "You must specify a line hash to ensure we display the comment on the right line after edits.~%")
    (exit 15)))
;; END VALIDATION

;; END PROCESSING COMMAND LINE ARGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN CHECKING PC SERVER STATUS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Figure out what port the server is listening on
; if it's specified on command line use that
; if it's specified in env use that
; otherwise 5749
(define listening-port
  (if (= (alist-ref 'pc-server-port cli-options) -1)
    (if (not
          (get-environment-variable "PRIVATE_COMMENTS_PORT"))
        5749
        (string->number
          (get-environment-variable "PRIVATE_COMMENTS_PORT")))
    (alist-ref 'pc-server-port cli-options)))

; TODO make URL configurable via env var
(define pc-url (sprintf "~A:~A" (alist-ref 'pc-server-url cli-options) listening-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BEGIN REQUIREMENTS CHECKS

(define git-repo-root
   (let (
        (git-output (capture "git rev-parse --show-toplevel"))
        )
    (if (not (string? git-output))
      (begin
        (format (current-error-port) "Must be run from within a git repository")
        (exit 2))
      (string-trim-both git-output))))


(define project-name
    (last (path->list git-repo-root)))

; takes a file path,
; strips out the undesireable folder navigation bits
; converts it to a list
;
; EXAMPLE:
; converts ./foo to foo and ../bar/foo to bar/foo
; and ../bar/../bar/foo to bar/bar/foo which would be bad, but
; there's only so much cleaning I'm willing to do with your
; crazy input people.
; Sheesh.
(define file-path-list
  (filter
     (lambda (elem) (not (or (equal? "." elem) (equal? ".." elem)) ))
     (path->list (alist-ref 'comments-file-path cli-options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get the path to the git repo containing our files
(define repo-file-path
  (list->path
    (append
      '("") ; we need a / at the start of the path but saying '("/") would get us 2 slashes
      (path->list git-repo-root)
      file-path-list)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; confirm that there's a git repo where we expect it.
(if (not (file-exists? repo-file-path))
  (begin
    (format (current-error-port)
            "pc was unable to find ~A~%" repo-file-path)
    (exit 3)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A string of the shell commands we'll execute to get info on a file
; TODO: test if file is known to git
;       git ls-files --error-unmatch <file name>
;       returns error if unknown
(define blame-command
  (sprintf "git blame -c ~A~A~A | awk '{print $1}'"
           git-repo-root
           directory-separator-string
           (alist-ref 'comments-file-path cli-options)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get the git blame info from the
; file we're concerned with.
(define blame-output
  (capture
      ,blame-command))

; there wasn't a file there for git to blame on
(if (not (string? blame-output))
    (begin
      (format (current-error-port) "That file isn't known to git~%")
      (exit 4) ))

; ping private_comments to make sure it's actually running.
; want to know a) that it's reachable b) that it's returning JSON
; We don't really care about specific errors

(read-json
  (get-or-die
    (string-join (list pc-url "status") "/")
    (sprintf
      "Unable to connect to Private Comments server at: ~A~% Error: ~~Y~%Is it running?~%"
      pc-url) ))

; END REQUIREMENTS CHECKS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extract the treeishes from the blame output
(define treeishes
  (string-split blame-output ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map the line number to the treesh responsible for it.
(define line-treeish-map (list-by-index treeishes '() 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; find the saved comments that apply
; to the _current_ version of the file
(define (extract-applicable-comments response-json line-treeish-map)
  ; EXAMPLE RESPONSE JSON
    ; {
    ;   "project_name_hash": "<hash here>",
    ;   "file_path_hash": "<hash here>",
    ;   "comments": [
    ;     {
    ;       "treeish": "<hash here>",
    ;       "line_number": 4,
    ;       "line_hash": "<hash here OR nil>",
    ;       "comment": "comment text here",
    ;       "saved_at": 12345
    ;     }
    ;   ]
    ; }
  (let ((comments-list
          (vector->list
            (alist-ref 'comments response-json))))

    (fold-right
      (lambda (comment clist)
        ; Is the treeish the file has associated with line x
        ; the same treeish this comment has associated with line x?
        (begin
          (let* (
                (line-number     (alist-ref 'line_number comment))
                (comment-treeish (alist-ref 'treeish     comment))
                (treeish-in-map  (alist-ref line-number  line-treeish-map))
                (comment-line-hash (alist-ref 'line_hash comment))
                )
          (if (not (null? comment-line-hash))
              ; if we have a line with the same content still...
              (if (hash-table-ref hash-line-hash-table comment-line-hash)
                  ; add it to the list
                  (cons comment clist)
                  ; skip it
                  clist
                  )
              ; else dealing with an old comment, with no line_hash
              (if (equal? treeish-in-map comment-treeish)
                  ; add it to the list
                  (cons comment clist)
                  ; skip it
                  clist)
              )
          ); end let

        )
        ); end lambda
      '() comments-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generate a hash for a given line
(define (hash-for-line line)
  (message-digest-string
   (sha256-primitive)
   (irregex-replace/all "^[[:space:]]*|[[:space:]]*$" line)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; git show command
(define cat-command
  (sprintf "cat ~A~A~A '"
           git-repo-root
           directory-separator-string
           (alist-ref 'comments-file-path cli-options))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extract line hashes
(define line-hashes
  (map
   (lambda (line) (hash-for-line line))
   (string-split cat-command)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map the hash to the line it came from
(define line-hash-mapping
  (list-by-index line-hashes '() 1)
  )

(define hash-line-hash-table
  (let* (
         (initial-size
           (if (!= (length line-hashes) 0)
             (length line-hashes)
             1))

         (ht
           (make-hash-table
            equal?
            equal?-hash
            initial-size )))
    (map
     (lambda(a-pair)(let ((k (car a-pair))
                          (v (cdr a-pair)))
                      (if (hash-table-exists? ht k)
                          ;append
                          (hash-table-set!
                           ht k
                           (append
                            (hash-table-ref ht k) v))
                          (hash-table-set! ht k (list v))
                          )
                      ))
     line-hash-mapping)

    ht))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sort the comments by line number (ascending)
(define (sort-comments-by-line comments-list)
  (sort comments-list
        (lambda (a b)
          (< (alist-ref 'line_number a)
             (alist-ref 'line_number b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extract the applicable comments, in order
(define (extract-applicable-comments-sorted response-json line-treeish-map)
  (let (
        (applicable-comments
         (extract-applicable-comments response-json line-treeish-map)))
  (sort-comments-by-line applicable-comments
            )))


(define (debug-comments response-json line-treeish-map)
  (format #t "project: ~A~%" project-name)
  (format #t "project hash: ~A~%" project-name-hash)

  (let ((applicable-comments
          (extract-applicable-comments-sorted
            response-json
            line-treeish-map)))

    ; iterate and print
    ; each comment starts with its line number
    ; so "foo berries" on line 12 becomes
    ; "12: foo berries"
    ; "treeish: <treeish here>"
    (if (> (length applicable-comments) 0)
      (begin
        (do-list comment applicable-comments
            (format #t "~A: ~A~%"
                    (cdr (assoc 'line_number comment))
                    (cdr (assoc 'comment     comment)))
            (format #t "treeish: ~A~%~%"
                    (cdr (assoc 'treeish comment))
                    )
            ); end do-list
      ); end begin
      (format #t "No comments found.")
      ) ))
(define (display-comments response-json line-treeish-map)

  (let ((applicable-comments
          (extract-applicable-comments-sorted
            response-json
            line-treeish-map)))

    ; iterate and print
    ; each comment starts with its line number
    ; so "foo berries" on line 12 becomes
    ; "12: foo berries"
    (if (> (length applicable-comments) 0)
      (begin
        (do-list comment applicable-comments
            (format #t "~A: ~A~%~%"
                    (cdr (assoc 'line_number comment))
                    (cdr (assoc 'comment     comment)))
            ); end do-list
      ); end begin
      ) ))


; 4. generate JSON request for private_comments
(define project-name-hash
  (message-digest-string
    (sha256-primitive)
    project-name))

(define file-path-hash
  (message-digest-string
    (sha256-primitive)
    (alist-ref 'comments-file-path cli-options)))

(define server-info
  (list
    (cons 'pc-url  pc-url)
    (cons 'project-name-hash  project-name-hash)
    (cons 'file-path-hash file-path-hash)))


(define (request-comments server-info treeishes )
  (let* (
        (unique-treeishes (delete-duplicates treeishes))
        (comment-request-url
          (sprintf
            "~A/v1/comments?project_name_hash=~A&file_path_hash=~A&treeishes=~A"
            (alist-ref   'pc-url            server-info)
            (alist-ref   'project-name-hash server-info)
            (alist-ref   'file-path-hash    server-info)
            (string-join unique-treeishes ",")))
        (comments-json (read-json
              (get-or-die
                comment-request-url
                "Error retrieving comments:~%~A~%")))
        )
    (if (not (cdr (assoc 'debug-it cli-options)))
          (display-comments comments-json line-treeish-map)
          (begin
            (format #t "file-path-hash: ~A~%" (alist-ref 'file-path-hash server-info))
            (format #t "file treeishes: ~%~{  ~A~%~}" unique-treeishes)
            (debug-comments comments-json line-treeish-map)))))


(define (generate-post-url server-info)
  (string-join
    (list
      (alist-ref 'pc-url server-info)
      "v1"
      "comments")
     "/")
  )
(define (generate-delete-url server-info data line-treeish-map)
  (conc
    (string-join
      (list
        (alist-ref 'pc-url server-info)
        "v1"
        "comments")
       "/")
    ; ?project_name_hash=$PROJECT_NAME_HASH&file_path_hash=$FILE_PATH_HASH&treeish=$TREEISH_ONE&line_number=4
    "?"
    (string-join
      (list
        (sprintf "project_name_hash=~A"
          (alist-ref 'project_name_hash data))
        (sprintf "file_path_hash=~A"
          (alist-ref 'file_path_hash    data))
        (sprintf "treeish=~A"
          (alist-ref
            (alist-ref 'line_number     data)
            line-treeish-map))
        (sprintf "line_number=~A"
          (alist-ref 'line_number       data)))
      "&")
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ARE WE REQUESTING COMMENTS OR CREATING THEM
(if (> (alist-ref 'line-number cli-options) -1) ; recording or deleting a comment
  (let* ((comment-info-alist (prep-comment-info-alist
            (alist-ref 'line-number cli-options)
            line-treeish-map
            (alist-ref 'comment cli-options)
            '() ; user-info TODO: allow this to be input / configured
            server-info))
        (url
          (if (not (alist-ref 'kill-it cli-options))
            (generate-post-url server-info)
            (generate-delete-url
              server-info
              comment-info-alist
              line-treeish-map)
            )
          )
        )
      (post-or-delete-comment
        comment-info-alist
        url
        (if (alist-ref 'kill-it cli-options) 'DETELE 'POST)))
  ; no line specified, probably want everything
  (request-comments server-info treeishes)
  )


(exit 0)
