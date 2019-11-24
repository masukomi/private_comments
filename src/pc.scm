#!/usr/bin/env csi -script

(import chicken.file)
(import chicken.format)
(import chicken.io)
(import chicken.port)
(import chicken.sort)
(import chicken.string)
(import chicken.syntax)
(import chicken.process-context)
(import args)
(import comment-recording)
(import filepath)
(import format)
(import http-client)
(import listicles)
(import masufiles)
(import masurequests)
(import masutils)
(import medea)
(import message-digest-byte-vector)
(import sha2)
(import shell)
(import simple-exceptions)
(import simple-loops)
(import srfi-1)
(import srfi-13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARS WE'LL NEED FROM THE COMMAND LINE INPUT
(define comments-file-path #f)
(define pc-server-url "http://0.0.0.0")
(define pc-server-port 5749)
(define comment #f)
(define line-number -1)


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
      (l line) (required: "<Line Number>")
        "The line number of the comment to be stored"
        (set! arg (string->number (or arg "-1")))
        )
    (args:make-option
      (s server) (required: "<Server URL>")
        "Private Comments Server Url [default: http://0.0.0.0]")
    (args:make-option
      (p port) (required: "<Server Port>")
        "Private Comments Server Port [default: 5749]"
        (set! arg (string->number (or arg "5749")))
                      )
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
  (format #t "Usage: ~A -f file-path [-fclsp] [option values]~%" (car (argv)) )
  (print (parameterize ((args:separator " ")
                          (args:indent 5)
                          (args:width 35))
             (args:usage opts)))
  (format #t "Report bugs at https://github.com/masukomi/private_comments/issues")
  (exit 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN PROCESSING COMMAND LINE ARGS

(receive (options operands)
    (args:parse (command-line-arguments) opts)
    (let (
           (file (alist-ref 'file options))
           (comment-arg (alist-ref 'comment options))
           (line (alist-ref 'line options))
           (server (alist-ref 'server options))
           (port (alist-ref 'port options))
           )
        (if file
          (set! comments-file-path file))
        (if line (set! line-number line))
        (if server (set! pc-server-url server))
        (if port (set! pc-server-port port))
        (if comment-arg (set! comment comment-arg))))


(if (or
      (not comments-file-path)
      (equal? "UNKNOWN" comments-file-path))
  (usage))

;; END PROCESSING COMMAND LINE ARGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN CHECKING PC SERVER STATUS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Figure out what port the server is listening on
; if it's specified on command line use that
; if it's specified in env use that
; otherwise 5749
(define listening-port
  (if (= pc-server-port -1)
    (if (not
          (get-environment-variable "PRIVATE_COMMENTS_PORT"))
        5749
        (string->number
          (get-environment-variable "PRIVATE_COMMENTS_PORT")))
    pc-server-port))

; TODO make URL configurable via env var
(define pc-url (sprintf "~A:~A" pc-server-url listening-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BEGIN REQUIREMENTS CHECKS

(define git-repo-root "UNKNOWN")
(define project-name
  (let ((git-output (capture "git rev-parse --show-toplevel"))  )
    (if (not (string? git-output))
      (begin
        (format (current-error-port) "Must be run from within a git repository")
        (exit 2))
        (set! git-repo-root (string-trim-both git-output))
      )
    (last (path->list git-output))))


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
     (path->list comments-file-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get the path to the git repo containing our files
(define repo-file-path
  (list->path
    (append
      (list directory-separator-string)
      (path->list git-repo-root)
      file-path-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; confirm that there's a git repo where we expect it.
(if (not (file-exists? repo-file-path))
  (begin
    (format (current-error-port)
            "Unable to find ~A~%" repo-file-path)
    (exit 3)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A string of the shell commands we'll execute to get info on a file
; TODO: test if file is known to git
;       git ls-files --error-unmatch <file name>
;       returns error if unknown
(define blame-command
  (sprintf "git blame -c ~A~A~A | awk '{print $1}'"
           git-repo-root
           directory-separator-string
           comments-file-path))


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
  (request-or-die
    (string-join (list pc-url "status") "/")
    (sprintf
      "Unable to connect to Private Comments server at: ~A~% Error: ~~A~%Is it running?~%"
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
    ;       "comment": "comment text here"
    ;     }
    ;   ]
    ; }
  (let ((comments-list
          (vector->list
            (cdr (assoc 'comments response-json)))))

    (fold-right
      (lambda (comment clist)
        ; Is the treeish the file has associated with line x
        ; the same treeish this comment has associated with line x?
        (begin
        (let* (
              (line-number (cdr (assoc 'line_number comment)))
              (comment-treeish (cdr (assoc 'treeish comment)))
              (treeish-in-map (cdr (assoc line-number line-treeish-map)))
              )
        (if (equal?
              treeish-in-map
              comment-treeish
              )
          (cons comment clist)
          clist)
        ); end let

        )
        ); end lambda
      '() comments-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sort the comments by line number (ascending)
(define (sort-comments-by-line comments-list)
  (sort comments-list
        (lambda (a b)
          (< (cdr (assoc 'line_number a))
             (cdr (assoc 'line_number b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extract the applicable comments, in order
(define (extract-applicable-comments-sorted response-json line-treeish-map)
  (let (
        (applicable-comments (extract-applicable-comments
              response-json line-treeish-map)))
  (sort-comments-by-line applicable-comments
            )))

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

          (format #t "~A: ~A~%~%" (cdr (assoc 'line_number comment))
                           (cdr (assoc 'comment comment)))
          ); end do-list
      ); end begin
      ) ))


; 4. generate JSON request for private_comments
(define project-name-hash
  (message-digest-string (sha256-primitive) project-name))

(define file-path-hash
  (message-digest-string (sha256-primitive) comments-file-path))

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
            (alist-ref 'pc-url server-info)
            (alist-ref 'project-name-hash server-info)
            (alist-ref 'file-path-hash server-info)
            (string-join unique-treeishes ",")))

        )

        (display-comments
          (read-json
            (request-or-die
              comment-request-url
              "Error retrieving comments:~%~A~%"))
          line-treeish-map)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ARE WE REQUESTING COMMENTS OR CREATING THEM

(if (not (eq? #f comment)) ; it's either #f or a string
  (if (> line-number -1)
    (record-comment line-number line-treeish-map comment server-info '())
    (begin
      (format (current-error-port)
              "You must specify the line number you're commenting on: ~A ~A~%" line-number comment)
      (exit 5)))
  (request-comments server-info treeishes))



(exit 0)
