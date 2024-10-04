; Private Comments server
; This file contains the private comments local web server.
; This serves as the persistence layer to the various
; Private Comments editor plugins.
;
; By default the server will run on port 5749
; If you define the PRIVATE_COMMENTS_PORT environment variable
; it will use that instead.
;
; If you have a global templatedir git directive that sets up
; a pre-commit hook it _will_ be disabled in the Private Comments
; repos _unless_ you set the PRIVATE_COMMENTS_ALLOW_PRE_COMMIT=true
;
; By default, data will be stored in XDG_DATA_HOME/private_comments
; ~/.config/private_comments
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
(import chicken.condition)
(import srfi-13)
(import srfi-18); multithreading support
(import srfi-1) ; (a)list library + first, and last
(import srfi-69); hash tables
(import filepath)
(import intarweb)
(import medea)
(import sha256-primitive message-digest-byte-vector)
(import simple-loops)
(import spiffy)
(import spiffy-request-vars)
(import uri-common)
(import shell)

;----
; local libs I wrote
(import masutils)
(import masufiles)
(import mdcd-config mdcd)
(import private-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make sure Prerequisets are met

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Figure out what port we're running on
(doc-fun "listening-port"

"## Private: listening-port

### Returns:
Returns the port number that this server will use.
Uses whatever is specified in PRIVATE_COMMENTS_PORT
or 5749 if nothing is specified.
")
(define listening-port
  (if (not (get-environment-variable "PRIVATE_COMMENTS_PORT"))
      5749
      (string->number (get-environment-variable "PRIVATE_COMMENTS_PORT"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(doc-fun "base-directory"

"## Private: base-directory

### Returns:
Returns the directory where private comments data will be stored.
uses PRIVATE_COMMENTS_DIR if specified,
or XDG_DATA_HOME/private_comments if not.")
(define base-directory
  ; get-environment-variable returs #f if it's not found
  (let ((home           (get-environment-variable "HOME"))
        (pc-dir         (get-environment-variable "PRIVATE_COMMENTS_DIR"))
        (xdg-data-home  (get-environment-variable "XDG_DATA_HOME"))
        )
    (if (not pc-dir)
        (if (not xdg-data-home)
          (list->path (list home ".local" "share" "private_comments"))
          (list->path (list xdg-data-home "private_comments")  ))
        pc-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; print basic info for people running it
(print "Private Comments server VERSION_NUMBER_HERE")
(print (sprintf "Base Directory: ~A~%" base-directory) )
(print "  Details: https://github.com/masukomi/private_comments/")

(doc-fun "add-note-to-git"

"## Private: add-note-to-git [project-dir-path note-file]

### Parameters:
* project-dir-path - String - the path to the directory containing the private comments for this project
* note-file - String - the path to the file we will be commiting in git.
")
(define (add-note-to-git project-dir-path note-file)
  ; NOTE: if you push the same note twice git will have a non-zero exit code here.
  ; Intentionally ignoring it.
  ; (format (current-error-port) "XXX adding ~A to dir ~A~%" note-file project-dir-path)
  (run* ,(sprintf "cd ~A; git add ~A && git commit -m \"added note\"" project-dir-path note-file)))

(doc-fun "remove-note-from-git"

"## Private: remove-note-from-git [project-dir-path note-file]
removes a previously saved note from the git repo.
This happens when someone deletes a comment.

")
(define (remove-note-from-git project-dir-path note-file)
  ; NOTE: Intentionally ignoring any complaints about file not existing
  ; & NOT checking if file exists because it may have been
  ; deleted from working dir manually but not removed from git.
  (run* ,(sprintf "cd ~A; git rm -f ~A && git commit -m \"deleting comment\"" project-dir-path note-file)))

(doc-fun "strip-redundant-json-data"

"## Private: strip-redundant-json-data [json-data]

Removes fields from the data supplied by a user
so that redundant data isn't saved in every note for
a given project or file.

### Parameters:
* json-data - alist - data provided by the client to be stored as a comment
"
         )
(define (strip-redundant-json-data json-data)
  (alist-delete 'project_name_hash
      (alist-delete 'file_path_hash json-data)))


;TODO add error handling
;WARNING: will blow up if file doesn't contain json
(define (json-file-path->data file-path)
  (strip-redundant-json-data
    (read-json
      (read-file-contents file-path))))

(define (request->data request)
  (if (request-has-message-body?)
      (let ((raw-json (request-body request)))
        (read-json raw-json)
        )
      '()))

(doc-fun "method-with-body?"

"## Private: method-with-body method
Tests if the provided method is POST or PUT.

Those are the only methods expected to have a body.

### Parameters:
* method - a Spiffy method

### Returns:
`#t` or `'()`

")
(define (method-with-body? method)
    (or (eq? 'POST method)
        (eq? 'PUT method)))

(doc-fun "request-body"

" ## Private: request-body request
Extracts the request body from the request.

### Parameters:
* request - Spiffy request

### Returns:
Returns a string or '()

### Notes:
⚠️ Only supports url-encoded request bodies
_not_ multipart.
")
(define (request-body request)
  (if (method-with-body? (request-method request))
      (let (
              (p (request-port request))
              (len (header-value 'content-length (request-headers request)))
              )
          (read-string len p)
        )
    '()))


(doc-fun "pc-headers"

"## Private: pc-headers
a list of response headers.

### Notes:
It's a normal list with multiple elements
and not an alist or hash because that's what
Spiffy wants

"
)
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
                     line_number)))


(doc-fun "files-for-treeish"

"## Private: files-for-treeish [treeish-dir]

This returns a list of comment files
that were associated with the commit treeish.

### Parameters:
* treeish-dir - A treeish corresponding to a commit in the project
                that has associated notes. It's also the name of
                a directory where we store those notes.

### Returns:
A list of file names contained within the directory
matching the specified treeish or an empty list.

### Notes:
See `files-for-file` documentation for info on the file structure.
")
(define (files-for-treeish treeish-dir)
  (if  (and (file-exists? treeish-dir) (file-readable? treeish-dir))
       (directory treeish-dir)
       '()))


(doc-fun "files-for-files"

"## Private: files-for-file [project-hash-file-path-hash treeishes]



### Notes:
Files are stored in a folder with the same name as
the project hash, in a subfolder named after the treeish
with a file named after the file-path-hash + the line number

I.e. `<project_hash>/<project_commit_treeish>/<file_path_hash>-<line_number>.json`

The project hash, commit treeish, and file path hash are all provided by
the client.

* project hash - typically just a hash of the name of the root folder containing the project
                 May or may not be salted.
* treeish      - a commit treeish in the project which a user has associated a comment with.
                 Salting this would make life difficult for the client
                 and not buy any real security.
* file hash    - a hash of the relative file path from the root of the project
                 E.g. foo/bar/baz.txt would be passed as
                 3de0caa67fb2b4de41f1639e77c3b424bf87972f8002f0cbf0e8505b31584c54
* line number  - the line number the user has associated the comment with
                 Note that the actual line number may have changed since
                 this was recorded.

```
  .
  ├── project_hash_1
  │   ├── treeish_1
  │   │   ├── file_hash-line_no_1.json
  │   │   └── file_hash-line_no_2.json
  │   └── treeish_2
  │       └── file_hash-line_no_3.json
  └── project_hash_2
      └── treeish_3
          └── file_hash-line_no_1.json
```
")
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

(doc-fun "comments-json"

"## Private: comments-json project-hash file-path-hash comments-list
Generates a JSON string with the comments that should be included
in the response body.

### Parameters:
* project-hash   - A hash string - typically of the project name
* file-path-hash - A hash string of the path within the project to the file
* comments-list  - A list of comments for the file in question.
                   This will have been generated by a call to `files-for-file`

### Notes:
See the documentation for `files-for-file`
to understand how project hashes and file path hashes
correspond to files

")
(define (comments-json project-hash file-path-hash comments-list)
  (json->string
    (list
        (cons 'project_name_hash project-hash)
        (cons 'file_path_hash file-path-hash)
        (cons 'comments (list->vector comments-list))
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File Processing Utilities
(define (generate-line-hash line)
  ; generate sha255 hash
  (message-digest-string (sha256-primitive)
    ; downcase
    (string-downcase
      ; remove ALL whitespace
      (irregex-replace/all '(+ whitespace) line ""))))

(doc-fun "file-to-line-hashes"
         "## Private: file-to-line-hashes [file-path]

### Parameters:
* file-path - String - an absolute file path to a
  git controlled file on the same computer as the server

### Returns:
A list of sha256 hashes of the lines contents as generated by
generate-line-hash
")
(define (file-to-line-hashes file-path)
  ; read file
  ; iterate over lines
  ; for each line generate-line-hash
  ; return a list of them

  (let ((hashes '()))

  (do-list var (file->lines file-path)
           (set! hashes (cons hashes (generate-line-hash var))))
    hashes))


(doc-fun "generate-line-hashes-lookup"
"## Private: generate-line-hashes-lookup [line-hashes]

### Parameters:
* line-hashes - Array of Strings - an array of non-unique sha256
  hashes each one representing a line in a file. It is assumed that
  the first element corresponds to the 1st line, and so on.

### Returns:
Returns an associative array where each \"key\" is a unique hash
and each \"value\" is a list of integers (line numbers). The vast
majority of hashes will only have one corresponding line number,
but repeated things like blank lines will have many.
")
(define (generate-line-hashes-lookup line-hashes)
  ; iterate over line-hashes list
  ; create associated list pairing that line hash with a list
  ;   of line numbers on which it appears (almost always just one element in list)
  ; return that associative list
  ;
  (let ((hash-to-list (make-hash-table equal?))
        (counter 1)
        )
    (do-list var line-hashes
             (lambda (h)(progn
                         ; make sure there's a list value to shove data into
                         (if (not (hash-table-exists? hash-to-list var))
                             (hash-table-set! hash-to-list var '()))
                         (hash-table-set! hash-to-list var (cons (hash-table-ref var) counter))
                         (set! counter (+ counter 1)) ;increment the line counter
                         )))
    hash-to-list))

(doc-fun "file-to-commit-hashes"

"## Private: file-to-commit-hashes [file-path]
Queries git for \"blame\" information about the specified
file and creates a unique list of the hashes it returns
as being part of the current file's data.

### Returns:
An list of unique strings. Each string is a short treeish from git.
")
(define (file-to-commit-hashes file-path)
  ; the first 8 characters of every line are a git treeish
  (let (treeishes '())
    (do-list var (read-lines
                    (capture ,(sprintf
                            "cd $(dirname ~A); git blame $(basename ~A)"
                            file-path
                            file-path)))
      ; var is a line like
      ; 1942e1ee src/pc.scm               (masukomi          2019-07-21 23:19:24 -0400 644) (start-server)
      (set treeishes
           (cons treeishes
                 ; the 1st 8 chars are always the treeish
                 (car (string-chop var 8) ) )))

    (delete-duplicates treeishes)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start Handling Web Requests
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(doc-fun "handle-comments-post"
"## Private: handle-comments-post [request]
Records a new / updated comment.

### Parameters:
* request - Spiffy request

The request's data is expected to be JSON with the following structure.
Key order doesn't matter.

```
{
  project_name_hash: \"<project name hash>\",
  file_path_hash: \"<file path hash>\",
  line_number: <line number>,
  treeish: \"<treeish>\",
  comment: \"comment text here\"
}
```

")
(define (handle-comments-post request)
    ; ( (project_name_hash . 1aabeb680a9ed12e4fb53d529513a2aa58341f5cfd0f7790c96388cbddbd5493)
    ;   (file_path_hash . 00c5f3f22a7c1dc3b2e377b650276b6027642ba5b21dc3bb132997f39065870a)
    ;   (treeish . #f)
    ;   (line_number . 4)
    ; )
      (handle-comment 'add (request->data request)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Delete a comment
; DELETE /comments

(doc-fun "handle-comments-delete"
"## Private: handle-comments-delete [request]
Deletes an existing comment, if found.

### Parameters:
* request - Spiffy request

The request's data is expected to be JSON with the following structure.
Key order doesn't matter.

```
{
  project_name_hash: \"<project name hash>\",
  file_path_hash: \"<file path hash>\",
  line_number: <line number>,
  treeish: \"<treeish>\"
}
```
"
)
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
            (treeish           (alist-ref 'treeish params)))
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
              (string->file (json->string params) file-path)
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
; GET /comments?project_name_hash=<hash_here>&file_path_hash=<hash_here>&treeishes=a,b,c
(doc-fun "handle-comments-get"

"## Private: handle-comments-get [request]

### Parameters:
* request - Spiffy request

#### Query String Parameters
* project_name_hash - a sha256 hash of the project name
* file_path_hash    - a sha256 hash of the path to the file
* treeishes         - a comma separated list of treeishes that are present in the
                      current version of the file according to git blame
")
(define (handle-comments-get request)
  (let*
    (
      (params (request-vars))
      (project-hash (params 'project_name_hash as-string))
      (file-path-hash (params 'file_path_hash as-string))
      (treeishes
        (delete-duplicates
          (string-split (params 'treeishes as-string) "," #t)) ))
      ; end param parsing

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
(doc-fun "handle-unknown-request"

"## Private: handle-unknown-request [request]

### Parameters:
* request - Spiffy request

The request here is assumed to not match any
expected endpoints and will be treated as such.
"
)
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
