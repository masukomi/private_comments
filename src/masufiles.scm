(module masufiles
  (
   list->path
   path->list
   read-file-contents
   directory-separator-string
   string->file
   file->string
  )
  (import scheme)
  (import srfi-13)
  (import filepath)
  (import chicken.string)
  (import chicken.io)

  ;debugging only
  (import chicken.base)
  (import chicken.format)
  (import srfi-28)
  (import pathname-expand)

  (define directory-separator-string
    (make-string 1 (filepath:path-separator)))

  (define (list->path a-list)
    (string-join
      a-list
      directory-separator-string))

  (define (path->list path)
    (string-split
      path
      directory-separator-string))

  ; returns a string containing the contents
  ; of the file at the specified file path
  ; TODO: check if file exists
  (define (read-file-contents file-path)
    (let ((file-contents ""))
      (let ((fh (open-input-file file-path)))
           (let loop((c (read-line fh)))
              (if (eof-object? c)
                (close-input-port fh)
                (begin
                  (set! file-contents
                    (string-append file-contents c))
                  (loop (read-line fh))))) )
      file-contents))

  (define (file->string file-path)
    (read-file-contents file-path))

  (define (string->file a-string file-path)
    (format (current-error-port) "XXX asked to write string to ~A~%" file-path)
    (define output-file-port
      (open-output-file
        (pathname-expand file-path)))
    (display a-string output-file-port)
    (close-output-port output-file-port))
)
