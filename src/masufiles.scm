(module masufiles
  (
   list->path
   path->list
   read-file-contents
   directory-separator-string
  )
  (import scheme)
  (import srfi-13)
  (import filepath)
  (import chicken.string)
  (import chicken.io)

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
)
