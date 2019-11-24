(import chicken.format)
(import chicken.file)
(import chicken.io)
(import srfi-13)
(import masufiles)
(import coops)

(define-class <file-repo> () 
              (
                (file-path     "UNKNOWN")
                (git-repo-root "UNKNOWN") ; set in initializer
                (project-name  "UNKNOWN")  ; set in initializer
              ))


(define-method (initialize-instance (fr <file-repo>))
    (call-next-method)
    (let ((git-output (capture "git rev-parse --show-toplevel"))  )
        (if (not (string? git-output))
          (begin
            (format (current-error-port) "Must be run from within a git repository")
            (exit 2))
            (set! (slot-value fr 'git-repo-root) (string-trim-both git-output))))
    (set! (slot-value fr 'project-name)
      (last (path->list (slot-value fr 'git-repo-root)))) )

; converts ./foo to foo and ../bar/foo to bar/foo
; and ../bar/../bar/foo to bar/bar/foo which would be bad, but
; there's only so much cleaning I'm willing to do with your
; crazy input people.
; Sheesh.
(define-method (fr-file-path-list (fr <file-repo>))
  (filter
     (lambda (elem) (not (or (equal? "." elem) (equal? ".." elem)) ))
     (path->list (slot-value fr 'file-path))))

(define-method (fr-file-path (fr <file-repo))
   (list->path
     fr-file-path-list))

(define-method (fr-repo-file-path (fr <file-repo>))
  (list->path
    (append
      (list directory-separator-string)
      (path->list (slot-value fr 'git-repo-root))
      fr-file-path-list)))
               
(define-method (fr-file-exists? (fr <file-repo>))
  (file-exists? (fr-repo-file-path fr)))

(define-method (fr-known-file? (fr <file-repo>))
  (let ((git-output 
          (capture 
            (sprintf 
              "git ls-files --error-unmatch ~A" 
              (fr-file-path fr))))  )
        (if (string? git-output) #t #f)))

(define-method (valid-or-die (fr <file-repo>))
  (if (not (fr-file-exists? fr))
      (begin 
      (format (current-error-port)
              "Unable to find ~A~%" (fr-repo-file-path fr)
               )
      (exit 3)))
  (if (not (fr-known-file? fr))
      (begin
        (format (current-error-port)
              "File is not currently tracked by git: ~A~%"
              (fr-repo-file-path fr))
        (exit 4)))
  
  ;TODO handle other invalidities
  #t ; valid! 
)


