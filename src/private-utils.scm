(module private-utils
  (
   guarantee-dir
   guarantee-git-project
   disable-pre-commits
   filename->path-hash
  )
  ; (import chicken)
  (import chicken.base)
  (import chicken.format)
  (import chicken.sort)
  (import scheme)
  (import shell)
  (import srfi-1) ;define's first and last
  (import srfi-69)

  ;----
  ; local libs I wrote
  (import masufiles)
  (import mdcd-config mdcd)

(doc-fun "guarantee-dir"

"## Private: guarantee-dir [dir-path]
Guarantees that the specified directory exists.

### Parameters:
* dir-path - String a path to a directory that may or may not exist")
(define (guarantee-dir dir-path)
  (if (not (file-exists? dir-path))
    (begin
      ; (format (current-error-port) "XXX didn't exist. creating: ~A~%" dir-path)
      ; (create-directory dir-path 'with-parents) ; <-- doesn't work!!
      (run ,(sprintf "mkdir -p ~A" dir-path)))))

(doc-fun "guarantee-git-project"

"## Private: guarantee-git-project [project-dir-path]
Tests if the indicated directory is a git repository
by looking for the .git dir within. If it's not present
it will initialize a git repo within it.

### Parameters:
* project-dir-path - String a directory path

### Notes:
This is _not_ a generic function, that could do things like create bare repos.
Instead it is intended to create the repo that private comments will use to store
a project's comments in.
")
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
(doc-fun "disable-pre-commits"
"## Private: disable-pre-commits [project-dir-path git-dir-path]
If the user has configured git templates for new repos then
pre-commit hooks may show up when we init a new dir for
storing private comments in.

### Parameters:
* project-dir-path - String - a path to the directory containing this projects private comments
* git-dir-path - String - a path to the .git dir within the project

### Notes:
You can override this behavior by setting
PRIVATE_COMMENTS_ALLOW_PRE_COMMIT to \"true\"
")
(define (disable-pre-commits project-dir-path git-dir-path)
  (if (not (equal? (get-environment-variable "PRIVATE_COMMENTS_ALLOW_PRE_COMMIT") "true"))
    ; If "true", I hope you know what you're doing. ;)
    (let ((pre-commit-path (list->path (list git-dir-path "hooks" "pre-commit"))))
        (if (and (file-exists? pre-commit-path) (file-executable? pre-commit-path))
            (begin
              (set-file-permissions! pre-commit-path 660)
              ; now disable the git warning about it not being executable
              ; _in this repo only_
              (run (sprintf "cd ~A; git config advice.ignoredHook false" project-dir-path))
              )))))


(doc-fun "filename->path-hash"


"## Private: filename->path-hash [filename]
extracts the path hash from a filename that is expected to also contain a line
number and a suffix.

### Parameters:
* filename - a Private Comments file name. These take the form of
             <file path hash>-<line_number>.json

### Returns:
The 65 character Sha256 hash at the beginning of the file name
or \"NOT-A-COMMENT-FILE\" if the filename doesn't appear to match
our naming convention.

If this ever returns \"NOT-A-COMMENT-FILE\" it means someone's been
mucking about with directories they shouldn't.

### Notes:
The test for file name format is simplistic at best, but
as we should be the ones creating files in these directories
we shouldn't have to worry much.

")
(define (filename->path-hash filename)
  ; SHA 256 hashes are 65 chars long
  ; filenames are <sha 256 hash>-<line number>.json
  (if (> (string-length filename) 64)
      (substring filename 0 64)
      "NOT-A-COMMENT-FILE"))


)
