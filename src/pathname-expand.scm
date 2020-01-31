;;; Pathname expansion, to replace the deprecated core functionality.
;
; Copyright (c) 2014, The CHICKEN Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;
;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in
;   the documentation and/or other materials provided with the
;   distribution.
;
;   Neither the name of the author nor the names of its contributors
;   may be used to endorse or promote products derived from this
;   software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.

(module pathname-expand
    (pathname-expand)

(import chicken.base)
(import chicken.condition)
(import chicken.file)
(import chicken.pathname)
(import chicken.platform)
(import chicken.process-context)
(import chicken.process-context.posix)
(import chicken.fixnum)
(import scheme)
(import srfi-13)


;; Expand pathname starting with "~", and/or apply base directory to
;; relative pathname
;
; Inspired by Gambit's "path-expand" procedure.

(define pathname-expand
  (let* ((home
          ;; Effective uid might be changed at runtime so this has to
          ;; be a lambda, but we could try to cache the result on uid.
          (lambda ()
            (cond-expand
              ((and windows (not cygwin))
               (or (get-environment-variable "USERPROFILE")
                   (get-environment-variable "HOME")
                   "."))
              (else
               (let ((info (user-information (current-effective-user-id))))
                 (list-ref info 5))))))
         (slash
          (cond-expand
            ((and windows (not cygwin)) '(#\\ #\/))
            (else '(#\/))))
         (ts (string-append "~" (string (car slash))))
         (tts (string-append "~" ts)))
    (lambda (path #!optional (base (current-directory)))
      (if (absolute-pathname? path)
          path
          (let ((len (string-length path)))
            (cond
             ((or (string=? "~~" path)
                  (and (fx>= len 3) (string=? tts (substring path 0 3))))
              ;; Repository-path
              (let ((rp (repository-path)))
                (if rp
                    (string-append rp (substring path 2 len))
                    (signal
                     (make-composite-condition
                      (make-property-condition
                       'exn 'location 'pathname-expand
                       'message "No repository path defined"
                       'arguments (list path))
                      (make-property-condition 'pathname-expand)
                      (make-property-condition 'repository-path))))))
             ((or (string=? "~" path)
                  (and (fx> len 2) (string=? ts (substring path 0 2))))
              ;; Current user's home dir
              (string-append (home) (substring path 1 len)))
             ((and (fx> len 0) (char=? #\~ (string-ref path 0)))
              ;; Arbitrary user's home dir
              (let ((rest (substring path 1 len)))
                (if (and (fx> len 1) (memq (string-ref path 1) slash))
                    (string-append (home) rest)
                    (let* ((p (string-index path (lambda (c) (memq c slash))))
                           (user (substring path 1 (or p len)))
                           (info (user-information user)))
                      (if info
                          (let ((dir (list-ref info 5)))
                            (if p
                                (make-pathname dir (substring path p))
                                dir))
                          (signal
                           (make-composite-condition
                            (make-property-condition
                             'exn 'location 'pathname-expand
                             'message "Cannot expand homedir for user"
                             'arguments (list path))
                            (make-property-condition 'pathname-expand)
                            (make-property-condition 'username))))))))
             (else (make-pathname base path))))))))

)
