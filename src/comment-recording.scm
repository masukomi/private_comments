(module comment-recording
  (
   record-comment
  )

  (import scheme)
  (import chicken.base)
  (import chicken.format)
  (import chicken.io)
  (import chicken.string)
  (import simple-exceptions)
  (import shell)
  (import medea)
  (import masurequests)
  (import srfi-13)

  (define (get-user-info)
    (list
      (cons 'user-email (string-translate*
                          (capture "git config --get user.email")
                          '(("\n" . ""))))
      (cons 'user-name (string-translate*
                         (capture "git config --get user.name")
                         '(("\n" . ""))))))

  ; generates the following JSON and posts it to the private comments server
  ; {
  ;   project_name_hash: "<project name hash>",
  ;   file_patH_hash: "<file path hash>",
  ;   line_number: "<line number>",
  ;   treeish: "<treeish>",
  ;   comment: "<comment>",
  ;   user_name: "<user name>", // optional
  ;   user_email: "<user email>", // optional
  ; }

  (define (record-comment line-number line-treeish-map comment server-info user-info)
    (if (null? user-info)
      (set! user-info (get-user-info)))
    ; TODO Implement better handling of comments on uncommitted lines
    ; lookup the treeish for the line number
    ; SHORT TERM: bail if that line isn't committed
    ; LONG TERM: store it in a temp file
    ;            and wait for the commit before sending it.
    (let ((user-info (get-user-info))
          (treeish (alist-ref line-number line-treeish-map))
          )
      (if treeish
        (begin
          ; (format (current-error-port)
          ;         "XXX comment for ~A <~A> on line ~A with treeish ~A~%comment: ~A~%"
          ;         (alist-ref 'user-name user-info)
          ;         (alist-ref 'user-email user-info)
          ;         line-number
          ;         treeish
          ;         comment)
          (let ((data
                  (list
                    (cons 'project_name_hash (alist-ref 'project-name-hash server-info))
                    (cons 'file_path_hash (alist-ref 'file-path-hash server-info))
                    (cons 'treeish treeish)
                    (cons 'line_number line-number)
                    (cons 'user_name (alist-ref 'user-name user-info))
                    (cons 'user_email (alist-ref 'user-email user-info))
                    (cons 'comment comment)
                    ))
                (pc-url
                  (string-join
                    (list
                      (alist-ref 'pc-url server-info)
                      "v1"
                      "comments")
                     "/"))
                )

              ; example of what we're doing here:
              ; (post-or-die "http://0.0.0.0:8080" "{fake: \"data\"}" "error message ~A~%")
              (post-or-die pc-url (json->string data)
                (sprintf
                  "Unable to connect to Private Comments server at: ~A~% ~~A~%Is it running?~%"
                  pc-url))
            )

        )
        (format (current-error-port)
                "Line ~A has not been committed yet. Please commit it before commenting.~%"
                line-number))

    )
  )


)

