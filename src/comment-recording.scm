(module comment-recording
  (
   post-or-delete-comment
   prep-comment-info-alist
  )

  (import scheme)
  (import chicken.base)
  (import chicken.format)
  (import chicken.io)
  (import chicken.string)
  (import simple-exceptions)
  (import shell)
  (import medea)
  (import masutils)
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

  (define (prep-comment-info-alist
            line-number
            line-treeish-map
            comment
            user-info
            server-info)

    (if (null? user-info)
      (set! user-info (get-user-info)))

    (let ((treeish
            (alist-ref line-number line-treeish-map)))
      (if (and (not (null? treeish)) (!= treeish "00000000"))
        (begin
          (list
            (cons 'project_name_hash (alist-ref 'project-name-hash server-info))
            (cons 'file_path_hash (alist-ref 'file-path-hash server-info))
            (cons 'treeish treeish)
            (cons 'line_number line-number)
            (cons 'user_name (alist-ref 'user-name user-info))
            (cons 'user_email (alist-ref 'user-email user-info))
            (cons 'comment comment)
            ))
        (begin
          (format (current-error-port)
                  "Line ~A has not been committed yet. I can't act on uncommitted lines.~%"
                  line-number)
          '()))))

  (define (post-or-delete-comment data pc-url post-or-delete)
    ; TODO Implement better handling of comments on uncommitted lines
    ; SHORT TERM: bail if that line isn't committed
    ; LONG TERM: store it in a temp file
    ;            and wait for the commit before sending it.

    ; bail early for uncommitted line...
    (if (null? (alist-ref 'treeish data))
      (format (current-error-port)
                  "Line ~A has not been committed yet. Please commit it before commenting.~%"
                  (alist-ref 'line_number data)))


    (if (equal? post-or-delete 'POST)
      (post-or-die
        pc-url
        (json->string data)
        (sprintf
          "Failed to create on Private Comments server at: ~A~% ~~A~%Is it running?~%"
          pc-url))
      (delete-or-die
        pc-url
        (sprintf
          "Failed to delete from Private Comments server at: ~A~% ~~A~%Is it running?~%"
          pc-url)
        )))
)

