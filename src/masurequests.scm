(module masurequests
  (
   delete-or-die
   get-or-die
   post-or-die
   put-or-die
   request-or-die
  )

  (import scheme)
  (import chicken.base)
  (import chicken.condition)
  (import format)
  (import chicken.io)
  (import uri-common)
  (import intarweb)
  (import http-client)
  (import simple-exceptions)

  ; see https://api.call-cc.org/4/doc/http-client
  ; for docs on with-input-from-request


  (define (delete-or-die url error-message-with-param)
    (request-or-die url #f 'DELETE error-message-with-param))

  (define (get-or-die url error-message-with-param)
    (request-or-die url #f 'GET error-message-with-param))

  (define (post-or-die url data-string error-message-with-param)
    (request-or-die url data-string 'POST error-message-with-param))


  (define (put-or-die url data-string error-message-with-param)
    (request-or-die url data-string 'PUT error-message-with-param))

  (define (request-or-die
            url
            data-string  ; alist or #f
            request-type ; 'GET, 'PUT, 'POST, 'DELETE, 'HEAD
            error-message-with-param
            )
    (guard
      (e
        (else
          (begin
            (format
              (current-error-port)
              error-message-with-param
              (condition->list e) )
            (exit 9))))
      (with-input-from-request
        (make-request method: request-type      ; make-request from intarweb
                      uri: (uri-reference url)) ; uri-reference from uri-common
        data-string ; may be #f
        read-string)
    ))




)
