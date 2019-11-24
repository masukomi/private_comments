(module masurequests
  (
   request-or-die
   post-or-die
  )

  (import scheme)
  (import chicken.base)
  (import chicken.condition)
  (import format)
  (import chicken.io)
  (import http-client)
  (import simple-exceptions)

  ; see https://api.call-cc.org/4/doc/http-client
  ; for docs on with-input-from-request

  (define (request-or-die url error-message-with-param)
    (guard
      (e
        (else
          (begin
            (format
              (current-error-port)
              error-message-with-param
              (condition->list e) )
            (exit 5))))
        (with-input-from-request url #f read-string)))


  (define (post-or-die url data-string error-message-with-param)
    (guard
      (e
        (else
          (begin
            ; TODO extract the actual error message from e
            (format
              (current-error-port)
              error-message-with-param
              (condition->list e))
            (exit 8))))
      (with-input-from-request
        url
        data-string
        read-string)))

); end module
