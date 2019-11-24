(module masutils
  (
   ; comparison tools
    ==
    !=
    ===
    !==
    boolify
    get-type
    get-type-string
    )

  (import chicken.base)
  (import scheme)

  (define (== a b)
    (equal? a b))

  (define (!= a b)
    (not (== a b)))

  (define (=== a b)
    (eq? a b))

  (define (!== a b)
    (not (=== a b)))

  (define (boolify x)
    (not (not x)))

  (define (get-type x)
    (cond
      ((complex? x)  'complex)
      ((real? x)     'real)
      ((rational? x) 'rational)
      ((integer? x)  'integer)
      ((number? x)   'number)
      ((list? x)     'list)
      ((pair? x)     'pair); a pair is a list but a list may not be a pair
      ((string? x)   'string)
      ((boolean? x)  'boolean)
      ((null? x)     'null)
      ((symbol? x)   'symbol)
      ((char? x)     'character)
      ((vector? x)   'vector)
      ((procedure? x)'procedure)
      ((input-port? x) 'input-port)
      ((output-port? x) 'output-port)
      ((eof-object? x) 'eof-object)
      (else          'unknown)))

  (define (get-type-string x)
    (symbol->string (get-type x)))

  )

