#lang br/quicklang

(define (read-syntax path port)
  (define src-datums (read port))
  (define module-datum `(module metamath br
                          '(,@src-datums)))
  (datum->syntax #f module-datum))

(provide read-syntax)