#lang br/quicklang

(define (read-syntax path port)
  (define src-datums (port->list read port))
  (define module-datum `(module metamath "reader.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))

(provide read-syntax)



(define-macro (metamath-module-begin EXPR ...)
  #'(#%module-begin EXPR ...))
(provide (rename-out [metamath-module-begin #%module-begin]))

(define constants* '())
(define variables* '())

(define (add-constants constant-symbols)
  (set! constants* (append constants* constant-symbols)))

(define-syntax $c
  (syntax-rules ()
    (($c new-constant ...)
     (add-constants (list 'new-constant ...)))))

(define (add-variables variable-symbols)
  (set! variables* (append variables* variable-symbols)))

(define-syntax $v
  (syntax-rules ()
    (($c new-var ...)
     (add-variables (list 'new-var ...)))))

(provide $c add-constants $v add-variables constants* variables*)