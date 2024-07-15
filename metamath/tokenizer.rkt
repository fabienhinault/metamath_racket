#lang br
(require "lexer.rkt" brag/support)
                                 
(define (metamath-token? x)
  (or (eof-object? x) (token-struct? x)))

(module+ test
  (require rackunit)
  (check-true (metamath-token? eof))
  (check-true (metamath-token? (token 'A-TOKEN-STRUCT "hi")))
  (check-false (metamath-token? 42)))

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token) (metamath-lexer ip))
  next-token)

(provide make-tokenizer)

(module+ test
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "$( this is a comment $)")
   (list (token 'COMMENT " this is a comment "
                ;         1234567 10 23456 19
                #:skip? #t
                #:position 3
                #:line 1
                #:column 2
                #:span 19)))

  (check-exn exn:fail? (λ ()  (apply-tokenizer-maker make-tokenizer "$( unfinished")))

  (let ((constants (apply-tokenizer-maker make-tokenizer "$c + = $.")))
    ;                                                     123456789  chars
    ;                                                     1 234567   tokens
    (check-pred list? constants)
    (check-equal? (length constants) 7)
    (check-equal? (car constants) (token "$c" "$c"
                                                  #:position 1
                                                  #:line 1
                                                  #:column 0
                                                  #:span 2))
    (check-equal? (cadr constants) (token " " #f
                                          #:position 3
                                          #:line 1
                                          #:column 2
                                          #:span 1
                                          #:skip? #t))
    (check-equal? (caddr constants) (token '_MATH-SYMBOL "+"
                                          #:position 4
                                          #:line 1
                                          #:column 3
                                          #:span 1))
    (check-equal? (last constants) (token "$." "$."
                                          #:position 8
                                          #:line 1
                                          #:column 7
                                          #:span 2))
    )
  (let ((floating (apply-tokenizer-maker make-tokenizer "tt $f term t $.")))
    ;                                                    1234567 10 2 15   chars
    ;                                                    1 23 45   6789   tokens
    (check-pred list? floating)
    (check-equal? (length floating) 9)
    (check-equal? (car floating) (token '_LABEL "tt"
                                        #:position 1
                                        #:line 1
                                        #:column 0
                                        #:span 2))
    (check-equal? (third floating) (token "$f" "$f"
                                            #:position 4
                                            #:line 1
                                            #:column 3
                                            #:span 2))
    (check-equal? (last floating) (token "$." "$."
                                            #:position 14
                                            #:line 1
                                            #:column 13
                                            #:span 2))
    )

  )
