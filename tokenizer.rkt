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
                #:position 3
                #:line 1
                #:column 2
                #:span 19)))

  (let ((unfinished-comment (apply-tokenizer-maker make-tokenizer "$( unfinished")))
    ;                                                              1234567 10 13
    (check-pred list? unfinished-comment)
    (check-equal? (length unfinished-comment) 13)
    (check-equal? (car unfinished-comment) (token '_DOLLAR "$"
                                                  #:position 1
                                                  #:line 1
                                                  #:column 0
                                                  #:span 1))
    (check-equal? (last unfinished-comment) (token '_LETTER-OR-DIGIT "d"
                                                  #:position 13
                                                  #:line 1
                                                  #:column 12
                                                  #:span 1)))

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
    (check-equal? (caddr constants) (token '_MATH-SYMBOL-CHARACTER "+"
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
    ;                                                    1234 567 10  13   tokens
    (check-pred list? floating)
    (check-equal? (length floating) 13)
    (check-equal? (car floating) (token '_LETTER-OR-DIGIT "t"
                                                  #:position 1
                                                  #:line 1
                                                  #:column 0
                                                  #:span 1))
    (check-equal? (fourth floating) (token "$f" "$f"
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
