#lang br/quicklang
(require brag/support racket/contract)

(module+ test
  (require rackunit))

(define-lex-abbrev reserved-terms (:or "$c"  "$." "${" "$}" "$v" "$d" "$f" "$e" "$a" "$p" "$=" "?" ))
  

(define metamath-lexer
  (lexer
   [(union #\space #\tab #\newline #\return #\page)
    (token lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span 1)]
   [(char-complement (char-range  #\! #\~))
    (token 'FORBIDDEN lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span 1)]
   [(from/to "$(" "$)")
    (token 'COMMENT (trim-ends "$(" lexeme "$)")
           #:skip? #t
           #:position (+ (pos lexeme-start) 2)
           #:line (line lexeme-start)
           #:column (+ (col lexeme-start) 2)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start) 4))]
   [reserved-terms (token lexeme lexeme
                          #:position (pos lexeme-start)
                          #:line (line lexeme-start)
                          #:column (col lexeme-start)
                          #:span (- (pos lexeme-end)
                                    (pos lexeme-start)))]
   [ #\$ (token '_FORBIDDEN_DOLLAR lexeme
                #:position (pos lexeme-start)
                #:line (line lexeme-start)
                #:column (col lexeme-start)
                #:span 1)]
   [(union (char-range #\0 #\9) (char-range #\a #\z) (char-range #\A #\Z))
    (token '_LETTER-OR-DIGIT lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span 1)]
   [(:or #\- #\. #\_)
    (token '_LABEL-CHARACTER lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span 1)]
   [(char-range #\! #\~)
    (token '_MATH-SYMBOL-CHARACTER lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span 1)]))

(provide metamath-lexer)
