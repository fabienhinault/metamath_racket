#lang br/quicklang
(require brag/support racket/contract)

(module+ test
  (require rackunit))

(define-lex-abbrev reserved-terms (:or "$c"  "$." "${" "$}" "$v" "$d" "$f" "$e" "$a" "$p" "$=" "?" ))
  

(define metamath-lexer
  (lexer
   [(union #\space #\tab #\newline #\return #\page)
    (token lexeme
           #:skip? #t
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
   [(:+ (union (char-range #\0 #\9) (char-range #\a #\z) (char-range #\A #\Z) #\- #\. #\_))
    (token '_LABEL lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start)))]
   [(:+ (:- (char-range #\! #\~) #\$))
    (token '_MATH-SYMBOL lexeme
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start)))]))

(provide metamath-lexer)
