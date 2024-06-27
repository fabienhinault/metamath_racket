#lang br/quicklang
(require brag/support racket/contract)

(module+ test
  (require rackunit))

(define (metamath-token? x)
  (or (eof-object? x) (token-struct? x)))

(module+ test
  (check-true (metamath-token? eof))
  (check-true (metamath-token? (token 'A-TOKEN-STRUCT "hi")))
  (check-false (metamath-token? 42)))

; PRINTABLE-SEQUENCE ::= _PRINTABLE-CHARACTER+
; MATH-SYMBOL ::= (_PRINTABLE-CHARACTER - ’$’)+
; /* ASCII non-whitespace printable characters */
; _PRINTABLE-CHARACTER ::= [#x21-#x7e]
; LABEL ::= ( _LETTER-OR-DIGIT | ’.’ | ’-’ | ’_’ )+
; _LETTER-OR-DIGIT ::= [A-Za-z0-9]
; COMPRESSED-PROOF-BLOCK ::= ([A-Z] | ’?’)+
; /* Define whitespace between tokens. The -> SKIP
; means that when whitespace is seen, it is
; skipped and we simply read again. */
; WHITESPACE ::= (_WHITECHAR+ | _COMMENT) -> SKIP
; /* Comments. $( ... $) and do not nest. */
; _COMMENT ::= ’$(’ (_WHITECHAR+ (PRINTABLE-SEQUENCE - ’$)’)*
; _WHITECHAR+ ’$)’ _WHITECHAR
; /* Whitespace: (’ ’ | ’\t’ | ’\r’ | ’\n’ | ’\f’) */
; _WHITECHAR ::= [#x20#x09#x0d#x0a#x0c]


(define (make-tokenizer port)
  (port-count-lines! port)
  (define (next-token)
    (define metamath-lexer
      (lexer
       ; MATH-SYMBOL
       ; LABEL
       [(char-range #x21 #x7e)
        (token '_PRINTABLE-CHARACTER lexeme)]
       
       [(char-set #\space #\tab #\newline #\return #\page)
        (token '_WHITECHAR lexeme)]
       ;_WHITECHAR : [#x20#x09#x0d#x0a#x0c]
      
       [(from/to "//" "\n") (next-token)]
       [(from/to "@$" "$@")
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@")
               #:position (+ (pos lexeme-start) 2)
               #:line (line lexeme-start)
               #:column (+ (col lexeme-start) 2)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start) 4))]
       [any-char (token 'CHAR-TOK lexeme
                        #:position (pos lexeme-start)
                        #:line (line lexeme-start)
                        #:column (col lexeme-start)
                        #:span (- (pos lexeme-end)
                                  (pos lexeme-start)))]))
    (jsonic-lexer port))
  next-token)
(provide
 (contract-out
  [make-tokenizer (input-port? . -> . (-> jsonic-token?))]))

(module+ test
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "// comment\n")
   empty)
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "@$ (+ 6 7) $@")
   (list (token 'SEXP-TOK " (+ 6 7) "
                #:position 3
                #:line 1
                #:column 2
                #:span 9)))
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "hi")
   (list (token 'CHAR-TOK "h"
                #:position 1
                #:line 1
                #:column 0
                #:span 1)
         (token 'CHAR-TOK "i"
                #:position 2
                #:line 1
                #:column 1
                #:span 1))))
