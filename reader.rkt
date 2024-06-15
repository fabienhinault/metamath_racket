#lang br/quicklang

(require racket)
(require (for-syntax racket))

(module+ test
  (require rackunit))

(module+ test-for-syntax
  (require (for-syntax rackunit)))

(define (quote-cdr l)
  (cons (car l) (map (λ (_) (list 'quote _)) (cdr l))))

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
(define floating-hypotheses* '())
(define provables* '())

(define-macro ($c . CONSTANTS)
  #'(add-constants 'CONSTANTS))

(define (add-constants constants-symbols)
  (set! constants* (append constants* constants-symbols)))


(define-macro ($v . VARIABLES)
  #'(add-variables 'VARIABLES))

(define (add-variables variable-symbols)
  (set! variables* (append variables* variable-symbols)))

(define-macro ($f NAME CONSTANT VARIABLE)
  #'(define NAME (add-floating-hypothesis 'NAME 'CONSTANT 'VARIABLE)))

(define (add-floating-hypothesis name constant variable)
  (when (not (member constant constants*))
    (error (~a constant " is not a constant.")))
  (when (not (member variable variables*))
    (error (~a variable " is not a variable.")))
  (let ((res (create-floating-hypothesis name constant variable)))
    (set! floating-hypotheses* (append floating-hypotheses* (list res)))
    res))

(define (create-floating-hypothesis name constant variable)
  (cons variable
        (hash
         'name name
         'statement (list constant variable)
         'step (λ (stack pds) (cons (list constant variable) stack)))))

(module+ test
  (let ((hyp-term-u (cdr (create-floating-hypothesis 'tu 'term 'u))))
    (check-equal? (hash-ref hyp-term-u 'name) 'tu)
    (check-equal? (hash-ref hyp-term-u 'statement) '(term u))
    (check-equal? ((hash-ref hyp-term-u 'step) '() '()) '((term u)))))
                
(define (get-present-variables essentials all-variables)
  (remove-duplicates (filter (λ (_) (member _ all-variables)) (flatten essentials))))

(module+ test
  (check-equal?
   (get-present-variables '((TT P) (TT (-> P Q))) '(u r s P Q))
   '(P Q)))

; extract from all-floatings the floating hypotheses needed by essentials
(define (get-floating-hypotheses essentials all-floatings all-variables)
  (let ((vs  (get-present-variables essentials all-variables)))
    (map (λ (_) (hash-ref (cdr _) 'statement)) (filter (λ (_) (member (car _) vs)) all-floatings))))

(module+ test
  (define (create-floating-hypotheses cvs)
    (map (λ (cv)
           (create-floating-hypothesis (car cv) (cadr cv) (caddr cv)))
         cvs))
  (define test-consts '(term wff + 0 = TT |(| |)|))
  (define test-vars '(u r s P Q))
  (define test-floats (create-floating-hypotheses '((tu term u) (tr term r) (ts term s)
                                                                (wp wff P) (wq wff Q))))
  (check-equal?
   (get-floating-hypotheses '((TT P) (TT  |(| P -> Q |)|)) test-floats test-vars)
   '((wff P) (wff Q))))


(define (evenths l)
  (cond ((null? l) '())
        ((null? (cdr l)) '())
        (else (cons (cadr l) (evenths (cddr l))))))

(module+ test
  (check-equal? (evenths '(min (TT P) maj (TT (-> P Q)))) '((TT P) (TT (-> P Q)))))

(define-for-syntax (treewise combine base tree)
  (if (pair? tree)
      (combine (treewise combine base (car tree)) (treewise combine base (cdr tree)))
      (base tree)))

(module+ test-for-syntax)

(define (bind-hypothesis hypothesis statement all-variables all-constants)
  (cond
    ((equal? hypothesis statement)
     '())
    ((null? hypothesis)
     (if (not (null? statement))
         (error (~a "impossible to bind " hypothesis " and " statement))
         '()))
    (else
     (let ((hypothesis-symbol (car hypothesis)))
       (cond ((member hypothesis-symbol all-constants)
              (let ((statement-symbol (car statement)))
                (if (not (equal? hypothesis-symbol statement-symbol))
                    (error (~a "impossible to bind constant " hypothesis-symbol " to " statement-symbol))
                    (bind-hypothesis (cdr hypothesis) (cdr statement) all-variables all-constants))))
             ((and (member hypothesis-symbol all-variables) (null? (cdr hypothesis)))
              (list (cons hypothesis-symbol statement)))
             (else
              (error "illegal state")))))))

(module+ test
  (check-equal?
   (bind-hypothesis '(term u) '(term u) test-vars test-consts)
   '())

  (check-equal?
   (bind-hypothesis '(term r) '(term 0) test-vars test-consts)
   '((r 0)))

  (check-equal?
   (bind-hypothesis '(term u) '(term |(| u + 0 |)|) test-vars test-consts)
   '((u . (|(| u + 0 |)|))))
  )

(define (chk-cons binding bindings)
  (let ((other-binding (assoc (car binding) bindings)))
    (if other-binding
        (if (not equal? (cdr binding) (cdr other-binding))
            (error (~a (car binding) "bound to " (cdr binding) " and " (cdr other-binding)))
            bindings)
        (cons binding bindings))))

(define (chk-append bindings other-bindings)
  (if (null? bindings)
      other-bindings
      (chk-cons (car bindings) (chk-append (cdr bindings) other-bindings))))

(define (bind-hypotheses hypotheses statements all-variables all-constants)
  (if (null? hypotheses)
      '()
      (let* ((bindings (bind-hypothesis (car hypotheses) (car statements) all-variables all-constants))
             (substituted-hyps
              (map (λ (hyp) (substitute-statement hyp bindings))
                   (cdr hypotheses))))
        (chk-append
         bindings
         (bind-hypotheses substituted-hyps (cdr statements) all-variables all-constants)))))

(module+ test
  (check-equal?
   (bind-hypotheses '((term u) (term r)) '((term u) (term 0)) test-vars test-consts)
   '((r 0))))

(define (substitute-statement statement bindings)
  (if (null? statement)
      '()
      (append (substitute-statement-symbol (car statement) bindings)
              (substitute-statement (cdr statement) bindings))))

(define (substitute-statement-symbol statement-symbol bindings)
  (let ((binding (assoc statement-symbol bindings)))
    (if (not binding)
        (list statement-symbol)
        (cdr binding))))

(define-macro ($a NAME ESSENTIALS DISTINCTS CONCLUSION)
  #'(define NAME (create-axiom 'ESSENTIALS 'DISTINCTS 'CONCLUSION
                               floating-hypotheses* variables* constants*)))
         
(define (create-axiom essentials distincts conclusion all-floatings all-variables all-consts)
  (let* ((ees (evenths essentials))
         (hyps (append (get-floating-hypotheses (list ees conclusion) all-floatings all-variables) ees)))
    (check-d-vars distincts)
    (hash 'step
          (if (null? hyps)
              (λ (stack pds) (cons conclusion stack))
              (λ (stack pds)
                (let* ((pos (length hyps))
                       (bindings (bind-hypotheses hyps (take stack pos) all-variables all-consts)))
                  (check-ds distincts pds)
                  (cons (substitute-statement conclusion bindings) (drop stack pos))))))))

(module+ test
  (let* ((tze (create-axiom '() '() '(term 0) test-floats test-vars test-consts))
         (tze-step (hash-ref tze 'step)))
    (check-equal? (tze-step '() '()) '((term 0))))
  
  (let* ((tpl (create-axiom '() '() '(term |(| u + r |)|) test-floats test-vars test-consts))
         (tpl-step (hash-ref tpl 'step)))
    (check-equal? (tpl-step '((term u) (term 0)) '()) '((term |(| u + 0 |)|))))
  
  (let* ((weq (create-axiom '() '() '(wff u = r) test-floats test-vars test-consts))
         (weq-step (hash-ref weq 'step)))
    (check-equal? (weq-step '((term |(| u + 0 |)|) (term u)) '())
                  '((wff |(| u + 0 |)| = u))))

  (let* ((p (create-axiom '() '() '(TT |(| u = u |)|) test-floats test-vars test-consts))
         (p-step (hash-ref p 'step)))
    (check-equal? (p-step '((term u)) '()) '((TT |(| u = u |)|))))
  
;  (let* ((mp (create-axiom '(min (TT P) maj (TT (P -> Q))) '() '(TT Q)))
;         (mp-step (hash-ref mp 'step))
;         (tu '(term u))
;         (tze '(term 0))
;         (tpl '(term |(| u + r |)| ))
;         (weq '(wff |(| u = r |)|))
;         (stack (list tu tze tpl tu weq tu tu weq)))
;    (check-equal? ))
  )


(define-macro ($p NAME ESSENTIALS DISTINCTS CONCLUSION (PROOF ... ))
  #'(define NAME (add-provable 'NAME 'ESSENTIALS 'DISTINCTS 'CONCLUSION (list PROOF ...)
                              floating-hypotheses* variables* constants*)))
               
(define (add-provable name essentials distincts conclusion proof
                      all-floatings all-variables all-consts)
  (let ((provable (create-provable name essentials distincts conclusion proof
                               all-floatings all-variables all-consts)))
    (set! provables* (cons provable provables*))
    provable))

(define (create-provable name essentials distincts conclusion proof
                         all-floatings all-variables all-consts)
  (let ((provable (create-axiom essentials distincts conclusion
                         all-floatings all-variables all-consts)))
    (hash-set provable 'verify
              (λ (debug)
                (let ((stack '()))
                  (map
                   (λ (statement)
                     (set! stack ((hash-ref statement 'step) stack distincts))
                     (when debug (print stack)))
                   proof)
                  (when (not (equal? stack '(,conclusion)))
                    (error "MM verify: " name stack)))))))
  
(module+ test                

  (let* ((p (create-provable 'p '() '() '(TT |(| u = u |)|) '() test-floats test-vars test-consts))
         (p-step (hash-ref p 'step))
         (p-verify (hash-ref p 'verify)))
    (check-equal? (p-step '((term u)) '()) '((TT |(| u = u |)|))))

)

(define (verify-proof provable debug)
  ((hash-ref provable 'verify) debug))

(define (verify-all debug)
  (for-each
   (λ (p) ((hash-ref p 'verify) debug))
   provables*))

(define (check-ds distincts previous-distincts)
  '())

(define (check-d-vars ds)
  (for-each
    (λ (d) (for-each
            (λ (dv) (when (null? (member dv variables*))
                      (error (~a "MM $a symbol in ds is not var " dv))))
            d))
    ds))



(define th1 '())
(provide $c $v $f $a $p verify-all false true th1 constants* variables* floating-hypotheses*)
