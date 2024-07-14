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


(module+ test
  ($c + = -> term wff TT 0 |(| |)|)
  (check-equal? constants* '(+ = -> term wff TT 0 |(| |)|))
  )

(define-macro ($v . VARIABLES)
  #'(add-variables 'VARIABLES))


(module+ test
  ($v u r s P Q)
  (check-equal? variables* '(u r s P Q))
  )

(define (add-variables variable-symbols)
  (set! variables* (append variables* variable-symbols)))

(define-macro ($f NAME CONSTANT VARIABLE)
  #'(define NAME (add-floating-hypothesis 'CONSTANT 'VARIABLE)))

(define (add-floating-hypothesis constant variable)
  (let ((res (create-floating-hypothesis constant variable constants* variables*)))
    (set! floating-hypotheses* (cons (cons variable res) floating-hypotheses*))
    res))

(define (create-floating-hypothesis constant variable all-constants all-variables)
  (when (not (member constant all-constants))
    (error (~a constant " is not a constant.")))
  (when (not (member variable all-variables))
    (error (~a variable " is not a variable.")))
  (hash
   'statement (list constant variable)
   'step (λ (stack pds) (cons (list constant variable) stack))))

(module+ test
  (let ((hyp-term-u (create-floating-hypothesis 'term 'u)))
    (check-equal? (hash-ref hyp-term-u 'statement) '(term u))
    (check-equal? ((hash-ref hyp-term-u 'step) '() '()) '((term u))))

  ($f tu term u)
  (check-equal? (hash-ref tu 'statement) '(term u))
  (check-equal? floating-hypotheses* (list (cons 'u tu)))
  (check-equal? ((hash-ref tu 'step) '() '()) '((term u)))
  )
                
(define (get-present-variables essentials all-variables)
  (remove-duplicates (filter (λ (_) (member _ all-variables)) (flatten essentials))))

(module+ test
  (check-equal?
   (get-present-variables '((TT P) (TT (-> P Q))) '(u r s P Q))
   '(P Q)))

(define (vars->floatings vars all-floatings)
  (map (λ (var) (assoc var all-floatings)) vars))

; extract from all-floatings the floating hypotheses needed by essentials
(define (get-floating-hypotheses essentials all-floatings all-variables)
  (let ((vars  (get-present-variables essentials all-variables)))
    (map (λ (var-float) (hash-ref (cdr var-float) 'statement)) (vars->floatings vars all-floatings))))

(module+ test
  (define (create-floating-hypotheses cvs)
    (map (λ (cv)
           (let ((var (cadr cv)))
             (cons var (create-floating-hypothesis (car cv) var))))
         cvs))
  (define test-consts '(term wff + 0 = TT |(| |)|))
  (define test-vars '(u r s P Q))
  (define test-floats (create-floating-hypotheses '((term u) (term r) (term s)
                                                             (wff P) (wff Q))))
  (check-equal?
   (map (λ (var-float) (hash-ref (cdr var-float) 'statement))
        (vars->floatings '(P Q) test-floats))
   '((wff P) (wff Q)))
  (check-equal?
   (get-floating-hypotheses '((TT P) (TT  |(| P -> Q |)|)) test-floats test-vars)
   '((wff P) (wff Q)))
  )

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
              (error "illegal state " hypothesis-symbol hypothesis)))))))

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
    ;(check-d-vars distincts)
    (hash 'step
          (if (null? hyps)
              (λ (stack pds) (cons conclusion stack))
              (λ (stack pds)
                (let* ((pos (length hyps))
                       (bindings (bind-hypotheses hyps (reverse (take stack pos)) all-variables all-consts)))
                  ;(check-ds distincts pds)
                  (cons (substitute-statement conclusion bindings) (drop stack pos))))))))

(module+ test
  (let* ((tze (create-axiom '() '() '(term 0) test-floats test-vars test-consts))
         (tze-step (hash-ref tze 'step)))
    (check-equal? (tze-step '() '()) '((term 0))))

  (let* ((tpl (create-axiom '() '() '(term |(| u + r |)|) test-floats test-vars test-consts))
         (tpl-step (hash-ref tpl 'step)))
    (check-equal? (tpl-step '((term 0) (term u)) '()) '((term |(| u + 0 |)|))))

  (let* ((weq (create-axiom '() '() '(wff u = r) test-floats test-vars test-consts))
         (weq-step (hash-ref weq 'step)))
    (check-equal? (weq-step '((term |(| u + 0 |)|) (term u)) '())
                  '((wff u = |(| u + 0 |)|))))

  (let* ((th1 (create-axiom '() '() '(TT u = u ) test-floats test-vars test-consts))
         (th1-step (hash-ref th1 'step)))
    (check-equal? (th1-step '((term u)) '()) '((TT u = u))))
  
  (let* ((mp (create-axiom '(min (TT P) maj (TT |(| P -> Q |)|)) '() '(TT Q)
                           test-floats test-vars test-consts))
         (mp-step (hash-ref mp 'step)))
    (check-equal? (mp-step
                   '((TT |(| P -> Q |)| )
                     (TT P)
                     (wff Q)
                     (wff P))
                   '())
                  '((TT Q)))
    (check-equal? (mp-step
                   '((TT |(| |(| u + 0 |)| = u -> u = u |)|)
                     (TT |(| u + 0 |)| = u)
                     (wff u = u)
                     (wff |(| u + 0 |)| = u))
                   '())
                  '((TT u = u)))
    )

  ($a tze () () (term 0))
  (check-equal? ((hash-ref tze 'step) '() '()) '((term 0)))
  ($f tr term r)
  ($a tpl () () (term |(| u + r |)|))
  (check-equal?
   ((hash-ref tpl 'step)
    '((term 0) (term u))
    '())
   '((term |(| u + 0 |)|)))
  ($a weq () () (wff u = r))
  (check-equal?
   ((hash-ref weq 'step)
    '((term u) (term |(| u + 0 |)|))
    '())
   '((wff |(| u + 0 |)| = u)))
  ($f wp wff P)
  ($f wq wff Q)
  ($a wim () () (wff |(| P -> Q |)|))
  (check-equal?
   ((hash-ref wim 'step)
    '((wff u = |(| u + 0 |)|) (wff u = u))
    '())
   '((wff |(| u = u -> u = |(| u + 0 |)| |)| )))
  ($f ts term s)
  ($a a1 () () (TT |(| u = r -> |(| u = s -> r = s |)| |)| ))
  (check-equal?
   ((hash-ref a1 'step)
    '((term u) (term u) (term |(| 0 + u |)| ))
    '())
   '((TT |(| |(| 0 + u |)| = u -> |(| |(| 0 + u |)| = u -> u = u |)| |)|)))
  ($a a2 () () (TT |(| u + 0 |)| = u))
  (check-equal?
   ((hash-ref a2 'step)
    '((term u))
    '())
   '((TT |(| u + 0 |)| = u)))
  ($a mp (min (TT P) maj (TT |(| P -> Q |)|)) () (TT Q))
  (check-equal?
   ((hash-ref mp 'step)
    '((TT |(| |(| u + 0 |)| = u -> u = u |)|)
      (TT |(| u + 0 |)| = u)
      (wff u = u)
      (wff |(| u + 0 |)| = u))
    '())
   '((TT u = u)))

  )


(define-macro ($p NAME ESSENTIALS DISTINCTS CONCLUSION (PROOF ... ))
  #'(begin
      (define NAME (add-provable 'NAME 'ESSENTIALS 'DISTINCTS 'CONCLUSION (list PROOF ...)
                              floating-hypotheses* variables* constants*))
      (hash-ref NAME 'verify) true))
               
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
                     (when debug (displayln stack)))
                   proof)
                  (when (not (equal? stack (list conclusion)))
                    (error "MM verify: " name stack)))))))
  
(module+ test                
  (let* ((th1 (create-provable 'p '() '() '(TT u = u)
                             (list tu tze tpl tu weq tu tu weq tu a2 tu tze tpl
                                   tu weq tu tze tpl tu weq tu tu weq wim tu a2
                                   tu tze tpl tu tu a1 mp mp)
                             test-floats test-vars test-consts))
         (p-step (hash-ref th1 'step))
         (p-verify (hash-ref th1 'verify)))
    (check-equal? (p-step '((term u)) '()) '((TT u = u)))
    (p-verify false)
    )

  ($p th1 () () (TT u = u)
    (tu tze tpl tu weq tu tu weq tu a2 tu tze tpl
     tu weq tu tze tpl tu weq tu tu weq wim tu a2
     tu tze tpl tu tu a1 mp mp))

  ((hash-ref th1 'verify) true)
)

(define (verify-proof provable debug)
  ((hash-ref provable 'verify) debug))


(define (verify-all debug)
  (for-each
   (λ (p) ((hash-ref p 'verify) debug))
   provables*))

(module+ test
  (verify-all true))

(define th1 '())

(provide $c $v $f $a $p verify-all false true th1 constants* variables* floating-hypotheses*)
