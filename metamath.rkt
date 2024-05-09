#lang racket

(require (for-syntax racket/list))

(define constants* '())
(define variables* '())
(define formulas* '())
(define proofs* (make-hash))

(define-syntax ($c . new-constants)
  #`(set! constants* (append constants* ',new-constants)))


(define-syntax ($v . new-vars)
  #`(set! variables* (append variables* new-vars)))

(define (create-f ct var)
  (let ((res (make-hash)))
    (hash-set! res 'statement (list ct var))
    (hash-set! res 'step (λ (stack pds) (cons (list ct var) stack)))
    (set! formulas* (append formulas* (list (cons var res))))
    res))

(define-for-syntax (vars x variables)
  (remove-duplicates (λ (_) (member _ variables) (flatten x))))

(define (vars x)
  (remove-duplicates (λ (_) (member _ variables*) (flatten x))))

(define-for-syntax (get-formulas x formulas)
  (map (λ (_) ((cdr _) 'statement) (filter (λ (_) (memf (car _) (vars x))) formulas))))

(define-syntax (with-constants . body)
  #`(let* (,(append-map (λ (c) `((,c ',c))) constants*)) ,@body))

(define-syntax (check-constants)
  #`(begin
      ,@(map (λ (c) `(when (not (equal? ,c ',c)) (error "checks: mismatch " ,c ',c))))))

(define-for-syntax (push-assoc-list x al key)
  (let ((it (assoc key al)))
    (if it
        (set-mcdr! it (mcons (x (cdr it))))
        (set! al (cons (mcons key (mcons x '())) al)))))

(define-for-syntax (treewise f base tree)
  (if (pair? tree)
      (f (treewise f base (car tree))
         (treewise f base (cdr tree)))
      (base tree)))

(define-for-syntax (gtree tree)
  (let* ((al '())
         (base (λ (atom) (when atom
                           (push-assoc-list (gensym) al atom))))
         (restree (treewise cons base tree)))
    (list al restree)))

(define-syntax (mlet var val . body)
  (let* ((algvar (gtree var))
         (al (car algvar))
         (gvar (cadr algvar)))
    #`(match-let ,gvar ,val
        ,@(map (λ (x) `(when (not (equal? ,@(cdr x)))
                         (error ,(~a "mlet: different values for " (car x) ": ")
                                (list ,@(cdr x)))))
               al)
        (let* ,(map (λ (_) (list (car _) (cadr _))) al)
          ,@body))))

(define (mbind l)
  (cond ((null? l) '())
        ((pair? l) `(cons ,(mbind (car l)) ,(mbind (cdr l))))
        (else l)))

(define-for-syntax (check-d-vars ds variables)
  (for-each
   (λ (d) (for-each
           (λ (dv) (when (null? (member dv variables))
                     (error "MM $a symbol in ds is not var " dv)))
           d))             
   ds))

(define (tense-1n x l)
  (map (λ (_) (list x _) l)))

(define (tense-nn l1 l2)
  (append-map (λ (_) tense-1n _ l2) l1))

(define (all2sets l)
  (cond ((null? l) '())
        ((null? (cdr l)) '())
        (append (tense-1n (car l) (cdr l)) (all2sets (cdr l)))))

(define-syntax (check-ds ds pds)
  #`(begin
      ,@(map
         (λ (d)
           `(do
                ,@(map
                   (λ (d2)
                     (let* ((a (car d2))
                            (b (cadr d2)))
                       `(check-d2 ,a ,b ',a ',b ,pds)))
                   (all2sets d))))
         ds)))

(define (check-d2 a bb name-a name-b pds)
  (let* ((varsa (vars a))
         (varsb (vars bb))
         (it (memf (λ (_) (member _ varsa) varsb))))
    (when it (error "MM check-ds common var" it name-a name-b))
    (for-each (λ (vab)
                (let* ((va (car vab))
                       (vb (cadr vab)))
                  (unless (memf (λ (_) (member vb _)) (filter (λ (_) (member va _)) pds))
                    (error "MM check-ds no d for " va vb 'in name-a name-b))))
              (tense-nn varsa varsb))))

(define-for-syntax (evenths l)
  (cond ((null? l) '())
        ((null? (cdr l)) '())
        (else (cons (cadr l) (evenths (cddr l))))))

(define-syntax ($a name es ds ccl)
  (let* ((rest (gensym))
         (hyps (append (get-formulas (list (evenths es) ccl)) (evenths es))))
    (check-d-vars ds)
    #`(begin
        (let* ((,name (make-hash)))
          (hash-set! ,name 'step
                     (λ (stack pds)
                       ,(if hyps
                            `(with-constants
                                 (mlet (,@(reverse hyps) . ,rest) stack
                                       (check-cs)
                                       (check-ds ,ds pds)
                                       (cons ,(mbind ccl) ,rest)))
                            `(cons ',ccl stack))))))))

(define-syntax (with-essential-hypotheses es . body)
  (if (null? es)
      #`(begin ,@body)
      #`(let ((,(car es) (hash 'step (λ (stack pds)
                                     (cons ',(cadr es) stack)))))
          (with-essential-hypotheses ,(cddr es) ,@body))))
               

(define-syntax ($p name es ds ccl proof)
  #`(begin
      ($a ,name ,es ,ds ,ccl)
      (with-essential-hypotheses ,es
        (hash-set! ,name 'verify
                   (λ ()
                     (let ((stack '()))
                       ,@(map
                          (λ (s)
                            `(set! stack ((hash-ref ,s 'step) stack ',ds)))
                          proof)
                       (when (not (equal? stack '(,ccl)))
                         (error "MM verify: " ',name stack)))))
        (hash-set! ,name 'debug
                   (λ ()
                     (let ((stack '()))
                       ,@(map
                          (λ (s)
                            `(begin (displayln ',s)
                                    (set! stack  ((hash-ref ,s 'step) stack ',ds))
                                    (displayln stack)))
                          proof))))
        (hash-set! proofs* ',name ,name))))

(define (verify-all)
  (for-each (λ (n p) ((hash-ref p 'verify)))
            proofs*))


            
