#lang racket
(require "sym-math.rkt")
(struct junct (aloac alojc sames adds))
(struct arrow (a b v))
;eqch arrow will be combind of outhers of same type based on allin ment when being transformed
(define (n+sym sym n)
  (string->symbol (append (symbol->string sym) "_" (number->string n))))

(define (numberafy sym add sam n n-i)
  (cond
    ((equal? sym add) (n+sym sym n-i))
    ((equal? sym same) (n+sym sym n))
    (else (n+sym sym n-i))
    ))

(define (arw->eq arw sym add same n n-i)
  (solve 
  (equ (numberafy (arrow-a arw) add same n n-i)
       (mull (arrow-v arw) (numberafy (arrow-b arw) add same n n-i)))
  (numberafy sym add same n n-i)
  ))



;junct->eqs
(define (junct->eqs jnct n ff fe)
  (cond
    ((and ff fe) (error 'junct->eqs "weird bypass section"))
    (ff (junct->eqs/f jnct n ff))
    (fe (junct->eqs/e jnct n fe))
    (else (junct->eqs/none jnct n))
    ))

(define (junct->eqs/f jnct n ff)
  (cond
    ((equal? (eqa-l ff) (n+sym (junct-sames jnct) n)) (junct->eqs/f/same jnct n ff))
    ((equal? (eqa-l ff) (n+sym (junct-adds jnct) n)) (junct->eqs/f/add jnct n ff))
    (else (error 'junct->eqs/f "symbols not match"))
    ))

(define (junct->eqs/e jnct n fe)
  (cond
    ((equal? (eqa-l fe) (n+sym (junct-sames jnct) n)) (junct->eqs/e/same jnct n ff))
    ((equal? (eqa-l fe) (n+sym (junct-adds jnct) n)) (junct->eqs/e/add jnct n ff))
    (else (error 'junct->eqs/e "symbols not match"))
    ))

(define (junct->eqs/f/same jnct n ff)
  (let*((sames (junct-sames jnct))
        (adds (junct-adds jnct))
        (n-i n)
        (rteqs (map (λ (x) (begin
                             (set! n-i (add1 n-i))
                             (arw->eq x adds adds same n n-i)))
                    (junct-aloac jnct)))
        (nceqs (map (λ (x) (begin
                                   (set! n-i (add1 n-i))
                                   (junct->eqs x n-i ff #f)))
                      (junct-aloac jnct)))
        (same-eq ff)
        (add-eqs (get-adds-unknown adds n n-i (append rteqs nceqs))))
    (cons same-eq add-eqs)))

(define (junct->eqs/f/adds jnct n fe);e is same
  (let*((sames (junct-sames jnct))
        (adds (junct-adds jnct))
        (n-i n)
        (rteqs (map (λ (x) (begin
                             (set! n-i (add1 n-i))
                             (arw->eq x adds adds same n n-i)))
                    (junct-aloac jnct)))
        (same-eq (get-sames same rteqs))
        (nceqs (map (λ (x) (begin
                                   (set! n-i (add1 n-i))
                                   (junct->eqs x n-i #f same-eq)))
                    (junct-aloac jnct)))
        (add-eqs (get-adds-known fe adds n n-i (append rteqs



                                                       
#|
  (let*((sames (junct-sames jnct))
        (adds (junct-adds jnct))
        (n-i n)
        (rteqs (map (λ (x) (begin
                             (set! n-i (add1 n-i))
                             (arw->eq x sames adds n)))
                    (junct-aloac jnct)))
        
        (smval (get-smval sames fweq rteqs))
        )
    (if smval
        (let*((sames-eqs (make-sames-eqs sames smval n n-i))
              (nceqs (map (λ (x) (begin
                                   (set! n-i (add1 n-i))
                                   (junct->eqs x n-i)))
                          (junct-alojc jnct)))
              (a|#