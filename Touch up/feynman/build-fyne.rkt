#lang plai
;Author: Tyler Gibbs
;2014
;builds all posible fyneman diagrams for a given input and outputs
;which can later be used to simulate physical interactions

;a node it either an external(ex) or internal(in) node
(define-type node   ;0=e- 1=e+ 2=ph
  (ex (c number?) (tp symbol?))
  (in (e- number?) (e+ number?) (ph number?))
  )

(define (make-tempate alex n)
   (vector-append (vector-map (λ (x) (ex -1 x)) alex)
   (build-vector n (λ (x) (in -1 -1 -1))))
  )

;takes the conjegate of the symbol passed to it
;posotron becomes an electon and vis versa
;a photon is its own conjagate
(define (conj sym)
  (cond
    ((symbol=? sym 'e+) 'e-)
    ((symbol=? sym 'e-) 'e+)
    ((symbol=? sym 'ph) 'ph)
    ))

(define (conect? temp n1 n2)
  (let ((tn1 (vector-ref temp n1))
        (tn2 (vector-ref temp n2)))    
  (or
    (overlap? tn1 tn2 'e- n1 n2)
    (overlap? tn1 tn2 'e+ n1 n2)
    (overlap? tn1 tn2 'ph n1 n2)
    )))

(define (overlap? node1 node2 nt n1 n2)
  (and (free? node1 nt n1 n2) (free? node2 (conj nt) n1 n2)))
(define (pos-conect? nod)
  (or
   (free? nod 'e- +inf.0 -nan.0)
   (free? nod 'e+ +inf.0 -nan.0)
   (free? nod 'ph +inf.0 -nan.0)
   ))
(define (free? nod nt n1 n2)
  (type-case node nod
    (ex (c tp) (and (equal? tp nt) (not (= n1 n2)) (= c -1)))
    (in (e- e+ ph) (cond
                     ((equal? (conj nt) 'e-)
                       (= e- -1))
                     ((equal? (conj nt) 'e+)
                       (= e+ -1))
                     ((equal? (conj nt) 'ph)
                       (and 
                        (= ph -1)
                        (not (= n1 n2))))
                     ))))

(define (conect temp n1 n2)
  (cond
    ((overlap? (vector-ref temp n1) (vector-ref temp n2) 'e- n1 n2)
      (attach temp n1 n2 'e-))
     ((overlap? (vector-ref temp n1) (vector-ref temp n2) 'e+ n1 n2)
      (attach temp n1 n2 'e+))
     ((overlap? (vector-ref temp n1) (vector-ref temp n2) 'ph n1 n2)
      (attach temp n1 n2 'ph))
     (else (error 'no-such-conection))))

(define (attach temp n1 n2 nt)
  (conec (conec temp n1 n2 (conj nt)) n2 n1 nt))

(define (conec temp-rel reff reft nt)
  (let ((temp (vector-map (λ (x)x) temp-rel)))
   (type-case node (vector-ref temp reff)
    (ex (c tp) 
        (begin
          (vector-set! temp reff (ex reft tp))
          temp
         ))
    (in (e- e+ ph) 
          (begin
            (vector-set! temp reff (if (equal? 'e- nt)
                                       (in reft e+ ph)
                                       (if (equal? 'e+ nt)
                                           (in e- reft ph)
                                           (if (equal? 'ph nt)
                                               (in e- e+ reft)
                                               (error 'death)
                                               ))))
            temp))
     )))

(define (act-same? temp s e at)
 (or
    (and
     (overlap? (vector-ref temp s) (vector-ref temp e) 'e- s e)
     (overlap? (vector-ref temp at) (vector-ref temp e) 'e- at e)
     )
    (and
     (overlap? (vector-ref temp s) (vector-ref temp e) 'e+ s e)
     (overlap? (vector-ref temp at) (vector-ref temp e) 'e+ at e)
     )
    (and
     (overlap? (vector-ref temp s) (vector-ref temp e) 'ph s e)
     (overlap? (vector-ref temp at) (vector-ref temp e) 'ph at e)
     )
    ))
     
    
    

(define (conect-broke? temp s v e at)
  (and (= v e)
   (act-same? temp s e at)
   )
  )

(define (self? temp n x y)
  (and (not (= n y))))
  
      
      

;this prevents multible conections to same node must use seperate test
;excucive?
(define (conect-broke-all? temp n v aux f1 f2)
  (foldl (λ (x y) (and (self? temp n (f1 x) (f2 x)) (conect-broke? temp n v (f1 x) (f2 x)) y))
         #t aux))
         


(define (get-ex-con temp l n x y aux)
  (cond
    ((= n l) aux)
    ((conect-broke-all? temp n y aux car cdr) (get-ex-con temp l n x y
                                                   (cons (cons x n) aux)))
    ((conect-broke-all? temp n y aux cdr car) (get-ex-con temp l n x y
                                                    (cons (cons n y) aux)))
    (else (get-ex-con temp l (add1 n) x y aux))
    ))

(define (get-conect-aux temp x l tn)
  (cond
    ((= x l) null)
    ((conect? temp x tn) (get-ex-con temp l 0 x tn (cons (cons x tn) null)))
    (else (get-conect-aux temp (add1 x) l tn))
    ))

(define (get-conect temp x l)
  (cond
    ((= x l) null)
    ((pos-conect? (vector-ref temp x)) (get-conect-aux temp 0 l x))
    (else (get-conect temp (add1 x) l))))

(define (conect-cn cn temp)
  (conect temp (car cn) (cdr cn)))

(define (conect-list alocn temp)
  (map 
   (λ (x) (conect-full (conect-cn x temp))) 
       alocn))

(define (done? temp n l)
  (cond
    ((= n l) #t)
    (else (and (not(pos-conect? (vector-ref temp n))) (done? temp (add1 n) l)))
    ))

(define (conect-full temp)
  (let ((alocn (get-conect temp 0 (vector-length temp) )))
    (if (done? temp 0 (vector-length temp)) (list temp)
    (apply append (conect-list alocn temp))
    )))

;testing/examples
(define templ (make-tempate (vector 'e- 'e+ 'ph) 1))
(define templ2 (make-tempate (vector 'e- 'e+ 'ph) 2))
(define templ3 (make-tempate (vector 'e- 'e+ 'ph) 3))
(define templ4 (make-tempate (vector) 2))
(define templ5 (make-tempate (vector 'e- 'e- 'e+ 'e+) 2))
(conect-full templ)
(define (tst x y)
  (set-subtract (list->set (append x y)) (set-intersect (list->set x) (list->set y))))
(conect-full templ2)
(conect-full templ3)
(length (conect-full templ3))
(length (remove-duplicates (conect-full templ3)))
#|
(conect-full templ5)
(get-conect templ 0 4)
(get-conect templ3 0 (vector-length templ3))
;(conect-list-n 0 '(0 1 2 3) templ) 
;(conect-list-n 0 '(0 2 3) templ) 
;(conect-list '(0 1 2 3) templ)
;(complete-conect templ)
;(complete-conect templ2)
(void(set-map (tst (conect-full templ3) t3a) (λ (x) (begin (print x) (newline)))))|#