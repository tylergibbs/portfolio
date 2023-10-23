#lang racket
(require "sym-math.rkt")
(struct junct (R C P alojc sames adds))
(struct arrow (a b v))

(define (sym+n sym n)
  (if (symbol? sym)
  (string->symbol (string-append (symbol->string sym) "_" (number->string n)))
  sym))

(define (arrow->eq arw n)
  (equ (sym+n (arrow-a arw) n) (mull (sym+n (arrow-v arw) n) (sym+n (arrow-b arw) n))))

(define (junct->eq jnct fe)
  (cond
    ((equal? (junct-sames jnct) (equ-l fe)) (junct->eq/sames/e jnct fe 0))
    ;((equal? (junct-adds jnct) (equ-l fe)) (junct->eq/adds/e jnct fe 0))
    (else (error 'junct->eq "fe-l not match junct"))
    ))
(define (get-from-arrows RCP known-eqs n123 search-name)
  (cond
    ((null? RCP) null)
    ((not (first RCP)) (get-f-from-arrows  (rest RCP) known-eqs n123 f))
    (else (let ((eq (arrow->eq (first RCP) (first n123)))
                (fn (sym+n search-name (first n123))))
            (cons
             (cond
              ((not (equ-contain? eq fn)) (equ fn fn))
              (else (solve-eq (equ-subs eq (first known-eqs))fn ))
              )
             (get-f-from-arrows  (rest RCP) (rest eeqs) (rest n123) search-name))
             ))))
(define (get-all f alojc fv n)
  (cond
    ((null? alojc) null)
    (else (let*((eqs (f (first alojc) fv n))
                (l (length eqs))
                (nn(list-ref eqs (- l 1)))
                (teqs (take eqs (- l 1))))
            (cons teqs (get-all f (rest alojc) fv nn))
            ))))

(define (junct->eq/sames/e jnct fe n);parellel
  (let*((sames (junct-sames jnct))
        (adds (junct-adds jnct))
        (R (junct-R jnct))
        (C (junct-C jnct));must be false
        (C-test (if C (error 'junct->eq/same/e "Capasitor unsolvable in position for f")
                    (void)))
        (P (junct-P jnct))
        (alojc (junct-alojc jnct))
        (l-RCP (length (filter (λ (x) (not (false? x))) (list R C P))))
        
        (num-vals (build-list (+ l-RCP (length alojc))
                              (λ (x) (+ 1 n x))));not include 0th
        (aloeeq (map (λ (x) (equ-subs fe (equ sames (sym+n sames x)))) num-vals))
        (f-from-arrows (get-from-arrows (list R C P) (take aloeeq l-RCP) (take num-vals l-RCP) adds))
        (next-junct-eqs (get-all junct->eq/adds/e alojc fe (+ n l-RCP)))
        (f-from-junct (map (λ (x) (cdr (car x))) next-junct-eqs))
        (alofeq (append f-from-arrows f-from-junct))
        (et (equ (sym+n sames n) (equ-r (first aloeeq))))
        (ft (equ (sym+n adds n) (foldr (λ (x y) (add (equ-r x) y)) 0 alofeq)))
        )
    (cons (cons et ft) (append (map (λ(x y) (cons x y)) aloeeq alofeq) next-junct-eqs(list (+ 1 l-RCP (length alojc)))))
    ))

;(junct->eq (junct (arrow 'e 'f 'r) #f #f null 'e 'f) (equ 'e 'V))
;(junct->eq (junct (arrow 'e 'f 8) #f #f null 'e 'f) (equ 'e 'V))
;(junct->eq (junct  #f #f (arrow 'f 'p 'v) null 'e 'f) (equ 'e 'V))
;;(junct->eq (junct #f (arrow 'e 'c 'q) #f null 'e 'f) (equ 'e 'V))error

(define (junct->eq/adds/e jnct fe n);parellel
  (let*((sames (junct-sames jnct))
        (adds (junct-adds jnct))
        (R (junct-R jnct))
        (C (junct-C jnct))
        (P (junct-P jnct))
        (alojc (junct-alojc jnct))
        (l-RCP (length (filter (λ (x) (not (false? x))) (list R C P))))
        ;(a (print (filter (λ (x) (not (false? x))) (list R C P))))
        (num-vals (build-list (+ l-RCP (length alojc))
                              (λ (x) (+ 1 n x))));not include 0th
        
        )
    (cond
      ((and P (or R C))
       (let*((f (solve (arrow->eq P l-RCP) (sym+n sames l-RCP)))
             (alofeq (map (λ (x) (equ (sym+n sames x) f)) num-vals))
             (e-from-arrows (get-from-arrows (list R C P) (take alofeq l-RCP) (take num-vals l-RCP) adds))
             (next-junct-eqs null);continue
             (e-from-junct (map (λ (x) (cdr (car x))) next-junct-eqs))
             (aloeeq (append e-from-arrows e-from-junct))
             (et fe)
             (ft (equ (sym+n sames n) (equ-r (first alofeq))))
             )
         (cons (cons et ft) (append (map (λ(x y) (cons x y)) aloeeq alofeq) next-junct-eqs))))
      (R (let*;only R and C
                     (e-from-jncts (get-all (get-e-from-jnct x y
                     
                     ))
      (else
       
      