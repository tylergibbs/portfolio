#lang racket
(provide (all-defined-out))

(struct equ (l r)#:transparent)

;name is add mull div
(struct sxp (name l r) #:transparent)
(define (add l r)
  (sxp 'add l r))
(define (add? sexp)
  (if (sxp? sexp)
  (symbol=? (sxp-name sexp)'add)
  #f))
(define (sub l r)
  (sxp 'sub l r))
(define (sub? sexp)
  (if (sxp? sexp)
  (symbol=? (sxp-name sexp)'sub)
  #f))
(define (mull l r)
  (sxp 'mull l r))
(define (mull? sexp)
  (if (sxp? sexp)
  (symbol=? (sxp-name sexp)'mull)
  #f))
(define (div l r)
  (sxp 'div l r))
(define (div? sexp)
  (if (sxp? sexp)
  (symbol=? (sxp-name sexp)'div)
  #f))


(define (contain? tree sym)
  (cond
  ((number? tree) #f)
  ((symbol? tree) (symbol=? sym tree))
  (else (or (contain? (sxp-l tree)sym) (contain? (sxp-r tree)sym)))
  ))

(define (contain# tree sym)
  (cond
  ((number? tree) 0)
  ((symbol? tree) (if (symbol=? sym tree) 1 0))
  (else (+ (contain# (sxp-l tree)sym) (contain# (sxp-r tree)sym)))
  ))
  
(define (numeric? tree)
  (cond
  ((number? tree) #t)
  ((symbol? tree) #f)
  (else (and (numeric? (sxp-l tree)) (numeric? (sxp-r tree))))
  ))

(define (replace tree sub sexp)
  (cond
    ((number? tree) tree)
    ((symbol? tree)
     (if (symbol=? tree sub) sexp tree))
    (else(sxp (sxp-name tree)
              (replace (sxp-l tree) sub sexp)
              (replace (sxp-r tree) sub sexp)))
    ))
;(replace (add 1 (mull 'a 2)) 'a (div 1 2))

(define (mul-throu tree acc sym)
  (cond
    ((numeric? tree) (combac mull acc tree))
    ((symbol? tree) (combac mull acc tree))
    ((or (add? tree) (sub? tree)) (sxp (sxp-name tree)
                                       (mul-throu (sxp-l tree) acc sym)
                                       (mul-throu (sxp-r tree) acc sym)))
    ((mull? tree) (let* ((l (mul-throu (sxp-l tree) null sym))
                       (r (mul-throu (sxp-r tree) null sym))
                       (nl? (or (numeric? l) (mull? l) (div? l) (not (contain? l sym))));not need distr?
                       (nr? (or (numeric? r) (mull? r) (div? r) (not (contain? r sym))))
                       )
                   (cond
                     ((and nl? nr?) (combac mull tree acc));no distribution nessasary
                     ((and (not nl?) nr?)
                           (mul-throu (sxp-l tree) (combac mull acc r) sym))
                     
                     ((and (not nr?) nl?)
                      (mul-throu (sxp-r tree) (combac mull acc l) sym))
                     
                     ((and (not nl?) (not nr?))
                      (error 'mul-throu "foil multiplication"))
                     (else (error 'mul-throu "you broke logic")))))
    ((div? tree) (mul-throu (sxp-l tree) (combac div acc (sxp-r tree)) sym));this intentianaly ignors the posibility
                                       ;of interest in denominator becouse denominator will alwas be know value
    (else (error 'mul-throu "expected num sym add sub mull div"))
    ))
(define (combac f ac r)
  (cond
    ((null? ac) r)
    ((null? r) ac)
    (else (f ac r))))
;(mul-throu (mull 2 (add (add (mull (add 1 2) 'c) (mull (add 's 2) 'c)) (div (sub 'x 1) (sub 4 2)))) null)
;(mul-throu (mull (add 1 1) (add 'c 3)) null)

;gets all expr not/with designated var in it
(define (get-consts/non sexp sym)
  (cond
    ((not (or (add? sexp) (sub? sexp)))
     (if (contain? sexp sym) (values null sexp) (values sexp null)))
    (else (local
            ((define-values (lcon lnon) (get-consts/non (sxp-l sexp)sym))
             (define-values (rcon rnon) (get-consts/non (sxp-r sexp)sym))
             )
            (values (combac (λ (x y) (sxp (sxp-name sexp) x y)) lcon rcon)
                    (combac (λ (x y) (sxp (sxp-name sexp) x y)) lnon rnon))))
    ))
;(get-consts/non (add (add (mull 1 'p) (add 4 'x)) (sub 'x (div 1 (add 'p 'x)))) 'x)'x will never in real be in denominator

(define (factr sexp sym)
  (cond
    ((not (or (add? sexp) (sub? sexp)))
     (let ((fctd (factr-aux sexp sym)))
       (cond
         ((contain? fctd sym) (error 'factr "unfactorable(posibly divition)"))
         ((not (= (contain# fctd '|unique_symbol_not_to_be_used_by_usr/ˇÁÒ´‰|) 1))
          (error 'factr "unfactorable(multible xs in exprestion)(posibly usage of reserved symbol)"))
         (else (replace fctd '|unique_symbol_not_to_be_used_by_usr/ˇÁÒ´‰| 1)))))
    (else (combac (λ (x y) (sxp (sxp-name sexp) x y))
                  (factr (sxp-l sexp)sym) (factr (sxp-r sexp)sym)))
    ))
(define (factr-aux sexp sym)
  (cond
    ((numeric? sexp) sexp)
    ((symbol? sexp) (if (symbol=? sexp sym) '|unique_symbol_not_to_be_used_by_usr/ˇÁÒ´‰| sexp))
    ((mull? sexp) (combac mull (factr-aux (sxp-l sexp) sym) (factr-aux (sxp-r sexp) sym)))
    ((div? sexp) (combac div (factr-aux (sxp-l sexp) sym) (sxp-r sexp)))
    ))

;(factr (add (sub (mull 3 'x) (div 'x 2)) 'x)'x)
;(factr (add (sub (mull 3 'x) (div 2 'x)) 'x)'x)error
;(factr (add (sub (mull 3 'x) (div 'x 2)) (mull 'x 'x))'x)error

(define (solve eqa sym)
  (local (
          (define treel (equ-l eqa))
          (define treer (equ-r eqa))
          ;(define a (print 'a))
          (define lmsexp (mul-throu treel null sym))
          (define-values (lconst lfcprt) (get-consts/non lmsexp sym))
          (define lcret (if (null? lconst) 0 lconst))
          (define ldivpt (if (null? lfcprt) 0
                             (factr lfcprt sym)))
          ;(define b (print 'b))
          ;(define bb(print treer))
          (define rmsexp (mul-throu treer null sym))
          (define-values (rconst rfcprt) (get-consts/non rmsexp sym))
          (define rcret (if (null? rconst) 0 rconst))
          (define rdivpt (if (null? rfcprt) 0
            (factr rfcprt sym)))
          ;(define c (print 'c))
         )
    (div (sub lcret rcret ) (sub rdivpt ldivpt ))
    ))
(solve (equ (mull 3 (add (mull 2 'p) 'x)) (add 5 (sub 'x (mull 'x 3)))) 'x)
#|[3(2p+x)=5+x-3x
 3*2*p+3*x=5+x-3x
 3*2*p  3*x = 5 x-3x
 3*2*p x(3*1) = 5 x(1-3*1) solution ^^
 x(3*1)-x(1-3*1)=5-3*2*p
 x((3*1)-(1-3*1))=5-3*2*p
 x=5-3*2*p/((3*1)-(1-3*1))]|#
(define (solve-eq eqa sym)
  (equ sym (solve eqa sym)))
(define (equ-subs eq1 eq2)
  (if (symbol? (equ-l eq2))
  (equ (replace (equ-l eq1) (equ-l eq2) (equ-r eq2))
       (replace (equ-r eq1) (equ-l eq2) (equ-r eq2))
       )
  (error 'equ-subs "second arg not solved equation")))

(define (non-equ? x)
  (equal? (equ-l x) (equ-r x)))

(define (equ-contain? eq sym)
  (or (contain? (equ-l eq) sym)
      (contain? (equ-r eq) sym)
      ))

  ;(solve (equ 'V (sxp 'mull 'r_1 'f_1)) 'f_1)