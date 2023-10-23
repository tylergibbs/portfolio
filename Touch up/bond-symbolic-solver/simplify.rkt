#lang racket
(require "equations.rkt")
(require redex)
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-metafunction equs
  numeric? : exp -> boolean
  ((numeric? (+ exp_1 exp_2))
   ,(and (term (numeric? exp_1)) (term (numeric? exp_2))))
  ((numeric? (- exp_1 exp_2))
   ,(and (term (numeric? exp_1)) (term (numeric? exp_2))))
  ((numeric? (* exp_1 exp_2))
   ,(and (term (numeric? exp_1)) (term (numeric? exp_2))))
  ((numeric? (/ exp_1 exp_2))
   ,(and (term (numeric? exp_1)) (term (numeric? exp_2))))
  ((numeric? number_n)
   #t)
  ((numeric? var_r)
   #f)
  )
;(term (numeric? (+ 1 (- 1 ("V" 1 1)))))
;(term (numeric? (+ 1 (- 1 2))))

(define-metafunction equs
  do-oper : exp -> number
  ((do-oper (+ exp_1 exp_2))
   ,(+ (term (do-oper exp_1)) (term (do-oper exp_2))))
  ((do-oper (- exp_1 exp_2))
   ,(- (term (do-oper exp_1)) (term (do-oper exp_2))))
  ((do-oper (* exp_1 exp_2))
   ,(* (term (do-oper exp_1)) (term (do-oper exp_2))))
  ((do-oper (/ exp_1 exp_2))
   ,(/ (term (do-oper exp_1)) (term (do-oper exp_2))))
  ((do-oper number_n)
   number_n)
  ((do-oper var_r)
   ,(error 'do-opper "expected numeric?"))
  )
;(term (do-oper (+ 1 (* 2 (/ 8 (- 4 2))))))

(define-metafunction equs
  neg? : exp -> any;exp #f
  ((neg? (- 0 exp_2))
   exp_2)
  ((neg? (* -1 exp_2))
   exp_2)
  ((neg? (* exp_1 -1))
   exp_1)
  ((neg? exp)
   #f)
  )
   

(define-metafunction equs
  simp-aux : exp -> exp
  ((simp-aux (+ exp_1 0))
   (simp exp_1))
  ((simp-aux (+ 0 exp_2))
   (simp exp_2))
  ((simp-aux (- exp_1 0))
   (simp exp_1))
  ((simp-aux (* exp_1 1))
   (simp exp_1))
  ((simp-aux (* 1 exp_2))
   (simp exp_2))
  ((simp-aux (* exp_1 0))
   0)
  ((simp-aux (* 0 exp_2))
   0)
  ((simp-aux (/ exp_1 1))
   (simp exp_1))
  ((simp-aux (- 0 exp_2))
   ,(let ((res (term (neg? exp_2))))
      (if res res (term (* -1 (simp exp_2))))))
  ((simp-aux (* -1 exp_2))
   ,(let ((res (term (neg? exp_2))))
      (if res res (term (* -1 (simp exp_2))))))
  ((simp-aux (* exp_1 -1))
   ,(let ((res (term (neg? exp_1))))
      (if res res (term (* -1 (simp exp_1))))))
  ((simp-aux (+ exp_1 exp_2))
   (+ (simp exp_1) (simp exp_2)))
  ((simp-aux (- exp_1 exp_2))
   (- (simp exp_1) (simp exp_2)))
  ((simp-aux (* exp_1 exp_2))
   (* (simp exp_1) (simp exp_2)))
  ((simp-aux (/ exp_1 exp_2))
   (/ (simp exp_1) (simp exp_2)))
  ((simp-aux number_n)
   number_n)
  ((simp-aux var_r)
   var_r)
  )


(define-metafunction equs
  simp : exp -> exp
  ((simp (+ exp_1 exp_2))
   ,(let ((exp1num? (term (numeric? exp_1)))
          (exp2num? (term (numeric? exp_2))))
      (cond
        ((and exp1num? exp2num?) (term (do-oper (+ exp_1 exp_2))))
        (exp1num? (term (simp-aux (+ (do-oper exp_1) exp_2))))
        (exp2num? (term (simp-aux (+ exp_1 (do-oper exp_2)))))
        (else (term (+ (simp-aux exp_1) (simp-aux exp_2)))))))
        
  ((simp (- exp_1 exp_2))
   ,(let ((exp1num? (term (numeric? exp_1)))
          (exp2num? (term (numeric? exp_2))))
      (cond
        ((and exp1num? exp2num?) (term (do-oper (- exp_1 exp_2))))
        (exp1num? (term (simp-aux (- (do-oper exp_1) exp_2))))
        (exp2num? (term (simp-aux (- exp_1 (do-oper exp_2)))))
        (else (term (- (simp-aux exp_1) (simp-aux exp_2)))))))
  ((simp (* exp_1 exp_2))
   ,(let ((exp1num? (term (numeric? exp_1)))
          (exp2num? (term (numeric? exp_2))))
      (cond
        ((and exp1num? exp2num?) (term (do-oper (* exp_1 exp_2))))
        (exp1num? (term (simp-aux (* (do-oper exp_1) exp_2))))
        (exp2num? (term (simp-aux (* exp_1 (do-oper exp_2)))))
        (else (term (* (simp-aux exp_1) (simp-aux exp_2)))))))
  ((simp (/ exp_1 exp_2))
   ,(let ((exp1num? (term (numeric? exp_1)))
          (exp2num? (term (numeric? exp_2))))
      (cond
        ((and exp1num? exp2num?) (term (do-oper (/ exp_1 exp_2))))
        (exp1num? (term (simp-aux (/ (do-oper exp_1) exp_2))))
        (exp2num? (term (simp-aux (/ exp_1 (do-oper exp_2)))))
        (else (term (/ (simp-aux exp_1) (simp-aux exp_2)))))))
  ((simp number_n)
   number_n)
  ((simp var_r)
   var_r)
  )

;(term (simp
;    (/
;     (-
;      (* 1 0)
;      (+
;       (- (* (/ 1 (- 0 (* 1 1))) 0) (* (/ 1 (- 0 (* 1 1))) (* 1 (* 1 ("F" 6 0)))))
;       (+
;        0
;        (+
;         (- (* (/ 1 (- 0 (* 1 1))) 0) (* (/ 1 (- 0 (* 1 1))) (* 1 (* 3 (* 1 ("V" 3 0))))))
;         (+ (- (* (/ 1 (- 0 (* 1 1))) 0) (* (/ 1 (- 0 (* 1 1))) (* 1 2))) (* 1 0))))))
;     (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0))))

(define-metafunction equs
  simp-equ : equ -> equ
  ((simp-equ (exp_1 exp_2))
  ((simp exp_1) (simp exp_2)))
  )
;(term (simp-equ
;        (("F" 6 1)
;    (/
;     (-
;      (*
;       1
;       (/
;        (-
;         (* 1 ("V" 3 1))
;         (-
;          (* (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0)) (* 1 0))
;          (+
;           (-
;            (* (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0)) (* (/ 1 (- 0 (* 1 1))) 0))
;            (*
;             (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0))
;             (* (/ 1 (- 0 (* 1 1))) (* 1 (* 1 ("F" 6 0))))))
;           (+
;            (* (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0)) 0)
;            (+
;             (-
;              (* (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0)) (* (/ 1 (- 0 (* 1 1))) 0))
;              (*
;               (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0))
;               (* (/ 1 (- 0 (* 1 1))) (* 1 (* 3 (* 1 ("V" 3 0)))))))
;             (+
;              (-
;               (* (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0)) (* (/ 1 (- 0 (* 1 1))) 0))
;               (*
;                (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0))
;                (* (/ 1 (- 0 (* 1 1))) (* 1 2))))
;              (* (/ 1 (- (+ (- 0 0) (+ (* 1 1) (+ (- 0 0) (+ (- 0 0) 0)))) 0)) (* 1 0))))))))
;        (- (- 0 (+ (- 0 0) (+ 0 (+ (- 0 0) (+ (- 0 0) 0))))) 0)))
;      (+
;       0
;       (+
;        (- (* (/ 1 (- (* (* 2 1) 1) 0)) (* 1 (* 1 ("F" 6 0)))) (* (/ 1 (- (* (* 2 1) 1) 0)) 0))
;        (* 1 0))))
;     (- (+ (* 1 1) (+ (- 0 0) 0)) 0)))
;        ))
;  