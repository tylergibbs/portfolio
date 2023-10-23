#lang racket
(require redex)
(require redex/reduction-semantics)
(provide (all-defined-out))


(define-language equs
  (step ((equ ...) ((var var) ...) (equ ...)  (var ...) (equ ...) (equ ...)))
  ;gives value, var in relations, desc relation, val needed, value in, done
  (syst ((var var ...) (equ equ ...) (syst ...)));systms are independent and do not flow information fowerd
  (equ (exp exp))
  (exp add sub mul div number var)
  (add (+ exp exp))
  (sub (- exp exp))
  (mul (* exp exp))
  (div (/ exp exp))
  (var (string number number)); name_num dirnum
  )

(define-metafunction equs
  var=? : var var -> number
  ((var=? (string_name number_n number_d) (string_name number_n number_d))
   1)
  ((var=? var var)
   0)
  )

(define-metafunction equs
  replace : exp var exp -> exp
  ((replace (+ exp_1 exp_2) var_r exp_3)
   (+ (replace exp_1 var_r exp_3) (replace exp_2 var_r exp_3)))
  ((replace (- exp_1 exp_2) var_r exp_3)
   (- (replace exp_1 var_r exp_3) (replace exp_2 var_r exp_3)))
  ((replace (* exp_1 exp_2) var_r exp_3)
   (* (replace exp_1 var_r exp_3) (replace exp_2 var_r exp_3)))
  ((replace (/ exp_1 exp_2) var_r exp_3)
   (/ (replace exp_1 var_r exp_3) (replace exp_2 var_r exp_3)))
  ((replace number_n var_r exp_1)
   number_n)
  ((replace var_r var_r exp_1)
   exp_1)
  ((replace var_nr var_r exp_1)
   var_nr)
  )
;(term (replace  (+ 1 (/("v" 1 1) ("v" 2 1))) ("v" 1 1) 2))

(define-metafunction equs
  ret-unsolved : exp any -> any
  ((ret-unsolved (+ exp_1 exp_2)any_l)
   ,(let* ((left (term(ret-unsolved exp_1 any_l)))
           (right (term(ret-unsolved exp_2 ,left))))
      right))
  ((ret-unsolved (- exp_1 exp_2) any_l)
   ,(let* ((left (term(ret-unsolved exp_1 any_l)))
           (right (term(ret-unsolved exp_2 ,left))))
      right))
  ((ret-unsolved (* exp_1 exp_2) any_l)
   ,(let* ((left (term(ret-unsolved exp_1 any_l)))
           (right (term(ret-unsolved exp_2 ,left))))
      right))
  ((ret-unsolved (/ exp_1 exp_2) any_l)
   ,(let* ((left (term(ret-unsolved exp_1 any_l)))
           (right (term(ret-unsolved exp_2 ,left))))
      right))
  ((ret-unsolved number_n any_l)
   any_l)
  ((ret-unsolved (string_name number_id number_dir) any_l)
   ,(if (or (= 0 (term number_dir)) (member (term (string_name number_id number_dir)) (term any_l)))
        (term any_l)
        (cons (term (string_name number_id number_dir)) (term any_l))))
  )


(define-metafunction equs
  contain# : var exp -> number
 ((contain# var_r (+ exp_1 exp_2))
  ,(+ (term (contain# var_r exp_1)) (term (contain# var_r exp_2))))
  ((contain# var_r (- exp_1 exp_2))
  ,(+ (term (contain# var_r exp_1)) (term (contain# var_r exp_2))))
  ((contain# var_r (* exp_1 exp_2))
  ,(+ (term (contain# var_r exp_1)) (term (contain# var_r exp_2))))
  ((contain# var_r (/ exp_1 exp_2))
  ,(+ (term (contain# var_r exp_1)) (term (contain# var_r exp_2))))
  ((contain# var_r number_n)
   0)
  ((contain# var_r var_r)
   1)
  ((contain# var_r var_nr)
   0)
  )
;(term (contain# ("v" 1 1) 1))
;(term (contain# ("v" 1 1) ("v" 1 2)))
;(term (contain# ("v" 1 1) ("v" 1 1)))
;(term (contain# ("v" 1 1) (+ 1("v" 1 2))))
;(term (contain# ("v" 1 1) (+ 1 (/("v" 1 1) ("v" 1 1)))))

(define-metafunction equs
  contain? : var exp -> boolean
 ((contain? var_r exp_r)
  ,(<= 1 (term (contain# var_r exp_r))))
  )
;(term (contain? ("v" 1 1) 1))
;(term (contain? ("v" 1 1) ("v" 1 2)))
;(term (contain? ("v" 1 1) ("v" 1 1)))
;(term (contain? ("v" 1 1) (+ 1("v" 1 2))))
;(term (contain? ("v" 1 1) (+ 1 (/("v" 1 1) ("v" 2 1)))))
(define-metafunction equs
  equcont : equ var -> boolean
  ((equcont (exp_1 exp_2) var)
   ,(or (term (contain? var exp_1))
        (term (contain? var exp_2))
        )))
(define-metafunction equs
  equrep : equ equ -> equ
  ((equrep (exp_1 exp_2) (var_r exp_3))
   ((replace exp_1 var_r exp_3) (replace exp_2 var_r exp_3)))
  )

(define-metafunction equs
  mull-through : exp exp var -> exp
  ((mull-through (+ exp_1 exp_2) exp_r var_r)
   (+ (mull-through exp_1 exp_r var_r) (mull-through exp_2 exp_r var_r)))
  ((mull-through (- exp_1 exp_2) exp_r var_r)
   (- (mull-through exp_1 exp_r var_r) (mull-through exp_2 exp_r var_r)))
  ((mull-through (* exp_1 exp_2) exp_r var_r)
   ,(let ((in-l (term (contain? var_r exp_1)))
          (in-r (term (contain? var_r exp_2))))
      (cond
        ((and in-r in-l) (error 'mul-through "multible x's"))
        (in-l (term (mull-through exp_1 (* exp_2 exp_r) var_r)))
        (in-r (term (mull-through exp_2 (* exp_1 exp_r) var_r)))
        (else (term (* exp_r (* exp_1 exp_2))));avoid foil multiplication
        )))
  ((mull-through (/ exp_1 exp_2) exp_r var_r)
        ,(cond
           ((term (contain? var_r exp_2)) (error 'mul-through "cannot handle x's under /"))
           (else (term (mull-through exp_1 (/ exp_r exp_2) var_r)))))
  ((mull-through var_n exp_r var_r)
   (* exp_r var_n))
  ((mull-through number_n exp_r var_r)
   (* exp_r number_n))
  )

;(term (mull-through (* (+ 2 3) (+ ("v" 1 1) 3)) 1 ("v" 1 1)))
;(term (mull-through (* (+ 2 3) (+ ("v" 1 1) 3)) 1 ("v" 2 1)))
;(term (mull-through (* 2 (+ (+ (* (+ 1 2) ("v" 1 1))
 ;                                  (* (+ ("v" 1 1) 2) ("d" 2 1)))
  ;                            (/ (- ("v" 1 1) 1) (- 4 2)))) 1 ("v" 1 1)))
(define-metafunction equs
  factr1 : exp var -> exp;given containment
  ((factr1 exp_r var_r)
   ,(if (= 1 (term (contain# var_r exp_r))) (term (replace exp_r var_r 1))
        (error 'factr1 "either no x or more that 1 possibly given add x x")))
  )


