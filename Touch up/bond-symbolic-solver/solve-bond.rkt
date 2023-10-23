#lang racket
(require "simplify.rkt")
(require "equations.rkt")
(require redex)
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-metafunction equs
  const/nonfd : exp var -> any
  ((const/nonfd (+ exp_1 exp_2) var_r)
   ,(let ((aux-l (term (const/nonfd exp_1 var_r)))
          (aux-r (term (const/nonfd exp_2 var_r))))
      (cons (term (+ ,(car aux-l) ,(car aux-r))) (term (+ ,(cdr aux-l) ,(cdr aux-r))))
      ))
  ((const/nonfd (- exp_1 exp_2) var_r)
   ,(let ((aux-l (term (const/nonfd exp_1 var_r)))
          (aux-r (term (const/nonfd exp_2 var_r))))
      (cons (term (- ,(car aux-l) ,(car aux-r))) (term (- ,(cdr aux-l) ,(cdr aux-r))))
      ))
  ((const/nonfd mul_v var_r)
   ,(cond
      ((term (contain? var_r mul_v)) (cons 0 (term (factr1 mul_v var_r))))
      (else (cons (term mul_v) 0))
      ))
  ((const/nonfd div_v var_r)
   ,(cond
      ((term(contain? var_r div_v)) (cons 0 (term (factr1 div_v var_r))))
      (else (cons (term mull_v) 0))
      ))
  ((const/nonfd var_r var_r)
   ,(cons 0 1))
  ((const/nonfd var_v var_r)
   ,(cons (term var_v) 0))
  ((const/nonfd number_n var_r)
   ,(cons (term number_n) 0))
  )
;(term (const/nonfd (mull-through (* (+ 2 3) (+ ("v" 1 1) 3)) 1 ("v" 1 1))("v" 1 1)))
;(term (const/nonfd (mull-through (* 2 (+ (+ (* (+ 1 2) ("v" 1 1))
 ;                                  (* (+ ("v" 1 1) 2) ("d" 2 1)))
  ;                           (/ (- ("v" 1 1) 1) (- 4 2)))) 1 ("v" 1 1))
   ;             ("v" 1 1)))

(define-metafunction equs
  solve : equ var -> equ
  ((solve (exp_l exp_r) var_v)
   ,(let* ((mlcn (term (const/nonfd(mull-through exp_l 1 var_v)var_v)))
           (mrcn (term (const/nonfd(mull-through exp_r 1 var_v)var_v))))
      (term (var_v (/ (- ,(car mlcn) ,(car mrcn)) (- ,(cdr mrcn) ,(cdr mlcn)))))
      )))
;(term(solve ((* 3 (+ (* 2 ("p" 1 1)) ("x" 1 1))) (+ 5 (- ("x" 1 1) (* ("x" 1 1) 3)))) ("x" 1 1)))




(define-metafunction equs
  solve-for : var (equ ...) -> any
  ((solve-for var_s (equ_c1 equ_cs ...))
   ,(let ((c1_vars (term (get-unsolved equ_c1 ()))))
     (if (and (=(length c1_vars)1) (term (equ=? var_s ,(first c1_vars))))
         (term (solve equ_c1 var_s))
         (term (solve-for var_s (equ_cs ...))))))
  ((solve-for var_s ())
   #f)
  )


(define-metafunction equs
  done? : equ -> boolean
  ((done? equ_n)
   ,(>= 1 (length (term (get-unsolved equ_n ())))))
  )

(define-metafunction equs
  part-solve-aux : equ equ (var ...) equ ... -> any
  ;((part-solve-aux equ_same (("null" -1 -nan.0) exp_v) (var_r ...) equ_e ...)
   ;(part-solve-aux equ_same (0 exp_v) (var_r ...) equ_e ...))

  ((part-solve-aux equ_same equ_add (var_same var_1 var_r ...) (+nan.0 -nan.0) equ_e ...)
   (part-solve-aux equ_same equ_add (var_same var_r ...) equ_e ...))
  
  ((part-solve-aux equ_same equ_add (var_same var_1 var_r ...) equ_1 equ_e ...)
   ,(if (term (equcont equ_1 var_1))
       (let*((eq (term (equrep (solve equ_1 var_1) equ_same)))
             ;(d (begin (print (term equ_1)) (newline) (print (term var_1)) (newline)
              ;         (print (term equ_same)) (newline) (print eq)))
             (ad(term(part-solve-aux equ_same (equrep equ_add ,eq) (var_same var_r ...) equ_e ...))))
         (if (term (done? ,eq))
         (term (,(first ad) ,(second ad) ,(cons eq (third ad))))
         (term (,(first ad) ,(cons eq (second ad)) ,(third ad)))
         ))
       (term (part-solve-aux equ_same equ_add (var_same var_r ...) equ_e ...))))

  ((part-solve-aux equ_same equ_add (var_same))
   (equ_add () ()))
  )

(define-metafunction equs
  get-unsolved : equ any -> any
  ((get-unsolved (exp_1 exp_2) any_l)
   ,(remove* (term any_l) (append (term(ret-unsolved exp_1 any_l)) (term(ret-unsolved exp_2 any_l)))))
  )
;(step ((equ ...) ((var var) ...) (equ ...)  (var ...) (equ ...) (equ ...)))
;gives value, var in relations, desc relation, val needed, value in, done
(define-metafunction equs
  part-solve+eq : (var ...) equ (equ equ ...) -> step
  ((part-solve+eq (var_r ...) (("null" -1 -nan.0) exp_e) (equ_same equ_comps ...));values in equ_sum in same order as equ_comps
   ,(let* ((aux (term(part-solve-aux equ_same (0 exp_e) (var_r ...)equ_comps ...)))
          ; (d (print aux))
          (aux-gets (term(get-unsolved ,(first aux) ()))))
      (if (= 1 (length aux-gets))
          (let ((solved  (term (solve ,(first aux) ,(first aux-gets)))))
            (term (()
                   ()
                   ()
                   ()
                   ()
                   
          ,(map (λ (x) (term (simp-equ ,x)))
                (append (cons solved
                         (cons (term equ_same)
                               (map (λ (x) (term (equrep ,x ,solved)))
                                    (second aux))))
                   (third aux))))))
          (error 'part-solve+eq "unsolveable"))
      ))
  
  ((part-solve+eq (var_r ...) (var_in exp_e) (equ_same equ_comps ...));values in equ_sum in same order as equ_comps
   ,(let* ((aux (term(part-solve-aux equ_same (var_in exp_e) (var_r ...)equ_comps ...)))
          ; (d (print aux))
          (aux-gets (term(get-unsolved ,(first aux) (var_in)))))
      (if (= 1 (length aux-gets))
   (term ((equ_same)
          ()
          ()
          (var_in)
          ,(cons (term (solve ,(first aux) ,(first aux-gets))) (second aux));second should always be blank?
          ,(third aux)))
   (error 'part-solve+eq "unsolveable"))
    ))
  )
;(term (part-solve+eq (("V" 1 1) ("F" 2 1) ("F" 1 1))
;                     (("null" -1 +nan.0) (+ ("F" 2 1) (+ ("F" 1 1) 0)))
;                     ((("V" 1 1) 2)
;                     (("V" 1 1) (* 3 ("F" 2 1)))
;                     (+nan.0 -nan.0)
;                     )))
;(term (part-solve+eq (("V" 1 1) ("F" 2 1) ("F" 1 1))
;                     (("F" 3 1) (+ ("F" 2 1) (+ ("F" 1 1) 0)))
;                     ((("V" 1 1) 2)
;                     (("V" 1 1) (* 3 ("F" 2 1)))
;                     (+nan.0 -nan.0)
;                     )))
;(term (part-solve+eq (("V" 1 1) ("F" 2 1) ("F" 1 1))
;                     (("null" -1 +nan.0) (+ ("F" 2 1) (+ ("F" 1 1) 0)))
;                     ((("V" 1 1) (* 2 ("F" 1 0)))
;                     (("V" 1 1) (* 3 ("F" 2 1)))
;                     (+nan.0 -nan.0)
;                     )))
;;;;;;


(define-metafunction equs
  add/unn/non : equ (var ...) equ ... -> any
  ((add/unn/non equ_add (var_1 var_r ...) equ_1 equ_comps ...);no skiping each var_r corrs to equ_comps
   ,(if (term (equcont equ_1 var_1))
        (let* ((eq (term (solve equ_1 var_1)))
               (ad (term(add/unn/non (equrep equ_add ,eq) (var_r ...) equ_comps ...)))
               )
          (if (term (done? ,eq))
              (term (,(first ad) ,(second ad) ,(cons eq (third ad))))
              (term (,(first ad) ,(cons eq (second ad)) ,(third ad)))
              ))
        (error 'add/unn/non "given equ_n not contain var_n")
        ))
  ((add/unn/non equ_add ())
   (equ_add () ()))
  )
  
   


;(step ((equ ...) ((var var) ...) (equ ...)  (var ...) (equ ...) (equ ...)))
;gives value, var in relations, desc relation, val needed, value in, done
(define-metafunction equs
  full-solve+eq : (var var ...) equ equ ... -> step
  ((full-solve+eq (var_same var_r ...) (("null" -1 -nan.0) exp_e) equ_comps ...)
   ,(let*((subin (term (add/unn/non (0 exp_e)  (var_r ...) equ_comps ...)))
          (sub-gets (term (get-unsolved ,(first subin ) ()))))
      (if (and (= 1 (length sub-gets)) (term(equ=? var_same ,(first sub-gets))))
          (let*((solved-add (term (solve ,(first subin) var_same)));solved for same
                ;(d (print (second solved-add)))
                (alcompsbs (map (λ (x) (term (equrep ,x ,solved-add))) (second subin)))
                );sub in same
          (term (()
                 ()
                 ()
                 ()
                 ()
                 ,(map (λ (x) (term (simp-equ ,x)))
                       (cons solved-add (append alcompsbs (third subin))))
                 )))
          (error 'full-solve+eq "unsolveable"))
          ))
  ((full-solve+eq (var_same var_r ...) (var_in exp_e) equ_comps ...)
   ,(let*((subin (term (add/unn/non (var_in exp_e)  (var_r ...) equ_comps ...)))
          (sub-gets (term (get-unsolved ,(first subin) (var_in)))))
      (if (and (= 1 (length sub-gets)) (term(equ=? var_same ,(first sub-gets))))
          (term (()
                 ((var_same var_in))
                 (,(first subin))
                 (var_same)
                 ,(second subin);solved for var_r in terms of same
                 ,(third subin)
                 ))
          (error 'full-solve+eq "unsolveable")))
   )
  )
;
;(term (full-solve+eq
;     (("F" 1 1) ("V" 2 1) ("V" 1 1))
;     (("null" -1 +nan.0) (+ ("V" 2 1) (+ ("V" 1 1) 0)))
;     (("V" 2 1) (* 3 ("F" 1 1)))
;     (("V" 1 1) 2)
;     ))
;(term (full-solve+eq
;     (("F" 1 1) ("V" 2 1) ("V" 1 1))
;     (("V" 3 1) (+ ("V" 2 1) (+ ("V" 1 1) 0)))
;     (("V" 2 1) (* 3 ("F" 1 1)))
;     (("V" 1 1) 2)
;     ))
;
;  





(define-metafunction equs
  sanatize : var ... ->(var ...)
  ((sanatize ("null" -1 +nan.0) var_r ...)
   (sanatize var_r ...))
  ((sanatize var_1 var_r ...)
   ,(cons (term var_1) (term (sanatize var_r ...))))
  ((sanatize)
   ())
  )

(define-metafunction equs
  get-same/restruct : var equ ... (equ ...) -> any;#f lst
  ((get-same/restruct var_same equ_1 equ_e ... (equ_i ...))
   ,(if (and (term (equcont equ_1 var_same)) (term (done? equ_1)))
        (if (not (term (get-same/restruct var_same equ_e ... (equ_i ... equ_1))))
            (term (equ_1 equ_i ... (+nan.0 -nan.0) equ_e ...))
            (error 'get-same/restruct "unsolveable multible definitions of same"))
        (term (get-same/restruct var_same equ_e ...(equ_i ... equ_1)))))
  ((get-same/restruct var_same (equ_i ...))
   #f)
  )

(define-metafunction equs
  merge-steps : (steps ...) -> step
  ((merge-steps (steps ...))
   ,(foldr (λ (x y) (map append x y)) '(() () () () () ()) (term (steps))))
  )
(define-metafunction equs
  get-desc : var step -> equ
  ((get-desc var_1 ((equ_give) () ()  (var_a ...) (equ_b ...) (equ_c ...)))
   ,(if (and (term (equcont equ_give var_1)) (term (done? equ_give)))
        (term equ_give)
        (begin (print (term equ_give)) (newline)
               (print (term var_same)) (newline)
               (error 'get-desc "gives wrong value"))
        ))
  ((get-desc var_same var_1 (() ((var_req var_get)) (equ_rel)  (var ...) (equ ...) (equ ...)))
   (if (and (term (equcont equ_rel var_same)) (term (equcont equ_give var_1)))
       (term equ_rel)
       (begin (print (term equ_rel)) (newline)
               (print (term var_same))(newline)
               (error 'getdesc "gives wrong value"))
       ))
  )
(define (get-desc-all vl lst)
  (map (λ (x y) (term (get-desc ,y ,x))) lst (take vl (length lst))))
   
;(step ((equ ...) ((var var) ...) (equ ...)  (var ...) (equ ...) (equ ...)))
;gives value, var in relations, desc relation, val needed, value in, done
(define-metafunction equs
  solve-dif : syst -> step
  ((solve-dif ((var_same var_add_in var_add ...) (equ_sum equ_comps ...) ()))
   ;is same var solve
   ;is equ sum solved
   ;must contain 1 RCIVF will give either sum or same(not and unless an input is set to 0)
   ;only one can be solve for becouse feed in
   ,(let*((same/struct (term(get-same/restruct var_same equ_comps ...()))));ret (equsame equ ...)
   (cond
     (same/struct (term(part-solve+eq (sanatize var_same var_add ...) equ_sum ,same/struct)))
     (else (term (full-solve+eq (var_same var_add ...) equ_sum equ_comps ...)));gets previus variable same allows to find sum given 2 pre
   )))
  ((solve-dif ((var_same var_add_in var_add ...) (equ_sum equ_comps ...) (syst_s ...)))
   ,(let*((nxcon (term ((solve-dif syst_s)...)))
          ;(d (begin (print nxcon) (newline)))
          (crcon (term (solve-dif ((var_same var_add_in var_add ...)
                                   ,(cons (term equ_sum) (append (get-desc-all (term (var_add ...)) nxcon)
                                                                 (term (equ_comps ...))))
                                   ()))))
          )
      (cond
        ((and (null? (first crcon)) (null? (second crcon)))
         (let (;(a (print crcon))
                (same (term (solve-for var_same ,(sixth crcon)))))
           (if same
         (term (()
                ()
                ()
                ()
                ()
                ,(append (map (λ (z) (term (simp-equ ,z)))
                               (sixth crcon))
                         (foldr (λ (x y) (append;trusts both are solved
                                    (map (λ (z) (term (simp-equ (equrep ,z ,same))))
                                         (fifth x))
                                    y)) null  nxcon)
                   (foldr (λ (x y) (append (sixth x) y))
                                  null nxcon))))
         (begin (print crcon) (error 'crcon "not know same v")))))
        ((not (null? (first crcon)))
          (term (,(map (λ (z) (term (simp-equ ,z))) (first crcon))
                 ()
                 ()
                 ,(fourth crcon)
                 ,(map (λ (z) (term (simp-equ ,z))) (fifth crcon))
                 ,(append (sixth crcon)
                   (foldr (λ (x y) (append;trusts both are solved
                                    (map (λ (z) (term (simp-equ (equrep ,z ,(first crcon)))))
                                        (fifth  x))
                                    y)) null  nxcon)
                   (foldr (λ (x y) (append (map (λ (z) (term (simp-equ ,z)))
                                                (sixth x)) y))
                                  null nxcon))
                 )))
          (else (let (;(a (print crcon))
                (slvd-sec-crc (term (solve ,(second crcon) var_same))))
          (term (()
                 ,(map (λ (z) (term (simp-equ ,z))) (second crcon))
                 ,(map (λ (z) (term (simp-equ ,z))) (third crcon))
                 ,(cons (fourth crcon) (foldr (λ (x y) (append (map (λ (z)
                                                 (if (term (equcont ,slvd-sec-crc ,(first z)))
                                                 (list (term var_same) (second x))
                                                 (error 'slvd-sec-crc "expected incorect variable")))
                                               (fourth x))
                                          y)) null nxcon))
                 ,(cons (fifth crcon) (foldr (λ (x y) (append (map (λ (z)
                                                 (term (simp-equ (equrep ,z ,slvd-sec-crc))))
                                               (fifth x))
                                          y)) null nxcon))
                 ,(append (sixth crcon) (foldr (λ (x y) (append (map (λ (z)
                                                 (term (simp-equ ,z)))
                                                        (sixth x)) y))
                                  null nxcon))
                 ))))
          )))
  )
          
                                          
                 



;(term (full-solve+eq
;     (("F" 1 1) ("V" 2 1) ("V" 1 1))
;     (("null" -1 +nan.0) (+ ("V" 2 1) (+ ("V" 1 1) 0)))
;     (("V" 2 1) (* 3 ("F" 1 1)))
;     (("V" 1 1) 2)
;     ))
;(term (solve-dif ((("F" 1 1) ("V" 0 1) ("V" 2 1) ("V" 1 1))
;   ((("V" 0 1) (+ ("V" 2 1) (+ ("V" 1 1) 0)))
;    (("V" 2 1) (* 3 ("F" 1 1)))
;    (("V" 1 1) 2))
;   ())))
;(term (solve-dif
;   ((("F" 1 1) ("null" -1 +nan.0) ("V" 3 1) ("V" 2 1) ("V" 1 1))
;   ((("null" -1 +nan.0) (+ ("V" 3 1) (+ ("V" 2 1) (+ ("V" 1 1) 0))))
;    (("V" 3 1) (* 1 ("F" 3 0)))
;    (("V" 2 1) (* 3 ("F" 1 1)))
;    (("V" 1 1) 2))
;   ())
;   ))
;
;(term (part-solve+eq (("V" 1 1) ("F" 2 1) ("F" 1 1))
;                     (("null" -1 +nan.0) (+ ("F" 2 1) (+ ("F" 1 1) 0)))
;                     ((("V" 1 1) 2)
;                     (("V" 1 1) (* 3 ("F" 2 1)))
;                     (+nan.0 -nan.0)
;                     )))
;(term(solve-dif
;      ((("V" 1 1) ("F" 0 1) ("F" 2 1) ("F" 1 1))
;       ((("F" 0 1) (+ ("F" 2 1) (+ ("F" 1 1) 0)))
;        (("V" 1 1) (* 3 ("F" 2 1)))
;        (("V" 1 1) 2))
;       ())
;      ))
;(term(solve-dif
;      ((("F" 1 1) ("V" 0 1) ("V" 1 1))
;       ((("V" 0 1) (+ ("V" 1 1) 0))
;        (("V" 1 1) (* 3 ("F" 1 1))))
;       ())
;      ))

;(term (solve-dif
;       ((("F" 1 1) ("null" -1 +nan.0) ("V" 4 1) ("V" 3 1) ("V" 2 1) ("V" 1 1))
;   ((("null" -1 +nan.0) (+ ("V" 4 1) (+ ("V" 3 1) (+ ("V" 2 1) (+ ("V" 1 1) 0)))))
;    (("F" 1 1) (* 1 ("V" 3 0)))
;    (("V" 2 1) (* 3 ("F" 1 1)))
;    (("V" 1 1) 2))
;   (((("V" 4 1) ("F" 1 1) ("F" 6 1) ("F" 5 1))
;     ((("F" 1 1) (+ ("F" 6 1) (+ ("F" 5 1) 0)))
;      (("V" 4 1) (* 1 ("F" 6 0)))
;      (("V" 4 1) (* 2 ("F" 5 1))))
;     ())))
;       ))

;(term (solve-for ("F" 1 1) ((("F" 1 1) (* 1 ("V" 3 0)))
;        (("V" 4 1) (/ (- 0 (* 1 (* 1 ("F" 6 0)))) (- 0 (* 1 1))))
;        (("V" 2 1) (/ (- 0 (* 1 (* 3 (* 1 ("V" 3 0))))) (- 0 (* 1 1))))
;        (("V" 1 1) (/ (- 0 (* 1 2)) (- 0 (* 1 1)))))))

(term (solve-dif
       ((("V" 1 1) ("null" -1 +nan.0) ("F" 2 1) ("F" 1 1))
   ((("null" -1 +nan.0) (+ ("F" 2 1) (+ ("F" 1 1) 0))) (("V" 1 1) (* ("R" 1 0) ("F" 2 1))) (("V" 1 1) ("V" 1 0)))
   ())
       ))

(term (solve-dif
      ((("F" 1 1) ("null" -1 +nan.0) ("V" 6 1) ("V" 3 1) ("V" 2 1) ("V" 1 1))
   ((("null" -1 +nan.0) (+ ("V" 6 1) (+ ("V" 3 1) (+ ("V" 2 1) (+ ("V" 1 1) 0)))))
    (("V" 2 1) (* ("R" 1 0) ("F" 1 1)))
    (("V" 1 1) ("V" 1 0)))
   (((("V" 6 1) ("F" 1 1) ("F" 8 1) ("F" 7 1))
     ((("F" 1 1) (+ ("F" 8 1) (+ ("F" 7 1) 0)))
      (("V" 6 1) (/ ("F" 8 0) ("C" 3 0)))
      (("V" 6 1) (* ("R" 3 0) ("F" 7 1))))
     ())
    ((("V" 3 1) ("F" 1 1) ("F" 5 1) ("F" 4 1))
     ((("F" 1 1) (+ ("F" 5 1) (+ ("F" 4 1) 0)))
      (("V" 3 1) (/ ("F" 5 0) ("C" 2 0)))
      (("V" 3 1) (* ("R" 2 0) ("F" 4 1))))
     ())))
      ));works