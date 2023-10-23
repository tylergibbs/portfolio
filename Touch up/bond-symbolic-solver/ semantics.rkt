#lang racket
(require "equations.rkt")
(require redex)
(require redex/reduction-semantics)

(define-extended-language bond+equs
  equs
  (const V F)
  (V #f (v exp))
  (F #f (f exp))
  (junct 0junct 1junct trans)
  (0junct (0 string string R C I V F junct ... ))
  (1junct (1 string string R C I V F junct ... ))
  (arw C R I V F)
  (C #f (c exp))
  (R #f (r exp))
  (I #f (i exp))
  
  )


(define-metafunction bond+equs
  const->eqs : const var -> equ
  ((const->eqs (v exp) var_know)
   (var_know exp))
  ((const->eqs (f exp) var_know)
   (var_know exp))
  ((const->eqs #f var_know)
   ,(error 'const->eqs))
  )

(define-metafunction bond+equs
  r->eqs : arw var var -> equ
  ((r->eqs (r exp_tr) var_V var_F)
   (var_V (* exp_tr var_F)))
  )

(define-metafunction bond+equs
  c->eqs : arw var var -> equ
  ((c->eqs (c exp_tr) var_Q var_V)
   (var_V (/ var_Q exp_tr)))
  )

(define-metafunction bond+equs
  i->eqs : arw var var -> equ
  ((i->eqs (i exp_tr) var_Fx var_F)
   (var_F (/ var_Fx exp_tr)))
  )

(define-metafunction bond+equs ; assumes 1 0 alternation
  get-syst : var var any any junct ... -> any
  ((get-syst (string_get exp_n1 1) (string_give exp_n2 1) any_var any_syst junct_out1 junct_out ...)
   ,(local ((define nsysf (term (jct->eqs junct_out1 exp_n1 (string_give exp_n2 1) (string_get exp_n1 1))))
            (define nsys (first nsysf))
            (define nn (second nsysf))
            (define aux
                 (term (get-syst (string_get ,nn 1) (string_give exp_n2 1)
                                 ,(cons (term (string_get exp_n1 1)) (term any_var))
                                 ,(cons nsys (term any_syst))
                                 junct_out ...)))
            (define +vargrp (first aux))
            (define systgrp (second aux))
            (define nnn (third aux))
            )
      (list +vargrp systgrp nnn)))
  ((get-syst (string_get exp_n1 1) (string_give exp_n2 1) any_vars any_systs)
   ,(list (term any_vars) (term any_systs) (term exp_n1)))
  )

  
(define-metafunction bond+equs
  jct->eqs : junct exp var var -> (syst exp)
  ((jct->eqs (0 string_Flux string_Q R_r C_c I_i V_v F_f junct_out ... )exp_n
             (string_pos exp_id exp_dir) var_start-same)
   ;errors will come from solving the syst
   ,(local((define n (add1 (term exp_n)))
          (define vargrps null);added stuff
          (define equgrps null)
          (define same-var (term var_start-same))

          (define-values (vargrps1 equgrps1 n1);1
            (if (term V_v) (values
                        (cons(term (string_Q ,n 1))vargrps)
                        (cons (term (const->eqs V_v ,same-var))equgrps)
                        (add1 n)
                        )
            (values vargrps equgrps n)))
          (define-values (vargrps2 equgrps2 n2);2
            (if (term F_f) (values
                        (cons(term (string_Q ,n1 1))vargrps1)
                        (cons (term (const->eqs F_f (string_Q ,n1 1)))equgrps1)
                        (add1 n1)
                        )
            (values vargrps1 equgrps1 n1)))

          (define-values (vargrps3 equgrps3 n3);3
            (if (term R_r) (values
                        (cons(term (string_Q ,n2 1))vargrps2)
                        (cons (term (r->eqs R_r ,same-var (string_Q ,n2 1)))equgrps2)
                        (add1 n2)
                       )
            (values vargrps2 equgrps2 n2)))

          (define-values (vargrps4 equgrps4 n4);4
            (if (term C_c) (values
                        (cons(term (string_Q ,n3 1))vargrps3)
                        (cons (term (c->eqs C_c (string_Q ,n3 0) ,same-var))equgrps3)
                        (add1 n3)
                       )
            (values vargrps3 equgrps3 n3)))
          
           (define-values (vargrps5 equgrps5 n5);5
            (if (term I_i) (values
                        (cons(term (string_Q ,n4 1))vargrps4)
                        (cons (term (i->eqs I_i (string_Flux ,n4 0) (string_Q ,n4 1)))equgrps4)
                        (add1 n4)
                       )
            (values vargrps4 equgrps4 n4)))
          

          (define aux
            (term (get-syst (string_Q ,n5 1) ,same-var ,vargrps5 ,null junct_out ...)))
          (define +vargrp  (first aux))
          (define systgrp (second aux))
          (define nn (third aux))
          (define input (term (string_pos exp_id exp_dir)))
          )
      
      (list (term (,(cons same-var (cons input +vargrp))
                        ,(cons (term(,input ,(foldr (λ (x y) (term (+ ,x ,y))) 0 +vargrp)))equgrps5)
                         ,systgrp))
                 nn)
      )
   )
  ((jct->eqs (1 string_Flux string_Q R_r C_c I_i V_v F_f junct_out ... )exp_n
             (string_pos exp_id exp_dir) var_start-same)
   ;errors will come from solving the syst
   ,(local ((define n (add1 (term exp_n)))
          (define vargrps (list ));added stuff
          (define equgrps (list))
          (define same-var (term var_start-same))
          
          (define-values (vargrps1 equgrps1 n1);1
            (if (term V_v) (values
                        (cons (term (string_Flux ,n 1)) vargrps)
                        (cons (term (const->eqs V_v (string_Flux ,n 1)))equgrps)
                        (add1 n)
                        )
            (values vargrps equgrps n)))
          (define-values (vargrps2 equgrps2 n2);2
            (if (term F_f) (values
                        (cons (term (string_Flux ,n1 1)) vargrps1)
                        (cons (term (const->eqs F_f ,same-var))equgrps1)
                        (add1 n1)
                        )
            (values vargrps1 equgrps1 n1)))

          (define-values (vargrps3 equgrps3 n3);3
            (if (term R_r) (values
                        (cons(term (string_Flux ,n2 1))vargrps2)
                        (cons (term (r->eqs R_r (string_Flux ,n2 1) ,same-var))equgrps2)
                        (add1 n2)
                       )
            (values vargrps2 equgrps2 n2)))

          (define-values (vargrps4 equgrps4 n4);4
            (if (term C_c) (values
                        (cons(term (string_Flux ,n3 1))vargrps3)
                        (cons (term (c->eqs C_c (string_Q ,n3 0) (string_Flux ,n3 1)))equgrps3)
                        (add1 n3)
                       )
            (values vargrps3 equgrps3 n3)))
          
           (define-values (vargrps5 equgrps5 n5);5
            (if (term I_i) (values
                        (cons(term (string_Flux ,n4 1))vargrps4)
                        (cons (term (i->eqs I_i (string_Flux ,n4 0) ,same-var))equgrps4)
                        (add1 n4)
                       )
            (values vargrps4 equgrps4 n4)))

          
          (define aux
            (term (get-syst (string_Flux ,n5 1) ,same-var ,vargrps5 ,null junct_out ...)))
          (define +vargrp (first aux))
          (define systgrp (second aux))
          (define nn (third aux))
          (define input (term (string_pos exp_id exp_dir)))
          )
      (list (term (,(cons same-var (cons input +vargrp))
                     ,(cons (term(,input
                                 ,(foldr (λ (x y) (term (+ ,x ,y))) 0 +vargrp)))equgrps5)
                     ,systgrp))
            nn)
      )
   )
  )
(term (jct->eqs (0 "V" "F" (r 2) #f #f (v 8) #f (1 "V" "F" (r 2) #f #f #f #f )) 0 ("null" -1 -nan.0) ("V" 1 1)))
(term (jct->eqs (1 "V" "F" (r 2) (c 1) #f (v 4) #f) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (1 "V" "F" (r 2) (c 1) #f #f (f 4)) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (1 "V" "F" #f (c 1) #f (v 2) #f) 0 ("null" -1 -nan.0) ("F" 1 1)));not enough information ot solve
(term (jct->eqs (1 "V" "F" (r 2) (c 1) #f (v 2) (f 4)) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (0 "V" "F" (r 2) (c 1) #f (v 2) (f 4)) 0 ("null" -1 -nan.0) ("V" 1 1)));not enough information to solve
(term (jct->eqs (0 "V" "F" (r 3) #f  (i 1) (v 2) #f) 0 ("null" -1 -nan.0) ("V" 1 1)))
(term (jct->eqs (1 "V" "F" (r 3) #f  (i 1) (v 2) #f) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (1 "V" "F" (r 3) #f  (i 1) (v 2) #f
                   (0 "V" "F" (r 2) (c 1) #f #f #f)
                   ) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (1 "V" "F" (r 3) #f  (i 1) (v 2) #f
                   (0 "V" "F" (r 2) (c 1) #f #f #f)
                   (0 "V" "F" (r 2) (c 1) #f #f #f)
                   ) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (0 "V" "F" (r 3) #f  #f (v 2) #f) 0 ("null" -1 -nan.0) ("V" 1 1)))
(term (jct->eqs (1 "V" "F" (r 3) (c 1)  #f (v 2) #f) 0 ("null" -1 -nan.0) ("F" 1 1)))
(term (jct->eqs (1 "V" "F" (r 3) #f  #f #f #f) 0 ("V" 0 1) ("F" 1 1)))

(term (jct->eqs (0 "V" "F" (r ("R" 1 0)) #f  #f (v ("V" 1 0)) #f) 0 ("null" -1 -nan.0) ("V" 1 1)))

(term (jct->eqs (1 "V" "F" (r ("R" 1 0)) #f  #f (v ("V" 1 0)) #f
                   (0 "V" "F" (r ("R" 2 0)) (c ("C" 2 0)) #f #f #f)
                   (0 "V" "F" (r ("R" 3 0)) (c ("C" 3 0)) #f #f #f)
                   ) 0 ("null" -1 -nan.0) ("F" 1 1)))


   

