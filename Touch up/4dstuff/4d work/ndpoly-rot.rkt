#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "opvec.rkt")
(provide (all-defined-out))
;rot


(define (build-rot-mat 1dim 2dim theta dim)
  (let ((mat (matrix-identity dim)))
    (begin
      (matrix-set! mat 1dim 1dim (cos theta))
      (matrix-set! mat 1dim 2dim (* -1 (sin theta)))
      (matrix-set! mat 2dim 1dim (sin theta))
      (matrix-set! mat 2dim 2dim (cos theta))
     mat)))
;(mat->ll (build-rot-mat 0 1 pi 3))
;(mat->ll (build-rot-mat 0 1 0 3))
;(mat->ll (build-rot-mat 0 1 (/ pi 4) 3))

(define (rotat ip deg-rot-mat)
  (matrix-mul deg-rot-mat ip))
;(mat->ll (rotat (l->mat '(1 0 0)) 0 1 (/ pi 2)))
;(mat->ll (rotat (l->mat '(1 0 0)) 0 2 (/ pi -4)))

(define (find-line-axis vec alodir)
  (foldr (λ (x y z) (vecl+ (vecl-scale x y) z)) (make-list (length vec) 0) 
        (mat->ll  (matrix-inverse (ll->mat alodir)))
        vec))

;(find-line-axis '(0.7071067811865476 0.7071067811865475 0)
 ;               '((1 0 0)
  ;                (0 1 0)
   ;               (0 0 1)))
;(find-line-axis '(0.7071067811865476 0.7071067811865475 0)
 ;               '((0.7071067811865476 0.7071067811865475 0)
  ;                (0.7071067811865476 -0.7071067811865475 0)
   ;               (0 0 1)))

(define (find-line-axis-inv vec alodir)
  (foldr (λ (x y z) (vecl+ (vecl-scale x y) z)) (make-list (length vec) 0)
         alodir
         vec))
;for nby1
(define (in-axis p axi)
  (local ((define axis (matrix-inverse axi))
          (define mv (matrix-rows axi))
          (define mm (make-matrix mv 1 0))
          (define (scal+ n ac1sv 2lv)
            (if (= ac1sv mv) mm
                (begin (matrix-set! mm ac1sv 0 (+ (matrix-ref mm ac1sv 0) (* n (matrix-ref axis ac1sv 2lv)))) (scal+ n (add1 ac1sv) 2lv))))
          (define (mldr ac2lv)
            (if (= ac2lv mv) mm    ;   transposed bc mat property of mats
                (begin (scal+ (matrix-ref p ac2lv 0) 0 ac2lv) (mldr (add1 ac2lv)))))
          
          )
    (mldr 0)))
;(mat->ll (in-axis (l->mat '(0.7071067811865476 0.7071067811865475 0)) (ll->mat 
 ;                                                                     '((1 0 0)
  ;                                                                      (0 1 0)
   ;                                                                     (0 0 1)))))
;(mat->ll (in-axis (l->mat '(0.7071067811865476 0.7071067811865475 0)) (ll->mat 
 ;                                                                     '((0.7071067811865476 0.7071067811865475 0)
  ;                                                                      (0.7071067811865476 -0.7071067811865475 0)
   ;                                                                     (0 0 1)))))

;for nby1
(define (in-axis-inv p axis)
  (local ((define mv (matrix-rows axis))
          (define mm (make-matrix mv 1 0))
          (define (scal+ n ac1sv 2lv)
            (if (= ac1sv mv) mm
                (begin (matrix-set! mm ac1sv 0 (+ (matrix-ref mm ac1sv 0) (* n (matrix-ref axis ac1sv 2lv)))) (scal+ n (add1 ac1sv) 2lv))))
          (define (mldr ac2lv)
            (if (= ac2lv mv) mm    ;   transposed bc mat property of mats
                (begin (scal+ (matrix-ref p ac2lv 0) 0 ac2lv) (mldr (add1 ac2lv)))))
          
          )
    (mldr 0)))

;(mat->ll (in-axis-inv (in-axis (l->mat '(0.7071067811865476 0.7071067811865475 0)) (ll->mat 
 ;                                                                    '((1 0 0)
  ;                                                                      (0 1 0)
   ;                                                                     (0 0 1))))(ll->mat 
    ;                                                                  '((1 0 0)
     ;                                                                   (0 1 0)
      ;                                                                  (0 0 1)))))
;(mat->ll (in-axis-inv (in-axis (l->mat '(0.7071067811865476 0.7071067811865475 0)) (ll->mat 
 ;                                                                     '((0.7071067811865476 0.7071067811865475 0)
  ;                                                                      (0.7071067811865476 -0.7071067811865475 0)
   ;                                                                     (0 0 1))))(ll->mat 
    ;                                                                  '((0.7071067811865476 0.7071067811865475 0)
     ;                                                                   (0.7071067811865476 -0.7071067811865475 0)
      ;                                                                  (0 0 1)))))


;for nbyn
(define (in-axis-sqm p axi)
  (local ((define axis (matrix-inverse axi))
          (define mv (matrix-rows axi))
          (define mm (make-matrix mv mv 0))
          (define (scal+ n ac1sv 2lv ac)
            (if (= ac1sv mv) mm
                (begin (matrix-set! mm ac1sv ac (+ (matrix-ref mm ac1sv ac) (* n (matrix-ref axis ac1sv 2lv)))) (scal+ n (add1 ac1sv) 2lv ac))))
          (define (mldr ac2lv ac)
            (if (= ac2lv mv) mm    ;   transposed bc mat property of mats
                (begin (scal+ (matrix-ref p ac2lv ac) 0 ac2lv ac) (mldr (add1 ac2lv) ac))))
          (define (mldr-col acc)
            (if (= acc mv) mm
                (begin (mldr 0 acc) (mldr-col (add1 acc)))))
          )
    (mldr-col 0)))
;(mat->ll (in-axis-sqm (ll->mat '((0.7071067811865476 0.7071067811865475 0)
 ;                                (0.7071067811865476 -0.7071067811865475 0)
  ;                               (0 0 1))) (ll->mat 
   ;                            '((0.7071067811865476 0.7071067811865475 0)
    ;                             (0.7071067811865476 -0.7071067811865475 0)
     ;                            (0 0 1)))))
;""
;(mat->ll (in-axis (l->mat '(0.7071067811865476 0.7071067811865475 0))
 ;                    (ll->mat '((0.7071067811865476 0.7071067811865475 0)
  ;                      (0.7071067811865476 -0.7071067811865475 0)
   ;                     (0 0 1)))))
;(mat->ll (in-axis (l->mat '(0.7071067811865476 -0.7071067811865475 0))
 ;                     (ll->mat '((0.7071067811865476 0.7071067811865475 0)
  ;                      (0.7071067811865476 -0.7071067811865475 0)
   ;                     (0 0 1)))))
;(mat->ll (in-axis (l->mat '(0 0 1))
 ;                     (ll->mat '((0.7071067811865476 0.7071067811865475 0)
  ;                      (0.7071067811865476 -0.7071067811865475 0)
   ;                     (0 0 1)))))
;""
;for nbyn
(define (in-axis-inv-sqm p axis)
  (local ((define mv (matrix-rows axis))
          (define mm (make-matrix mv mv 0))
          (define (scal+ n ac1sv 2lv ac)
            (if (= ac1sv mv) mm
                (begin (matrix-set! mm ac1sv ac (+ (matrix-ref mm ac1sv ac) (* n (matrix-ref axis ac1sv 2lv)))) (scal+ n (add1 ac1sv) 2lv ac))))
          (define (mldr ac2lv ac)
            (if (= ac2lv mv) mm    ;   transposed bc mat property of mats
                (begin (scal+ (matrix-ref p ac2lv ac) 0 ac2lv ac) (mldr (add1 ac2lv) ac))))
          (define (mldr-col acc)
            (if (= acc mv) mm
                (begin (mldr 0 acc) (mldr-col (add1 acc)))))
          )
    (mldr-col 0)))

;(mat->ll(in-axis-inv-sqm (in-axis-sqm (ll->mat '((0.7071067811865476 0.7071067811865475 0)(0.7071067811865476 0.7071067811865475 0)(0.7071067811865476 0.7071067811865475 0))) (ll->mat 
 ;                                                                     '((0.7071067811865476 0.7071067811865475 0)
  ;                                                                      (0.7071067811865476 -0.7071067811865475 0)
   ;                                                                     (0 0 1))))(ll->mat 
    ;                                                                  '((0.7071067811865476 0.7071067811865475 0)
     ;                                                                   (0.7071067811865476 -0.7071067811865475 0)
      ;                                                                  (0 0 1)))))

;(mat->ll(in-axis-inv-sqm (in-axis-sqm (ll->mat '((0.7071067811865476 0.7071067811865475 0)(0.7071067811865476 0.7071067811865475 0)(0.7071067811865476 0.7071067811865475 0))) (ll->mat 
 ;                                                                     '((0 1 0)
  ;                                                                      (.2 .5 0)
   ;                                                                     (0 0 1))))(ll->mat 
    ;                                                                  '((0 1 0)
     ;                                                                   (.2 .5 0)
      ;                                                                  (0 0 1)))))


(define (rot-axis axs rot-mat)
  (in-axis-inv-sqm rot-mat axs))

(define (rot-abt-pt-abs  rp cp deg-rot-mat)
  (matrix-add cp (rotat (matrix-sub rp cp) deg-rot-mat)))

(define (matrix-rot p axis deg-rot-mat)
  (in-axis-inv (rotat (in-axis p axis) deg-rot-mat) axis))

(define (rot-abt-pt rp cp axis deg-rot-mat)
  (matrix-add cp (matrix-rot (matrix-sub rp cp) axis deg-rot-mat)))
;used to get deg-rot-mat (build-rot-mat 1dim 2dim deg (matrix-rows ip))
