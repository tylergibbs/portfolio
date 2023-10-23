#lang racket
(require math/matrix)
(define (vec- v vp)
  (map - v vp))
(define (vec+ v vp)
  (map + v vp))
(define (vec-scale v n)
  (map (λ (x) (* n x)) v))
(define (mat->list l alon)
  (if (null? alon) null (cons (take alon l) (mat->list l (drop alon l)))))
(define (xy->p x y width)
  ;(print x) (newline) (print y)(newline) (print (*  4 (+ x (* y width))))(newline)(newline)
  (inexact->exact (*  4 (truncate (+ x (* y width))))))
(define (2d-mat-inv mat)
  (local (
        (define a (first (first mat)))
        (define b (second (first mat)))
        (define c (first (second mat)))
        (define d (second (second mat)))
        (define det (- (* a d) (* c b))))
    (if (zero? det) '((0 0)(0 0))
    (list (list (* (/ 1 det) d) (* -1 (/ 1 det) b))
          (list (* -1 (/ 1 det) c) (* (/ 1 det) a))))))
(define (p-in-plain vec alodir)
  ;(print alodir)
  (set! vec (take vec 2))
  (foldr (λ (x y z) (vec+ (vec-scale x y) z)) (make-list 2 0) (2d-mat-inv alodir) vec))
;#########################################################################################################
(require racket/draw)
(define-struct squar (start axis width hight btst)#:transparent)
(define-struct line (vec start) #:transparent)
(define-struct hold (dist pixl))
;btstr
(define (move-pixle tb tbr fb fbr)
  (begin
    (bytes-set! tb tbr (bytes-ref fb fbr))
    (bytes-set! tb (+ 1 tbr) (bytes-ref fb (+ 1 fbr)))
    (bytes-set! tb (+ 2 tbr) (bytes-ref fb (+ 2 fbr)))
    (bytes-set! tb (+ 3 tbr) (bytes-ref fb (+ 3 fbr)))
    tb))

;btstr
(define (pixl-ref tbst n)
  (bytes
    (bytes-ref tbst n)
    (bytes-ref tbst (+ 1 n))
    (bytes-ref tbst (+ 2 n))
    (bytes-ref tbst (+ 3 n))))


;line


;bool
(define (in-sqr p sq)
  (and (<= (first p) (squar-width sq)) (<= (second p) (squar-hight sq))
       (>= (first p) 0) (>= (second p) 0)
       ))


;hold/bool
(define (in-sq sq p d)
    (if 
     (in-sqr p sq)
     (hold d (pixl-ref (squar-btst sq) (xy->p (first p) (second p) (squar-width sq))))
     #f))

(define (in-plain p sq)
  (if p
  (let ((LS (vec- p (squar-start sq)))
        (in-p (p-in-plain p (squar-axis sq)))
        )
    in-p)
  #f))


(define (check-T T S Q lin sq)
  (let* ((p (vec+ (vec-scale (line-vec lin) T) (line-start lin)))
        (sq-p (vec+ (vec-scale (first (squar-axis sq))S) (vec+ (vec-scale (second (squar-axis sq))Q) (squar-start sq))))
        (asw (if (and (foldr (λ (x y z) (and (<= (abs (- x y)) .01) z)) #t p sq-p) (not (foldr (λ (x y) (or (negative? x) y)) #f (drop p 2)))
           ) sq-p #f))
        ;(A (if (not asw) (begin (print p)(newline) (print sq-p)(newline) (newline)) (begin (print #f) (newline))))
        )
    asw
  ))



;p
;N(x y z w ...)+(x y z w ...) = T(x y z w ...)+S(x y z w ...)+(x y z w ...)
;     M              D               A              B              E
(define (find-p lin sq)
  (let ((M (line-vec lin))
        (D (line-start lin))
        (A (first (squar-axis sq)))
        (B (second (squar-axis sq)))
        (E (squar-start sq)))
    (let((x1 (first M))
         (x2 (first D))
         (x3 (first A))
         (x4 (first B))
         (x5 (first E))
         (y1 (second M))
         (y2 (second D))
         (y3 (second A))
         (y4 (second B))
         (y5 (second E))
         (z1 (third M))
         (z2 (third D))
         (z3 (third A))
         (z4 (third B))
         (z5 (third E)))
     (let ((dt (+(* x1(-(* y4 z3) (* y3 z4))) (* x3 (- (* y1 z4) (* y4 z1))) (* x4(-(* y3 z1) (* y1 z3)))))
        (dq (+ (* x1(-(* y4 z3) (* y3 z4))) (* x3(-(* y1 z4) (* y4 z1))) (* x4(-(* y3 z1) (* y1 z3)))))
        (ds (+(* x1(-(* y4 z3) (* y3 z4))) (* x3(-(* y1 z4) (* y4 z1))) (* x4(-(* y3 z1) (* y1 z3))))))
      (if (or (zero? dt) (zero? dq) (zero? ds)) #f;dissaper becosue will be face at angle to to display
           (let ((t (/(+(* x4 (+(* y3 (- z5 z2)) (* -1 y5 z3) (* y2 z3))) (* x3(+ (* y4 (- z2 z5)) (* y5 z4) (* -1 y2 z4))) (* x2 (- (* y3 z4) (* y4 z3))) (* x5(- (* y4 z3) (* y3 z4)))) dt))

                (q (/ (* -1 (+ (* x3 (-(* y1(- z5 z2)) (+(* y5 z1) (* y2 z1)))) (* x1(+(* y3(- z2 z5)) (* y5 z3) (* -1 y2 z3))) (* x2 (-(* y1 z3) (* y3 z1))) (* x5(-(* y3 z1) (* y1 z3))))) dq))

                (s (/(+(* x4(+ (* y1(- z5 z2)) (* -1 y5 z1) (* y2 z1))) (* x1(+(* y4(- z2 z5)) (* y5 z4) (* -1 y2 z4))) (* x2(-(* y1 z4) (* y4 z1))) (* x5 (-(* y4 z1) (* y1 z4)))) dq)))
    (check-T t s q lin sq))))
      )))





(define (dist l li)
  (sqrt (foldr (λ (x y z) (+ (sqr (- x y)) z)) 0 l li)))

;hold/bool
(define (intersect-sq? lin sq)
  (let ((A (find-p lin sq)))
    (if A
        ;(begin (print (dist (line-start lin) A))(newline) (print (vec- A (squar-start sq))) (newline)
        (in-sq sq (vec- (in-plain A sq) (take (squar-start sq) 2)) (dist (line-start lin) A))
        #f)))

(define (comp h acc)
  (if (< (hold-dist h) (hold-dist acc))
      h
      acc))

;pixl/#f
(define (find-intersect-tri lin alotri acc)
  (if
    (null? alotri) (hold-pixl acc)
    (let ((h (intersect-sq? lin (first alotri))))
      (if h (find-intersect-tri lin (rest alotri) (comp h acc))
          (find-intersect-tri lin (rest alotri) acc)))))
#|
(define (get-line p1 xint)
  (line (vec- p1 xint) xint))
(define (make-point x y p )
  (append (list x y) (make-list (- (length p) 2)0)))
(define (make-cp cam-p dist)
  (append '(50 50) (make-list (- (length cam-p)2) (* 1 dist))))
|#
(define (get-line x y cam-p dist)
  (line (append (list x y) (make-list (- (length cam-p)2) dist)) (append (list 0 0) (make-list (- (length cam-p) 2) (* -1 dist)))))

(define (get-2d-rec alotri cam-p bytstr xval yval xmax ymax bytstr-ref bytstr-l dist)
  (if 
    (= bytstr-l bytstr-ref) bytstr
    (let (;(A (newline))
          (thing (find-intersect-tri (get-line xval yval cam-p dist) alotri (hold +inf.0 #f))))
      (if thing
      (if
         (= xval xmax) (get-2d-rec alotri cam-p (move-pixle bytstr (* 4 bytstr-ref) thing 0) -50 (add1 yval) xmax ymax (add1 bytstr-ref) bytstr-l dist)
         (get-2d-rec alotri cam-p (move-pixle bytstr (* 4 bytstr-ref) thing 0) (add1 xval) yval xmax ymax (add1 bytstr-ref) bytstr-l dist))
      (if
         (= xval xmax) (get-2d-rec alotri cam-p bytstr -50 (add1 yval) xmax ymax (add1 bytstr-ref) bytstr-l dist)
         (get-2d-rec alotri cam-p bytstr (add1 xval) yval xmax ymax (add1 bytstr-ref) bytstr-l dist))))))

(define (transform sq cam-p);this is a hack but should work becouse 1 pixle off
  (squar (map (λ (x y) (inexact->exact (round #|here is he hack|# (- x y)))) (squar-start sq) cam-p) (squar-axis sq) (squar-width sq) (squar-hight sq) (squar-btst sq)))
;this is a hack but should work becouse 1 pixle off


(define (get-rel-posn x y sq)
  (let ((relp (vec- (p-in-plain (list x y) (squar-axis sq)) (squar-start sq))))
    (if (in-sqr relp sq) (pixl-ref (squar-btst sq) (xy->p (first relp) (second relp) (squar-width sq))) #f)))

(define (get-2d-int-byte x y alosq)
  (if
   (null? alosq) #f
   (let ((thing (get-rel-posn x y (first alosq))))
     (if thing
         thing
         (get-2d-int-byte x y (rest alosq))))))

(define (2d-get alosq bytstr xval yval xmax ymax bytstr-ref bytstr-l)
  (if 
    (= bytstr-l bytstr-ref) bytstr
    (let ((thing (get-2d-int-byte xval yval alosq)))
      (if thing
          (if
           (= xval xmax) (2d-get alosq (move-pixle bytstr (* 4 bytstr-ref) thing 0) -50 (add1 yval) xmax ymax (add1 bytstr-ref) bytstr-l)
           (2d-get alosq (move-pixle bytstr (* 4 bytstr-ref) thing 0) (add1 xval) yval xmax ymax (add1 bytstr-ref) bytstr-l))
          (if 
           (= xval xmax) (2d-get alosq bytstr -50 (add1 yval) xmax ymax (add1 bytstr-ref) bytstr-l)
           (2d-get alosq bytstr (add1 xval) yval xmax ymax (add1 bytstr-ref) bytstr-l))))))

(define (get-2d alosq cam-p bytstr width hight dist)
  ;(print (map (λ (x) (transform x cam-p)) alosq))
  ;(newline)
  (if (= (length cam-p) 2)
      (2d-get (map (λ (x) (transform x cam-p)) alosq)  bytstr (/ width -2) (/ hight -2) (sub1 (/ width 2)) (sub1 (/ hight 2)) 0 (/ (bytes-length bytstr)4))
      (get-2d-rec (map (λ (x) (transform x cam-p)) alosq) cam-p bytstr (/ width -2) (/ hight -2) (sub1 (/ width 2)) (sub1 (/ hight 2)) 0 (/ (bytes-length bytstr) 4) dist)))

#|
"2d"
(local(
(define empty-sceen0   (make-bytes (* 4 100 100) 0))
(define empty-sceen1   (make-bytes (* 4 100 100) 0))
(define empty-sceen2   (make-bytes (* 4 100 100) 0))
(define empty-sceen3   (make-bytes (* 4 100 100) 0))
(define empty-sceen4   (make-bytes (* 4 100 100) 0))
(define empty-sceen5   (make-bytes (* 4 100 100) 0))
(define empty-sceen6   (make-bytes (* 4 100 100) 0))
(define empty-sceen7   (make-bytes (* 4 100 100) 0))
(define empty-bitmap0 (make-object bitmap% 99 99))
(define empty-bitmap1 (make-object bitmap% 99 99))
(define empty-bitmap2 (make-object bitmap% 99 99))
(define empty-bitmap3 (make-object bitmap% 99 99))
(define empty-bitmap4 (make-object bitmap% 99 99))
(define empty-bitmap5 (make-object bitmap% 99 99))
(define empty-bitmap6 (make-object bitmap% 99 99))
(define empty-bitmap7 (make-object bitmap% 99 99)))
(begin
;(send empty-bitmap0 set-argb-pixels 1 1 100 100 (get-2d (list ) '(0 0) empty-sceen0 100 100 10))
(send empty-bitmap1 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(1.3 0) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(0 0) empty-sceen1 100 100 10))
(send empty-bitmap2 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(1.6 0) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(0 0) empty-sceen2 100 100 10))
(send empty-bitmap3 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(1 0) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(0 0) empty-sceen3 100 100 10))
;(send empty-bitmap4 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 -5) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(0 0) empty-sceen4 100 100 10))
;(send empty-bitmap5 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 20) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(0 0) empty-sceen5 100 100 10))
;(send empty-bitmap6 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(10 20) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(7 -10) empty-sceen6 100 100 10))
;(send empty-bitmap7 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(10 20) '((1 0)(0 1)) 9 9 (make-bytes 400 127)) (squar '(30 10) '((1 0)(0 1)) 9 9 (make-bytes 400 127))) '(0 0) empty-sceen7 100 100 10))
(values ;empty-bitmap0
empty-bitmap1
empty-bitmap2
empty-bitmap3
;empty-bitmap4
;empty-bitmap5
;empty-bitmap6
;empty-bitmap7
)))
|#

"3d"
(local(
(define empty-sceen0   (make-bytes (* 4 100 100) 0))
(define empty-sceen1   (make-bytes (* 4 100 100) 0))
(define empty-sceen2   (make-bytes (* 4 100 100) 0))
(define empty-sceen3   (make-bytes (* 4 100 100) 0))
(define empty-sceen4   (make-bytes (* 4 100 100) 0))
(define empty-sceen5   (make-bytes (* 4 100 100) 0))
(define empty-sceen6   (make-bytes (* 4 100 100) 0))
(define empty-sceen7   (make-bytes (* 4 100 100) 0))
(define empty-bitmap0 (make-object bitmap% 100 100))
(define empty-bitmap1 (make-object bitmap% 100 100))
(define empty-bitmap2 (make-object bitmap% 100 100))
(define empty-bitmap3 (make-object bitmap% 100 100))
(define empty-bitmap4 (make-object bitmap% 100 100))
(define empty-bitmap5 (make-object bitmap% 100 100))
(define empty-bitmap6 (make-object bitmap% 100 100))
(define empty-bitmap7 (make-object bitmap% 100 100))
)
(begin
(send empty-bitmap0 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 0 2) `((,(cos(/ pi 3)) 0 ,(sin(/ pi 3)))(0 1 0)) 29 29 (make-bytes 3600 127))
                                                              ) '(0 0 0) empty-sceen0 100 100 100))
(send empty-bitmap1 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 0 2) '((1 0 0)(0 1 0)) 29 29 (make-bytes 3600 127))
                                                              ) '(0 0 0) empty-sceen1 100 100 100))
(send empty-bitmap2 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 0 2) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4))) (0 1 0)) 29 29 (make-bytes 3600 127))
                                                              ) '(0 0 0) empty-sceen2 100 100 100))
(send empty-bitmap3 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 0 2) `((0 0 1)(0 1 0)) 29 29 (make-bytes 3600 127)) 
                                                              ) '(0 0 0) empty-sceen3 100 100 100))
;(send empty-bitmap4 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 0 2) `((,(cos(* pi 7/4)) ,(sin(* pi 7/4)) 0)(,(cos(/ pi 4)) ,(sin(/ pi 4)) 0)) 29 29 (make-bytes 3600 127))
 ;  ) '(0 0 0) empty-sceen4 100 100 1000))
#|(send empty-bitmap0 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(-40 -40 30) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(-50 -50 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 ,(cos(/ pi 4)) ,(sin(/ pi 4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen0 100 100 1000))
(send empty-bitmap1 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(-40 -40 31) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(-50 -50 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 ,(cos(/ pi 4)) ,(sin(/ pi 4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen1 100 100 1000))
(send empty-bitmap2 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(-40 -40 50) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(-50 -50 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 ,(cos(/ pi 4)) ,(sin(/ pi 4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen2 100 100 1000))
(send empty-bitmap3 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(-40 -40 71) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(-50 -50 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 ,(cos(/ pi 4)) ,(sin(/ pi 4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen3 100 100 1000))
(send empty-bitmap4 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(-40 -40 72) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(-50 -50 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 ,(cos(/ pi 4)) ,(sin(/ pi 4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen4 100 100 1000))|#
;(send empty-bitmap0 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(-0 -0 1) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
 ;                                                             (squar '(-0 -0 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 1 0)) 29 29 (make-bytes 3600 127))
  ;                                                            
   ;                                                           ) '(0 0 0) empty-sceen0 100 100 1000))
;(send empty-bitmap1 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(-0 -0 2) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
 ;                                                             (squar '(-0 -0 1) `((,(cos(/ pi 4)) 0 ,(sin(/ pi 4)))(0 1 0)) 29 29 (make-bytes 3600 127))
  ;                                                            
   ;                                                           ) '(0 0 0) empty-sceen1 100 100 1000))
;(send empty-bitmap2 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(0 0 25) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
 ;                                                             (squar '(0 0 25) `((,(cos(* pi -1/4)) 0 ,(sin(* pi -1/4)))(0 1 0)) 29 29 (make-bytes 3600 127))
  ;                                                            
   ;                                                           ) '(0 0 0) empty-sceen2 100 100 1000))
;(send empty-bitmap3 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(0 0 20) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
 ;                                                             (squar '(0 0 20) `((,(cos(* pi -1/4)) 0 ,(sin(* pi -1/4)))(0 1 0)) 29 29 (make-bytes 3600 127))
  ;                                                            
   ;                                                           ) '(0 0 0) empty-sceen3 100 100 1000))
  #|
(send empty-bitmap0 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(0 00 1) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(00 00 1) `((,(cos(* pi 1/4)) 0 ,(sin(* pi 1/4)))(0 ,(cos(* pi 1/4)) ,(sin(* pi 1/4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen0 100 100 1000))
(send empty-bitmap1 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(0 0 2) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(0 0 1) `((,(cos(* pi 1/4)) 0 ,(sin(* pi 1/4))) (0 ,(cos(* pi 1/4)) ,(sin(* pi 1/4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen1 100 100 1000))
(send empty-bitmap2 set-argb-pixels 1 1 100 100 (get-2d (list (squar `(0 0 1) `((-1 0 0) (0 -1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(0 0 1) `((,(cos(* pi 3/4)) 0 ,(sin(* pi 3/4)))(0 ,(cos(* pi 3/4)) ,(sin(* pi 3/4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen2 100 100 1000))
(send empty-bitmap3 set-argb-pixels 1 1 100 100 (get-2d (list (squar '(0 0 2) `((-1 0 0) (0 -1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(0 0 1) `((,(cos(* pi 3/4)) 0 ,(sin(* pi 3/4)))(0 ,(cos(* pi 3/4)) ,(sin(* pi 3/4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen3 100 100 1000))|#
(send empty-bitmap4 set-argb-pixels 1 1 100 100 (get-2d (list ;(squar `(20 20 1) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(0 0 1 1) `((,(cos(* pi 3/4)) 0 0 ,(sin(* pi 3/4))) (0 ,(cos(* pi 3/4)) ,(sin(* pi 3/4)) 0)) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0 0)  empty-sceen4 100 100 1000))
(send empty-bitmap5 set-argb-pixels 1 1 100 100 (get-2d (list ;(squar `(20 20 1) `((1 0 0) (0 1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(20 20 1 1) `((,(cos(* pi 1)) 0 ,(sin(* pi 1)) 0) (0 ,(cos(* pi 1)) ,(sin(* pi 1)) 0)) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0 0) empty-sceen5 100 100 1000))
(send empty-bitmap6 set-argb-pixels 1 1 100 100 (get-2d (list ;(squar `(20 20 1) `((-1 0 0) (0 -1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(20 20 1) `((,(cos(* pi 1)) 0 ,(sin(* pi 1))) (0 ,(cos(* pi 1/4)) ,(sin(* pi 1/4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen6 100 100 1000))
(send empty-bitmap7 set-argb-pixels 1 1 100 100 (get-2d (list ;(squar '(20 20 1) `((-1 0 0) (0 -1 0)) 29 29 (make-bytes 3600 50))
                                                              (squar '(20 20 1) `((,(cos(* pi 7/4)) 0 ,(sin(* pi 7/4)))(0 ,(cos(* pi 7/4)) ,(sin(* pi 7/4)))) 29 29 (make-bytes 3600 127))
                                                              
                                                              ) '(0 0 0) empty-sceen7 100 100 1000))
(values 
empty-bitmap0
empty-bitmap1
empty-bitmap2
empty-bitmap3
empty-bitmap4
empty-bitmap5
empty-bitmap6
empty-bitmap7
)))

