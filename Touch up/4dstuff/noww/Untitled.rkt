;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;#lang racket
(require math/matrix)
(require racket)

;vector+
(define (vec+ v ve)
  (map + v ve))

;vector-
(define (vec- v ve)
  (map - v ve))

;vector-scale
(define (vec-scale ve n)
  (map (λ (x) (* n x)) ve))

;circ n avov -> v
(define (circ d n avov ac)
  (if (= 0 n) null
      (cons (vec-scale (vec- (vector-ref avov (remainder (add1 ac)4)) (vector-ref avov (remainder ac 4))) (if (= 1 n) (/ d 90) 1))
            (circ d (sub1 n) avov (add1 ac)))))
;(circ 90 1 (vector '(1 0)'(0 1)'(-1 0)'(0 -1))0)
;(circ 180 2 (vector '(1 0)'(0 1)'(-1 0)'(0 -1))0)
(define (vec-l vec)
  (sqrt (foldr (λ (x y) (+ (sqr x) y)) 0 vec)))

(define (norm vec n)
  (let ((l (vec-l vec)))
    (map (λ (x) (* n (/ x l))) vec)))
;(norm '(1 1) 1)
;(norm '(0 0) 1) error

;circ-vec: d vec vec
(define (circ-vec d vec1 vec2)
  ;(if (positive? d)
   ;   (begin (print d)
   ; (apply map + (circ (remainder d 90) (add1 (quotient (* -1 d) 90))
     ;    (vector vec1 (map (λ (x) (* -1 x)) vec2) (map (λ (x) (* -1 x)) vec1) vec2 )0)))
    (apply map + (circ (remainder d 90) (add1 (quotient d 90))
         (vector vec1 vec2 (map (λ (x) (* -1 x)) vec1) (map (λ (x) (* -1 x)) vec2)) 0)))
;(circ-vec 0  (list 1 0) (list 0 1))
;(circ-vec 90  (list 1 0) (list 0 1))
;(circ-vec 180 (list 1 0) (list 0 1))
;(circ-vec 270  (list 1 0) (list 0 1))
;(circ-vec 360 (list 1 0) (list 0 1))
;(circ-vec 90  (list 2 0) (list 0 2))
;(circ-vec 45  (list 2 0) (list 0 2))
;(circ-vec 180 (list 2 0) (list 0 2))
;(circ-vec 0  (list 1 0) (list 0 1))
;(circ-vec -90  (list 1 0) (list 0 1))
;(circ-vec -180 (list 1 0) (list 0 1))
;(circ-vec -270  (list 1 0) (list 0 1))
;(circ-vec -360 (list 1 0) (list 0 1))

(define (perp-line vec)
  (append (reverse (map * (take vec 2) '(1 -1)))
    (build-list (- (length vec)2) (λ (x) 0))))
;(perp-line '(1 1))
;(perp-line '(0 1))
;(perp-line '(1 0))
;(perp-line '(-1 -1))
;(perp-line '(-1 -1 1))
(define (rot x d acc ac)
  (cond
    ((= x 0) (values acc ac))
    ((= (sub1 d) ac) (rot (sub1 x) d (add1 acc) (+ 2 acc)))
    (else (rot (sub1 x) d acc (add1 ac)))))

(define (dot vec v)
  (apply + (apply append (map (λ (x) (map (λ (y) (* x y)) v))vec))))
;(dot '(3 3 2) '(2 2))
(define (mat-inv mat)
  (local (
        (define a (first (first mat)))
        (define b (second (first mat)))
        (define c (first (second mat)))
        (define d (second (second mat)))
        (define det (- (* a d) (* c b))))
    (if (zero? det) '((0 0)(0 0))
    (list (list (* (/ 1 det) d) (* -1 (/ 1 det) b))
          (list (* -1 (/ 1 det) c) (* (/ 1 det) a))))))
;(mat-inv (list (list 4 7)  (list 2 6)))

(define (select vec n m)
    (list (list-ref vec n) (list-ref vec m)))
;(select '(1 2 3)
;        '(1 2 3) 0)
;(select '(1 2 3)
;        '(1 2 3) 1)
;(select '(1 2 3)
;        '(1 2 3) 2)

;finds a ling progected onto a playn between or1 or2
(define (find-line-plane vec or1 or2 n m)
  (local (
          (define S (list (select or1 n m) (select or2 n m)))
          (define D (select vec n m))
          (define B (mat-inv S))
          (define V (vec+ (vec-scale (first B) (first D)) (vec-scale (second B) (second D))))
          (define V1(perp-line V))
          ;(define a (begin (print S) (newline) (print n) (newline) (print m) (newline)
           ;                (print or1) (newline) (print or2) (newline) (print B) (newline)
            ;               ))
          )
    (values 
    V
    V1
    )))
#|
""
(find-line-plane '(1 0 0) '(1 0 0) '(0 0 1) 0)
(find-line-plane '(0 1 0) '(1 0 0) '(0 0 1) 0)
(find-line-plane '(0 0 1) '(1 0 0) '(0 0 1) 0)
""
(find-line-plane '(1 0 0) '(1 0 0) '(0 0 1) 1)
(find-line-plane '(0 1 0) '(1 0 0) '(0 0 1) 1)
(find-line-plane '(0 0 1) '(1 0 0) '(0 0 1) 1)
""
(find-line-plane '(1 0 0) '(1 0 0) '(0 0 1) 2)
(find-line-plane '(0 1 0) '(1 0 0) '(0 0 1) 2)
(find-line-plane '(0 0 1) '(1 0 0) '(0 0 1) 2)
""
|#
#|
(find-line-plane '(1 0 0) '(0 1 0) '(0 0 1) 0)
(find-line-plane '(0 1 0) '(0 1 0) '(0 0 1) 0)
(find-line-plane '(0 0 1) '(0 1 0) '(0 0 1) 0)
""
(find-line-plane '(1 0 0) '(0 1 0) '(0 0 1) 1)
(find-line-plane '(0 1 0) '(0 1 0) '(0 0 1) 1)
(find-line-plane '(0 0 1) '(0 1 0) '(0 0 1) 1)
""
(find-line-plane '(1 0 0) '(0 1 0) '(0 0 1) 2)
(find-line-plane '(0 1 0) '(0 1 0) '(0 0 1) 2)
(find-line-plane '(0 0 1) '(0 1 0) '(0 0 1) 2)
""
|#
;(find-line-plane '(0 1 0) '(0 1 0) '(0 0 1) 2)
;(find-line-plane '(1 0 0) '(0 1 0) '(0 0 1) 2)
;(find-line-plane '(1 1) '(1 0) '(0 1) 0)
;(find-line-plane '(1 0) '(1 1) '(-1 1) 0)
;(find-line-plane '(1 1) '(1 1) '(-1 1) 0)
;(find-line-plane '(-2 2) '(1 1) '(-2 2) 0)
;(find-line-plane '(2 2) '(1 1) '(-2 2) 0)
(define (find-line vec or1 or2 n m)
  (local ((define B (list (select or1 n m) (select or2 n m)))
          (define V (vec+ (vec-scale (first B) (first vec)) (vec-scale (second B) (second vec))))
          )
    V))


(define (rotat vec or1 or2 deg x)
  (local((define-values (n m) (rot x (length or1) 0 1))
        (define-values (2d p2d) (find-line-plane vec or1 or2 n m)))
    (if (foldr (λ (x y) (and (zero? x) y)) #t 2d) vec
        (local(
        (define l2 (vec-l 2d))
        ;(define l (vec-l vec))
        (define cir (find-line (norm (vec+ 2d (circ-vec deg 2d p2d)) l2) or1 or2 n m)))
  (build-list (length vec) (λ (x) (cond
                                    ((= x m) (second cir))
                                    ((= x n) (first cir))
                                    (else (list-ref vec x)))))))))
    
   
;(rotat '(0 -1 0) '(1 0 0) '(0 1 0) 90 0)
#|
(rotat '(0 1 0) '(1 0 0) '(0 0 1) 90 1)
(rotat '(0 0 1) '(1 0 0) '(0 0 1) 90 1)
(rotat '(1 0 0) '(1 0 0) '(0 0 1) 90 1)
(rotat '(1 0 0) '(1 0 0) '(0 1 0) 90 0)
(rotat '(0 1 0) '(1 0 0) '(0 1 0) 90 0)
(rotat '(0 0 1) '(1 0 0) '(0 1 0) 90 0)
(rotat '(0 1 0) '(0 1 0) '(0 0 1) 90 2)
(rotat '(0 0 1) '(0 1 0) '(0 0 1) 90 2)
(rotat '(1 1 0) '(0 1 0) '(0 0 1) 90 2)
|#
(define (mat->list l alon)
  (if (null? alon) null (cons (take alon l) (mat->list l (drop alon l)))))
(define (find-line-axis vec alodir)
  (foldr (λ (x y z) (vec+ (vec-scale x y) z)) (make-list (length vec) 0) (mat->list (length alodir) (matrix->list  (matrix-inverse (list->matrix (length alodir) (length alodir) (apply append alodir))))) vec))

(define (find-line-axis-inv vec alodir)
  (foldr (λ (x y z) (vec+ (vec-scale x y) z)) (make-list (length vec) 0) alodir vec))


(define (sub-rotat vec alodir deg x)
  (local((define-values (n m) (rot x (length vec) 0 1))
         (define in-ax (find-line-axis vec alodir))
         (define 2d (select in-ax n m)))
    (if (foldr (λ (x y) (and (zero? x) y)) #t 2d) vec
        (local(
         (define p2d (perp-line 2d))
         (define 2d-circ (norm (vec+ 2d (circ-vec deg 2d p2d)) (vec-l 2d)))
         (define nd-circ (build-list (length vec) (λ (x) (cond
                                    ((= x m) (second 2d-circ))
                                    ((= x n) (first 2d-circ))
                                    (else (list-ref in-ax x))))))
         )
    (find-line-axis-inv nd-circ alodir)))))

(define (vector-rotate vec axis new-dir)
  vec)

;alon alosn alon n
(define (dim-1 cam-globe p c)
  (let* ((num-dim (length cam-globe))
         (to-line (append (make-list (sub1 num-dim) 0) (list (* -1 c))))
         (vec (vec- to-line p))
         (n (/ (* -1 (first (reverse p))) (first (reverse vec))))
         (proj (vec+ p (vec-scale vec n))))
    (take proj (sub1 num-dim))))
;(define (progect vec dist cam with high)
 ; (local ((define (d=2 vec cam)
  ;           (if (= 2 (length vec)) vec (d=2 (dim-1 cam vec dist) (reverse (rest (reverse cam)))))))
   ;       (map + (list (/ with 2) (/ high 2)) (d=2 vec cam))
    ;))

(define (dim-1-point cam-globe p to-line)
  (let* ((num-dim (length cam-globe))
         (vec (vec- to-line p))
         (n (/ (* -1 (first (reverse p))) (first (reverse vec))))
         (proj (vec+ p (vec-scale vec n))))
    (take proj (sub1 num-dim))))

(define (linne cam p1 p2 dist with high)
  (local ((define (d=2 vec svec cam)
             (cond
               ((= 2 (length vec)) (values vec svec))
               ((and (negative? (first (reverse vec))) (negative? (first (reverse svec)))) (values (list with high) (list with high)))
               ((negative? (first (reverse vec))) (d=2 (dim-1-point cam vec svec) (dim-1 cam svec dist) (reverse (rest (reverse cam)))))
               ((negative? (first (reverse svec))) (d=2 (dim-1 cam vec dist) (dim-1-point cam svec vec) (reverse (rest (reverse cam)))))
               (else (d=2 (dim-1 cam vec dist) (dim-1 cam svec dist)  (reverse (rest (reverse cam)))))))
     (define-values (x y) (d=2 p1 p2 cam)))
    (values (map + (list (/ with 2) (/ high 2)) x) (map + (list (/ with 2) (/ high 2)) y))))
     




;################################


(require 2htdp/universe)
(require 2htdp/image)

(define-struct world(cam alobj avop axis))
(define-struct obj  (c aloreface axis vel alocol))
(define-struct poly (c aloref color))
;(struct posn (x y)#:transparent)
(define (p- p p1)
  (make-posn (- (posn-x p)(posn-x p1)) (-(posn-y p)(posn-y p1))))
;polys in 2d already with extranius and cliping eliminated.
(define (proj alopoly avop)
  (foldr (λ (x y) (place-image (polygon (map (λ (z) (p- (vector-ref avop z)(poly-c x))) (poly-aloref x)) "solid" (poly-color x))
                               (posn-x (poly-c x))
                               (posn-y (poly-c x))
                               y)) (empty-scene 20 20) alopoly))

(proj (list (make-poly (make-posn 16 16) '(0 1 2) "red")) (vector (make-posn 30 10) (make-posn 10 10) (make-posn 10 30)))

;take 
(define (dim=1


