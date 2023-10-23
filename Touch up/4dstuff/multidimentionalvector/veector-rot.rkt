#lang racket
(require math/matrix)
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
  (let* (;(e (print p))
         (num-dim (length cam-globe))
         (to-line (append (make-list (sub1 num-dim) 0) (list (* -1 c))))
         ;(F (print num-dim))
         (vec (vec- to-line p))
         ;(e1 (print vec))
         (n (/ (* -1 (first (reverse p))) (first (reverse vec))))
         ;(e2 (print n))
         (proj (vec+ p (vec-scale vec n)))
         ;(e3 (print proj))
         )
    (take proj (sub1 num-dim))))


(define (progect vec dist cam with high)
  (local ((define (d=2 vec cam)
             (if (= 2 (length vec)) vec (d=2 (dim-1 cam vec dist) (reverse (rest (reverse cam)))))))
          (d=2 vec cam)
    ))

;(progect '(1 1 2000) -5 '(0 0 0) 50 50)
(progect '(1 1 20 2) -50 '(0 0 0 0) 50 50)


(define (dim-1-point cam-globe p to-line)
  (let* ((num-dim (length cam-globe))
         (vec (vec- to-line p))
         (n (/ (* -1 (first (reverse p))) (first (reverse vec))))
         (proj (vec+ p (vec-scale vec n))))
    (take proj (sub1 num-dim))))

(define (linne cam p1 p2 dist with high)
  (local (;(define A (begin (print cam) (newline)))
          (define (d=2 vec svec cam)
             (cond
               ((= 2 (length vec)) (values vec svec))
               ((and (negative? (first (reverse vec))) (negative? (first (reverse svec)))) (values (list with high) (list with high)))
               ((negative? (first (reverse vec))) (d=2 (dim-1-point cam vec svec) (dim-1 cam svec dist) (reverse (rest (reverse cam)))))
               ((negative? (first (reverse svec))) (d=2 (dim-1 cam vec dist) (dim-1-point cam svec vec) (reverse (rest (reverse cam)))))
               (else (d=2 (dim-1 cam vec dist) (dim-1 cam svec dist)  (reverse (rest (reverse cam)))))))
     (define-values (x y) (d=2 p1 p2 cam)))
    (values (map + (list (/ with 2) (/ high 2)) x) (map + (list (/ with 2) (/ high 2)) y))))
     




;################################

;front dir - first alodir 
(require 2htdp/image)
(require 2htdp/universe)
(struct world(cam obj axis)#:transparent)
(struct obj (c alop alodir vel agop)#:transparent)

(define (acc ob dir am)
  (obj (obj-c ob) (obj-alop ob) (obj-alodir ob) (vec+ (obj-vel ob) (vec-scale dir am)) (obj-agop ob)))

(define (acc-w wor ref der am bool)
  (let* ((ob (if (= ref 0) (world-cam wor) (first (drop (world-obj wor) (sub1 ref)))))
        (after (if (= ref 0) (world-obj wor) (rest (drop (world-obj wor) (sub1 ref)))))
        (befor (if (= ref 0) #f (take (world-obj wor) (sub1 ref))))
        (dir (list-ref (if bool (obj-alodir ob) (cords (length (obj-c ob)))) der)))
    (if (= ref 0) (world (acc ob dir am) after (world-axis wor)) (world (world-cam wor) (append befor (cons (acc ob dir am) after)) (world-axis wor))))) 
;(rot 0 2 0 1)
;(rot 0 3 0 1)
;(rot 1 3 0 1)
;(rot 2 3 0 1)
;(rot 0 4 0 1)
;(rot 1 4 0 1)
;(rot 2 4 0 1)
;(rot 3 4 0 1)
;(rot 4 4 0 1)
;(rot 5 4 0 1)

(define (cords d)
  (build-list d (λ (x) (build-list d (λ (y) (if (= x y) 1 0))))))
;(cords 3)

(define (rotate-p ob o deg rel ax)
 (cond
   ((= 0 rel)
    (local ((define l (length (obj-c ob)))
          (define-values (n m) (rot o l 0 1))
          (define or1 (list-ref (cords l) n))
          (define or2 (list-ref (cords l) m)))
  (obj (obj-c ob) 
       (map (λ (x) (vec+ (obj-c ob) (rotat (vec- x (obj-c ob)) or1 or2 deg o))) (obj-alop ob))
       (map (λ (x) (rotat x or1 or2 deg o)) (obj-alodir ob))
       (obj-vel ob)
       (obj-agop ob))))
  
   ((= 1 rel)
  (obj (obj-c ob) 
       (map (λ (x) (vec+ (obj-c ob) (sub-rotat (vec- x (obj-c ob)) ax deg o))) (obj-alop ob))
       (map (λ (x) (sub-rotat x ax deg o)) (obj-alodir ob))
       (obj-vel ob)
       (obj-agop ob)))
   
   ((= 2 rel)
    (obj (obj-c ob) 
       (map (λ (x) (vec+ (obj-c ob) (sub-rotat (vec- x (obj-c ob)) (obj-alodir ob) deg o))) (obj-alop ob))
       (map (λ (x) (sub-rotat x (obj-alodir ob) deg o)) (obj-alodir ob))
       (obj-vel ob)
       (obj-agop ob)))
  ))
   

;(rotate-p (obj '(0 0) '((1 1)) '((1 0) (0 1)) '(1 0) '(0 0)) 0 90 #f)
;(rotate-p (obj '(0 0 0) '((1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0)) 0 180 #f)
#|
(rotate-p (obj '(0 0 0) '((1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
(rotate-p (obj '(0 0 0) '((1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
(rotate-p (obj '(0 0 0) '((1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
""
(rotate-p (obj '(0 0 0) '((0 1 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
(rotate-p (obj '(0 0 0) '((0 1 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
(rotate-p (obj '(0 0 0) '((0 1 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
""
(rotate-p (obj '(0 0 0) '((-1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
(rotate-p (obj '(0 0 0) '((-1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
(rotate-p (obj '(0 0 0) '((-1 0 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
""
(rotate-p (obj '(0 0 0) '((0 -1 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
(rotate-p (obj '(0 0 0) '((0 -1 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
(rotate-p (obj '(0 0 0) '((0 -1 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
""
(rotate-p (obj '(0 0 0) '((0 0 -1)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
(rotate-p (obj '(0 0 0) '((0 0 -1)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
(rotate-p (obj '(0 0 0) '((0 0 -1)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
|#
;(rotate-p (obj '(0 0 0) '((10 10 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
;(rotate-p (obj '(0 0 0) '((10 10 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
;(rotate-p (obj '(0 0 0) '((10 10 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
;""
;(rotate-p (obj '(0 0 0) '((10 10 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 0 90 #f)
;(rotate-p (obj '(0 0 0) '((10 10 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 1 90 #f)
;(rotate-p (obj '(0 0 0) '((10 10 0)) '((1 0 0)(0 1 0)(0 0 1)) '(1 0 0) '(0 0 0) null) 2 90 #f)
;""
;(rotate-p (obj '(0 0 0) '((10 10)) '((1 0)(0 1)) '(1 0) '(0 0) null) 0 90 #f)
;(rotate-p (obj '(0 0 0) '((-10 10)) '((1 0)(0 1)) '(1 0) '(0 0) null) 0 90 #f)
;(rotate-p (obj '(0 0 0) '((-10 -10)) '((1 0)(0 1)) '(1 0) '(0 0) null) 0 90 #f)
;(rotate-p (obj '(0 0 0) '((10 -10)) '((1 0)(0 1)) '(1 0) '(0 0) null) 0 90 #f)

(define (rotate-cam wor o deg rel)
  (let*((cc (obj-c (world-cam wor)))
       (cam (world-cam wor)))
  (cond 
    ((= 0 rel)
  (world 
   (obj (obj-c cam) 
       (map (λ (x) (vec+ (obj-c cam) (sub-rotat (vec- x (obj-c cam)) (cords (length cc)) deg o))) (obj-alop cam))
       (map (λ (x) (sub-rotat x (cords (length cc)) deg o)) (obj-alodir cam))
       (sub-rotat (obj-vel cam) (cords (length cc)) deg o)
       (obj-agop cam))
   
       (map (λ (y) 
                (obj 
       (vec+ cc (sub-rotat (vec- (obj-c y) cc) (cords (length cc)) deg o))
       (map (λ (x) (vec+ cc (sub-rotat (vec- x cc) (cords (length cc)) deg o))) (obj-alop y))
       (map (λ (x) (sub-rotat x (cords (length cc)) deg o)) (obj-alodir y))
       (sub-rotat (obj-vel y) (cords (length cc)) deg o)
       (obj-agop y))) (world-obj wor))
       (map (λ (x) (sub-rotat x (cords (length x)) deg o)) (world-axis wor))))
    
    ((= 1 rel)
  (world 
   (obj (obj-c cam)
       (map (λ (x) (vec+ (obj-c cam) (sub-rotat (vec- x (obj-c cam)) (world-axis wor) deg o))) (obj-alop cam))
       (map (λ (x) (sub-rotat x (world-axis wor) deg o)) (obj-alodir cam))
       (sub-rotat (obj-vel cam) (world-axis wor) deg o)
       (obj-agop cam))
   
       (map (λ (y) 
                (obj 
       (vec+ cc (sub-rotat (vec- (obj-c y) cc) (world-axis wor) deg o))
       (map (λ (x) (vec+ cc (sub-rotat (vec- x cc) (world-axis wor) deg o))) (obj-alop y))
       (map (λ (x) (sub-rotat x (world-axis wor) deg o)) (obj-alodir y))
       (sub-rotat (obj-vel y) (world-axis wor) deg o)
       (obj-agop y))) (world-obj wor))
       (map (λ (x) (sub-rotat x (world-axis wor) deg o)) (world-axis wor))))
    
    ((= 2 rel)
  (world 
   (obj (obj-c cam)
       (map (λ (x) (vec+ (obj-c cam) (sub-rotat (vec- x (obj-c cam)) (obj-alodir cam) deg o))) (obj-alop cam))
       (map (λ (x) (sub-rotat x (obj-alodir cam) deg o)) (obj-alodir cam))
       (sub-rotat (obj-vel cam) (obj-alodir cam) deg o)
       (obj-agop cam))
   
       (map (λ (y) 
                (obj 
       (vec+ cc (sub-rotat (vec- (obj-c y) cc) (obj-alodir cam) deg o))
       (map (λ (x) (vec+ cc (sub-rotat (vec- x cc) (obj-alodir cam) deg o))) (obj-alop y))
       (map (λ (x) (sub-rotat x (obj-alodir cam) deg o)) (obj-alodir y))
       (sub-rotat (obj-vel y) (obj-alodir cam) deg o)
       (obj-agop y))) (world-obj wor))
       (map (λ (x) (sub-rotat x (obj-alodir cam) deg o)) (world-axis wor))))
     )))



(define (rotate-w wor x ref bool deg)
  (if (= ref 0) (rotate-cam wor x (* -1 deg) bool)
    (world (world-cam wor) (append (take (world-obj wor) (sub1 ref)) (cons (rotate-p (first (drop (world-obj wor) (sub1 ref))) x deg bool (world-axis wor)) (rest (drop (world-obj wor) (sub1 ref))))) (world-axis wor))
    ))

(define (change-o ob)
  (obj (map + (obj-c ob) (obj-vel ob)) (map (λ (x) (map + x (obj-vel ob))) (obj-alop ob)) (obj-alodir ob) (obj-vel ob) (obj-agop ob)))

(define (change-w wor)
  (world (change-o (world-cam wor)) (map change-o (world-obj wor)) (world-axis wor)))

(define ref 0)
(define rel 0)
(define (move wor str)
  (cond
    ((equal? str "r") (begin (set! rel (remainder (add1 rel)3)) wor))
    ((equal? str "t") (begin (set! ref (remainder (add1 ref) (add1 (length (world-obj wor))))) wor))
    
    ((equal? str "z") (rotate-w wor 0 ref rel 10))
    ((equal? str "x") (rotate-w wor 1 ref rel 10))
    ((equal? str "c") (rotate-w wor 2 ref rel 10))
    ((equal? str "v") (rotate-w wor 3 ref rel 10))
    ((equal? str "b") (rotate-w wor 4 ref rel 10))
    ((equal? str "n") (rotate-w wor 5 ref rel 10))
    
    ((equal? str "f") (rotate-w wor 0 ref rel -10))
    ((equal? str "g") (rotate-w wor 1 ref rel -10))
    ((equal? str "h") (rotate-w wor 2 ref rel -10))
    ((equal? str "j") (rotate-w wor 3 ref rel -10))
    ((equal? str "k") (rotate-w wor 4 ref rel -10))
    ((equal? str "l") (rotate-w wor 5 ref rel -10))
    
    ((equal? str "d") (acc-w wor ref 0 1 rel))
    ((equal? str "s") (acc-w wor ref 1 1 rel))
    ((equal? str "e") (acc-w wor ref 2 1 rel))
    ((equal? str "1") (acc-w wor ref 3 1 rel))
    ((equal? str "a") (acc-w wor ref 0 -1 rel))
    ((equal? str "w") (acc-w wor ref 1 -1 rel))
    ((equal? str "q") (acc-w wor ref 2 -1 rel))
    ((equal? str "3") (acc-w wor ref 3 -1 rel))
    (else wor)
    ))

;wor->i
(define (disp-line wor)
  (foldr (λ (a b) 
           (let ((p (obj-c (world-cam wor)))
                 (cam (world-cam wor))
                 (alop (obj-alop a)))
            
             (foldr (λ (f ap e)
                      (foldr (λ (g h)
                               (local ((define-values (p l) (linne (obj-c cam) (vec- (list-ref alop g) (obj-c cam))  (vec- ap (obj-c cam)) 100 500 500))
                                       )
                                 (scene+line h
                                             (first p)
                                             (second p)
                                             (first l)
                                             (second l)
                                             'black
                                           )))
                             e
                             f
                             ))
                    b
                    (obj-agop a)
                    (obj-alop a))
             )
           )
         (empty-scene 500 500)
         (world-obj wor)))

;(struct obj (c alop alodir start-front vel agop)#:transparent)

(big-bang (world (obj '(0 0 -2000 -2000) '() '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))  '(0 0 0 0) '())
           (list(obj '(0 0 550 550)'((2000 2000 2550 2550)
                                 (-2000 2000 2550 2550)
                                 (2000 -2000 2550 2550)
                                 (-2000 -2000 2550 2550)
                                 (2000 2000 -1450 2550)
                                 (-2000 2000 -1450 2550)
                                 (2000 -2000 -1450 2550)
                                 (-2000 -2000 -1450 2550)
                                 (2000 2000 2550 -1450)
                                 (-2000 2000 2550 -1450)
                                 (2000 -2000 2550 -1450)
                                 (-2000 -2000 2550 -1450)
                                 (2000 2000 -1450 -1450)
                                 (-2000 2000 -1450 -1450)
                                 (2000 -2000 -1450 -1450)
                                 (-2000 -2000 -1450 -1450)
                                 
  
                                 )
                      '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))
                       '(0 0 0 0)
                               '((1 2 4 8)
                                 (0 3 5 9)
                                 (6 3 0 10)
                                 (7 2 1 11)
                                 (0 5 6 12)
                                 (1 4 7 13)
                                 (2 7 4 14)
                                 (3 5 6 15)
                                 
                                 (9 10 12 0)
                                 (8 11 13 1)
                                 (14 11 8 2)
                                 (15 10 9 3)
                                 (8 13 14 4)
                                 (9 12 15 5)
                                 (10 15 12 6)
                                 (11 13 14 7)
                                 ))
                                 )
                
           '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1)))
          (to-draw disp-line)
          (on-key move)
          (on-tick change-w))
#|
(big-bang (world (obj '(0 0 -2000 -2000) '() '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))  '(0 0 0 0) '())
           (list(obj '(0 0 550 2000)'((2000 2000 2550 2550)
                                 (-2000 2000 2550 2550)
                                 (2000 -2000 2550 2550)
                                 (-2000 -2000 2550 2550)
                                 (2000 2000 -1450 2550)
                                 (-2000 2000 -1450 2550)
                                 (2000 -2000 -1450 2550)
                                 (-2000 -2000 -1450 2550)
                                 
                                 
  
                                 )
                      '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))
                       '(0 0 0 0)
                               '((1 2 4)
                                 (0 3 5)
                                 (6 3 0)
                                 (7 2 1)
                                 (0 5 6)
                                 (1 4 7)
                                 (2 7 4)
                                 (3 5 6)
                                 ;(9 10 11)
                                 ;(8)
                                 ;(8)
                                 ;(8)
                                 ))
                                 )
                
           '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1)))
          (to-draw disp-line)
          (on-key move)
          (on-tick change-w))
|#
#|
(big-bang (world (obj '(0 0 0) '() '((1 0 0)(0 1 0)(0 0 1)) '(0 0 0) '())
           (list(obj '(0 0 350) '((100 100 450)
                                 (-100 100 450)
                                 (100 -100 450)
                                 (-100 -100 450)
                                 (100 100 250)
                                 (-100 100 250)
                                 (100 -100 250)
                                 (-100 -100 250)
                                 ;(0 0 250)
                                 ;(50 0 250)
                                 ;(0 50 250)
                                 ;(0 0 350)
                                 )
                      '((1 0 0)(0 1 0)(0 0 1))
                      '(0 0 0)
                               '((1 2 4)
                                 (0 3 5)
                                 (6 3 0)
                                 (7 2 1)
                                 (0 5 6)
                                 (1 4 7)
                                 (2 7 4)
                                 (3 5 6)
                                 ;(9 10 11)
                                 ;(8)
                                 ;(8)
                                 ;(8)
                                 )))
           '((1 0 0)(0 1 0)(0 0 1)))
          (to-draw disp-line)
          (on-key move)
          (on-tick change-w))
|#



#|
(big-bang (world (obj '(0 0) '() '((1 0)(0 1)) '(0 0) '())
           (list(obj '(0 0)    '((200 200)
                                 (-200 200)
                                 (200 -200)
                                 (-200 -200)
                                 )
                      '((1 0)(0 1))
                      '(0 0)
                               '((1 2)
                                 (0 3)
                                 (0 3)
                                 (2 1)
                                 )))
           '((1 0)(0 1)))
          (to-draw disp-line)
          (on-key move)
          (on-tick change-w))
|#