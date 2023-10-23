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

(define (mat->list l alon)
  (if (null? alon) null (cons (take alon l) (mat->list l (drop alon l)))))

(define (find-line-axis vec alodir)
  (foldr (λ (x y z) (vec+ (vec-scale x y) z)) (make-list (length vec) 0) 
    (mat->list (length alodir) 
       (matrix->list  (matrix-inverse (list->matrix (length alodir) (length alodir) (apply append alodir))))) vec))

(define (find-line-axis-inv vec alodir)
  (foldr (λ (x y z) (vec+ (vec-scale x y) z)) (make-list (length vec) 0) alodir vec))

(define (dim-1 p cam dist axis)
  (let* ((num-dim (length cam))
        (p-vec (find-line-axis p axis))
        (to-line (append (make-list (sub1 num-dim) 0) (list (* -1 dist))))
        (vec (vec- to-line p-vec))
        (n (/ (* -1 (first (reverse p-vec))) (first (reverse vec))))
        (proj (vec+ p (find-line-axis-inv (vec-scale vec n) axis))))
    (take proj (sub1 num-dim))))
        

(define (pro p cam dist axis)
    (cond
      ((= 2 (length cam)) p)
      ((negative? (first (reverse p))) (list +inf.0 +inf.0))
      (else (pro (dim-1 p cam dist axis) (take cam (sub1 (length cam))) dist axis))))

(define (progect p cam dist axis)
  (pro (vec- p cam) cam dist axis))

(define (move-pixl fbstr fn/4 tbstr tn/4)
  (let ((fn (inexact->exact (* fn/4 4)))(tn (inexact->exact (* tn/4 4))))
  (begin 
    (bytes-set! tbstr tn (bytes-ref fbstr fn))
    (bytes-set! tbstr (+ 1 tn) (bytes-ref fbstr (+ 1 fn)))
    (bytes-set! tbstr (+ 2 tn)  (bytes-ref fbstr (+ 2 fn)))
    (bytes-set! tbstr (+ 3 tn)  (bytes-ref fbstr (+ 3 fn)))
    tbstr)))

(define (xy->pn x y width high)
  (+ x (* y width)))

;place-pixle: stp dirpoint numbertimesmoved cam texture tbytestring dist axis
(define (place-pixle stp dpr dpd ntm cam tex fh fw tbstr th tw dist axis)
  (let ((worldxy (progect (vec+ stp (vec+ (vec-scale dpr (quotient ntm tw)) 
                                           (vec-scale dpd (remainder ntm tw))))
                           cam dist axis))
        )
    (if (or (>= (first worldxy) tw) (>= (second worldxy) th)) tbstr
  (move-pixl tex ntm tbstr (xy->pn (first worldxy) (second worldxy) tw tw)))))

(place-pixle '(0 0) '(0 1) '(1 0) 0 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(0 0) '(0 1) '(1 0) 1 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(1 1) '(0 1) '(1 0) 0 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(1 0) '(0 1) '(1 0) 0 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(1 0) '(0 1) '(1 0) 1 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(1 0) '(0 1) '(1 0) 2 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(0 1) '(0 1) '(1 0) 0 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(place-pixle '(0 1) '(0 1) '(1 0) 1 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                           (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
;place-pixle: stp dirpoint numbertimesmoved maxnumbertimes moved number of times cam texture tbytestring dist axis
(define (add-texture stp dpr dpd ntm ntmmax cam tex fh fw tbstr th tw dist axis)
  (if (= ntmmax ntm) tbstr
      (add-texture stp dpr dpd (add1 ntm) ntmmax cam tex fh fw
                   (place-pixle stp dpr dpd ntm cam tex fh fw tbstr th tw dist axis)
                   th tw dist axis)))
(add-texture '(0 0) '(0 1) '(1 0) 0 4 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(add-texture '(1 1) '(0 1) '(1 0) 0 4 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(add-texture '(1 0) '(0 1) '(1 0) 0 4 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(add-texture '(0 1) '(0 1) '(1 0) 0 4 '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))

(define (4ptextdisp p1 p2 p3 p4 cam tex text-width text-hight image image-width image-hight dist axis)
    (add-texture p1 (vec- p2 p1) (vec- p3 p1) 0 (/ (bytes-length tex) 4)
        cam tex text-hight text-width image image-hight image-width dist axis))
(4ptextdisp '(0 0) '(0 1) '(1 0) '(1 1)  '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(4ptextdisp '(1 0) '(1 1) '(2 0) '(2 1)  '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(4ptextdisp '(0 1) '(0 2) '(1 1) '(1 2)  '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(4ptextdisp '(1 1) '(1 2) '(2 1) '(2 2)  '(0 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(4ptextdisp '(1 1) '(1 2) '(2 1) '(2 2)  '(1 1) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
(4ptextdisp '(1 1) '(1 2) '(2 1) '(2 2)  '(1 0) (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 2 2
                                             (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2 2
                                           1 '((1 0)(0 1)))
