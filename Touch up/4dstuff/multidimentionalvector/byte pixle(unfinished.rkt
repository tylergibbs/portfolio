#lang racket
;vector-scale
(define (vec-scale ve n)
  (map (λ (x) (* n x)) ve))
;vector+
(define (vec+ v ve)
  (map + v ve))
;vector-
(define (vec- v ve)
  (map - v ve))
(define (dist vec ve)
  (sqrt (foldr (λ (x y z) (+ (sqr (- x y)) z)) 0 vec ve)))
(define (xyw->n x y w)
  (+ x (* y w)))
(define (vec-l vec)
  (sqrt (foldr (λ (x y) (+ (sqr x) y)) 0 vec)))
(define (norm vec n)
  (let ((l (vec-l vec)))
    (map (λ (x) (* n (/ x l))) vec)))


(define (move-pixl fbstr fn/4 tbstr tn/4)
  (let ((fn (inexact->exact (* fn/4 4)))(tn (inexact->exact (* tn/4 4))))
  (begin 
    (bytes-set! tbstr tn (bytes-ref fbstr fn))
    (bytes-set! tbstr (+ 1 tn) (bytes-ref fbstr (+ 1 fn)))
    (bytes-set! tbstr (+ 2 tn)  (bytes-ref fbstr (+ 2 fn)))
    (bytes-set! tbstr (+ 3 tn)  (bytes-ref fbstr (+ 3 fn)))
    tbstr)))

;(move-pixl #"APPL" 0 (bytes 0 0 0 0 0 0 0 0) 1)

(define (find-ref-xy x y width hight vec1 vec2 vec3 vec4)
  (let* ((v (list x y))
        (d1 (dist '(0 0) v))(d2 (dist (list width 0) v))(d3 (dist (list 0 hight) v))(d4 (dist (list width hight) v))
        )
    (local ((define (run alod alov)
              (cond
                ((null? alod) #f)
                ((= (first alod) 0) (first alov))
                (else (run (rest alod) (rest alov)))))
            ;(define b (begin (print d1) (newline) (print d2) (newline) (print d3) (newline) (print d4) (newline)))
            (define r (run (list d1 d2 d3 d4) (list vec1 vec2 vec3 vec4))))
      (if (not r)
        (let*
        ((ad1 (/ 1 d1)) (ad2 (/ 1 d2)) (ad3 (/ 1 d3)) (ad4 (/ 1 d4))
        (md (+ ad1 ad2 ad3 ad4))
        (w1 (/ ad1 md))(w2 (/ ad2 md))(w3 (/ ad3 md))(w4 (/ ad4 md))
        (ch-vec (vec+ (vec-scale vec1 w1) (vec+ (vec-scale vec2 w2) (vec+ (vec-scale vec3 w3) (vec-scale vec4 w4)))))
        (p (vec+ v ch-vec)))
          (map round p))
        (vec+ v r)))))

;(find-ref-xy 10 0 10 10 '(0 0) '(1 0) '(0 0) '(0 0))
;(find-ref-xy 5 5 10 10 '(10 10) '(10 10) '(10 10) '(10 10))
;(find-ref-xy 7 7 10 10 '(-10 10) '(10 -10) '(10 10) '(-10 -10))
;(find-ref-xy 7 7 10 10 '(10 -10) '(-10 10) '(-10 -10) '(10 10))
;(find-ref-xy 7 7 10 10 '(0 0) '(0 0) '(0 0) '(10 10))
;(find-ref-xy 7 7 10 10 '(10 0) '(10 0) '(10 0) '(0 10))

(define (set-pixle tbstr tn byte)
  (begin 
    (bytes-set! tbstr tn (bytes-ref byte 0))
    (bytes-set! tbstr (+ 1 tn) (bytes-ref byte 1))
    (bytes-set! tbstr (+ 2 tn) (bytes-ref byte 2))
    (bytes-set! tbstr (+ 3 tn) (bytes-ref byte 3))
    tbstr))


(define (xy-move tbstr fbstr tp fp width hight)
  (let ((fx (first fp))
        (fy (second fp))
        (fn (xyw->n (first fp) (second fp) width))
        (tn (xyw->n (first tp) (second tp) width)))
  (cond
    ((or (> -.1 fx) (< (sub1 width) fx) (> -.1 fy) (< (sub1 hight) fy)) (set-pixle tbstr tn (bytes 0 0 0 0)))
    (else (move-pixl fbstr fn tbstr tn)))))


;btstr bstr fp1-sp1 fp2-sp2(vectors 1 = 1pix)(fp1 (take 2 p))(sp1 (progect p))
(define (whxy fbstr tbstr vec1 vec2 vec3 vec4 x y width hight)
  (cond
    ((and (= x (sub1 width)) (= (sub1 hight) y)) (xy-move tbstr fbstr (list x y) (find-ref-xy x y width hight vec1 vec2 vec3 vec4) width hight))
    ((= x (sub1 width)) (whxy fbstr (xy-move tbstr fbstr (list x y) (find-ref-xy x y width hight vec1 vec2 vec3 vec4) width hight) vec1 vec2 vec3 vec4 0 (add1 y) width hight))
    (else (whxy fbstr (xy-move tbstr fbstr (list x y) (find-ref-xy x y width hight vec1 vec2 vec3 vec4) width hight) vec1 vec2 vec3 vec4 (add1 x) y width hight))))
(whxy (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 
      (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      '(1 0) '(1 0) '(1 0) '(1 0) 0 0 4 5)
(whxy (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 
      (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      '(-1 0) '(-1 0) '(-1 0) '(-1 0) 0 0 4 5)
(whxy (bytes 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) 
      (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      '(-1 1) '(-1 1) '(-1 1) '(-1 1) 0 0 4 5)


;(define (4p-distort fbm fp1 fp2 fp3 fp4 tp1 tp2 tp3 tp4 x y)
 ; (local (