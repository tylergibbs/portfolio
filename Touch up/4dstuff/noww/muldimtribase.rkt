#lang racket
(require math/matrix)


(struct tri (p1 p2 p3 btst)#:transparent)
;btst is asqaur with 1/2 clear, scaled baracentric cords will ref the btst (u,v)-> n, baracentric make into right triangle
(define-struct line (vec start) #:transparent)
(define-struct hold (dist pixl))

(define (vec- v vp)
  (map - v vp))
(define (vec+ v vp)
  (map + v vp))
(define (vec-scale v n)
  (map (λ (x) (* n x)) v))
(define (cartitian->barecentric tr bc)
  (list 
(define (barecentic->cartian tr bp)
  (vec+ (vec-scale (tri-p1 tr) (first bp)) (vec+ (vec-scale (tri-p2 tr) (second bp)) (vec-scale (tri-p3 tr) (third bp)))))
(define (barecentic->pixle-ref-num bc width hight)
  (+ (* width (first bc)) (* width hight (second bc))))

(define (find-baracentric-intercect lin tr)
  (let ((O (line-start lin))
        (D (line-vec lin))
        (V0(tri-p1 tr))
        (V1(tri-p2 tr))
        (V2(tri-p3 tr)))
    (let ((T (vec- O V0))
          (E1(vec- V1 V0))
          (E2(vec- V2 V0))