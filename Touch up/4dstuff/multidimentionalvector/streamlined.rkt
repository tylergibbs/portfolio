#lang racket
(require math/matrix)
(define d 4)

(define (mod n m)
  (if (< n 0) (+ m (remainder n m))
      (remainder n m)))
;!+n->n
(define (! n)
  (if (= n 1) 0 (+ (sub1 n) (! (sub1 n)))))

(define (scal vec dim)
  (diagonal-matrix (matrix->list vec)))
(scal (row-matrix (2 2 1)) 2)

(define (scal-n n dim)
  (diagonal-matrix (append (make-list dim n) '(1))))
(scal-n 2/3 2)

(define (mov vec dim)
  (build-matrix (add1 dim) (add1 dim) (λ (x y)
                                        (cond 
                                          ((= x y) 1)
                                          ((= y dim) (matrix-ref vec 0 x))
                                          (else 0)))))

(define (aux x ac acc lim)
  (cond
    ((= x 0) (list ac acc))
    ((= ac lim) (aux (sub1 x) (+ 2 acc) (add1 acc) lim))
    (else (aux (sub1 x) (add1 ac) acc lim))))


(define (rotat deg dim x)
  (local((define a (aux x 1 0 (sub1 dim)))
         (define r (first a))
         (define q (second a)))
    (build-matrix (add1 dim) (add1 dim)
                  (λ (y z)
                    (cond
                      ((and (= y q) (= z q)) (cos deg))
                      ((and (= y q) (= z r)) (* -1(sin deg)))
                      ((and (= y r) (= z q)) (* 1(sin deg)))
                      ((and (= y r) (= z r)) (cos deg))
                      ((= y z) 1)
                      (else 0))))
    ))
(define (inv-rotat deg dim x)
  (local((define a (aux x 1 0 (sub1 dim)))
         (define r (first a))
         (define q (second a)))
    (build-matrix (add1 dim) (add1 dim)
                  (λ (y z)
                    (cond
                      ((and (= y q) (= z q)) (cos deg))
                      ((and (= y q) (= z r)) (* 1(sin deg)))
                      ((and (= y r) (= z q)) (* -1(sin deg)))
                      ((and (= y r) (= z r)) (cos deg))
                      ((= y z) 1)
                      (else 0))))
    ))

;shorten: alon alon -> alon
(define (shorten alon)
  (cond
    ((= (length alon)2) (cons 1 alon))
    ;((> (first alon)0) '(500 500 1))
    (else (shorten (map (λ (x) x) (rest alon))))))

;progect: mat mat alon mat ->mat
(define (progect a c alor dim)
  (list->matrix 1 3 (reverse (shorten 
                              (rest (reverse (matrix->list (apply matrix* (matrix- a c) (reverse (map (λ (x y) (rotat x dim y)) alor (build-list (length alor) (λ(x)x)))))))
                                    )))))

;normalize:alod x deg dim -> alod

;############################
(struct obj (p alop agop vel alovec alod)#:transparent)
;agraph has no designator it is a list of list of ref numbers
(struct world (cam aloobj)#:transparent)

;rotat-p: obj->obj
(define (rotate-p ob dim x deg)
  (obj (obj-p ob) (map (λ (p) (matrix* p (rotat deg dim x))) (obj-alop ob))
       (obj-agop ob) (obj-vel ob)
       (map (λ (v) (matrix* v (rotat deg dim x))) (obj-alovec ob))
       ;;d change
       (map (λ (y z) (if (= x z) (+ deg y) y)) (obj-alod ob) (build-list (! d) (λ (x)x)))
       ))

;sub-rotate-p
#|
(define (sub-rotate mat dim x deg alod)
  (apply matrix* ;(matrix* 
                  (apply matrix* mat (map (λ (y z) (inv-rotat y dim z))alod(build-list (! dim) (λ (x)x))))
                  ;(rotat deg dim x))
                 (map (λ (y z) (rotat y dim z))alod(build-list (! dim) (λ (x)x)))))
(define (sub-rotate-p ob dim x deg)
  (obj (obj-p ob)
       (map (λ (p) (sub-rotate p dim x deg (obj-alod ob))) (obj-alop ob))
       (obj-agop ob)
       (obj-vel ob)
       (map (λ (p) (sub-rotate p dim x deg (obj-alod ob))) (obj-alovec ob))
        (obj-alod ob)))
|#
;move-p: obj obj
(define (move-p ob dir)
  (obj (matrix+ (obj-p ob) dir) (obj-alop ob) (obj-agop ob) (obj-vel ob) (obj-alovec ob) (obj-alod ob)))

;accselerate-p:obj->obj
(define (acc-p ob dir)
  (obj (obj-p ob) (obj-alop ob) (obj-agop ob) (matrix+ (obj-vel ob) dir) (obj-alovec ob) (obj-alod ob)))

;############################
(require 2htdp/image)
(require 2htdp/universe)


(define stat 'no)
;key wor str ->wor
(define (key wor str)
  (let* ((h (λ (x) (cond
                  ((equal? x "a") (λ (x) (acc-p x (first (obj-alovec x)))))
                  ((equal? x "w") (λ (x) (acc-p x (second (obj-alovec x)))))
                  ((equal? x "e") (λ (x) (acc-p x (third (obj-alovec x)))))
                  ((equal? x "e") (λ (x) (acc-p x (fourth (obj-alovec x)))))
                  ((equal? x "d") (λ (x) (acc-p x (matrix-scale(first (obj-alovec x))-1))))
                  ((equal? x "s") (λ (x) (acc-p x (matrix-scale(second (obj-alovec x))-1))))
                  ((equal? x "q") (λ (x) (acc-p x (matrix-scale(third (obj-alovec x))-1))))
                  ((equal? x "2") (λ (x) (acc-p x (matrix-scale(fourth (obj-alovec x))-1))))
                  ((equal? x "z") (λ (x) (rotate-p x d 0 (degrees->radians 10))))
                  ((equal? x "x") (λ (x) (rotate-p x d 1 (degrees->radians 10))))
                  ((equal? x "c") (λ (x) (rotate-p x d 2 (degrees->radians 10))))
                  ((equal? x "v") (λ (x) (rotate-p x d 3 (degrees->radians 10))))
                  ((equal? x "b") (λ (x) (rotate-p x d 4 (degrees->radians 10))))
                  ((equal? x "n") (λ (x) (rotate-p x d 5 (degrees->radians 10))))
                  ;((equal? x "m") (λ (x) (sub-rotate-p x d 0 (degrees->radians 0))))
                  (else (λ (x)x))
                  )))
         (f (λ (ob) ((h str)ob))))
    (cond
      ((equal? str "i") (begin (set! stat 'cam) wor))
      ((equal? str "o") (begin (set! stat 'obj) wor))
      ((equal? stat 'cam) (world (f (world-cam wor)) (world-aloobj wor)))
      ((equal? stat 'obj) (world (world-cam wor) (map f (world-aloobj wor))))
      (else wor))))
;wor->wor
(define (change wor)
  (world (move-p (world-cam wor) (obj-vel (world-cam wor))) (map (λ (x) (move-p x (obj-vel x))) (world-aloobj wor))))
;wor->img
(define (disp wor)
  (foldr (λ (w z) (let ((c (obj-p (world-cam wor)))
                        (r (map (λ (z) (* -1 z)) (obj-alod (world-cam wor)))))
                    (foldr (λ (x y) 
                             (let ((l (progect (matrix+ (obj-p w) x) c r d)))
                               (place-image
                                (circle 3 'solid 'black)
                                (+ 150 (matrix-ref l 0 0))
                                (+ 150 (matrix-ref l 0 1))
                                y)))
                           z
                           (obj-alop w))))
         (empty-scene 300 300)
         (world-aloobj wor)))

;wor->i
(define (disp-line wor)
  (foldr (λ (a b) 
           (let ((c (obj-p (world-cam wor)))
                 (r (map (λ (z) (* -1 z)) (obj-alod (world-cam wor))))
                 (alop (obj-alop a)))
             (foldr (λ (f ap e)
                      (foldr (λ (g h)
                               (let ((p (progect (matrix+ (obj-p a) (list-ref alop g)) c r d))
                                     (l (progect (matrix+ (obj-p a) ap) c r d)))
                                 (add-line h
                                           (+ 150 (matrix-ref l 0 0))
                                           (+ 150 (matrix-ref l 0 1))
                                           (+ 150 (matrix-ref p 0 0))
                                           (+ 150 (matrix-ref p 0 1))
                                           'black
                                           ))
                               )
                             e
                             f
                             ))
                    b
                    (obj-agop a)
                    (obj-alop a)))
           )
         (empty-scene 300 300)
         (world-aloobj wor)))

#|
(big-bang (world (obj (matrix ((0 0 -50 1))) (list ) '() (matrix ((0 0 0 0))) (list (matrix ((1 0 0 0))) (matrix ((0 1 0 0))) (matrix ((0 0 1 0)))) (list 0 0 0))
                 (list 
                  (obj (matrix ((0 0 50 1))) 
                       (list (matrix ((10 10 -10 1)))
                             (matrix ((-10 10 -10 1)))
                             (matrix ((10 -10 -10 1)))
                             (matrix ((-10 -10 -10 1)))
                             (matrix ((10 10 10 1)))
                             (matrix ((-10 10 10 1)))
                             (matrix ((10 -10 10 1)))
                             (matrix ((-10 -10 10 1)))
                             ) '((1 2 4)
                                 (0 3 5)
                                 (6 3 0)
                                 (7 2 1)
                                 (0 5 6)
                                 (1 4 7)
                                 (2 7 4)
                                 (3 5 6)) (matrix ((0 0 0 0))) (list (matrix ((1 0 0 0))) (matrix ((0 1 0 0))) (matrix ((0 0 1 0)))) (list 0 0 0) )))
          (to-draw disp-line)
          (on-key key)
          (on-tick change))
|#

(big-bang (world (obj (matrix ((0 0 -50 0 1))) (list ) '() (matrix ((0 0 0 0 0))) (list (matrix ((1 0 0 0 0))) (matrix ((0 1 0 0 0))) (matrix ((0 0 1 0 0))) (matrix ((0 0 0 1 0)))) (list 0 0 0 0 0 0))
                 (list 
                  (obj (matrix ((0 0 50 0 1))) 
                       (list (matrix ((10 10 -10 10 1)))
                             (matrix ((-10 10 -10 10 1)))
                             (matrix ((10 -10 -10 10 1)))
                             (matrix ((-10 -10 -10 10 1)))
                             (matrix ((10 10 10 10 1)))
                             (matrix ((-10 10 10 10 1)))
                             (matrix ((10 -10 10 10 1)))
                             (matrix ((-10 -10 10 10 1)))
                             
                             (matrix ((10 10 -10 -10 1)))
                             (matrix ((-10 10 -10 -10 1)))
                             (matrix ((10 -10 -10 -10 1)))
                             (matrix ((-10 -10 -10 -10 1)))
                             (matrix ((10 10 10 -10 1)))
                             (matrix ((-10 10 10 -10 1)))
                             (matrix ((10 -10 10 -10 1)))
                             (matrix ((-10 -10 10 -10 1)))
                             ) '((1 2 4 8)
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
                                 (11 13 14 7)) (matrix ((0 0 0 0 0))) (list (matrix ((1 0 0 0 0))) (matrix ((0 1 0 0 0))) (matrix ((0 0 1 0 0))) (matrix ((0 0 0 1 0)))) (list 0 0 0 0 0 0) )))
          (to-draw disp-line)
          (on-key key)
          (on-tick change))
