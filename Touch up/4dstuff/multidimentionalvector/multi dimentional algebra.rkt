#lang racket

;num = avon
(begin
(define (new-posn p1 p2)
  (let ((graph #(       #(0	1	2	3	4	5	6	7)
                        #(1	0	3	2	5	4	7	6)
                        #(2	3	0	1	6	7	4	5)
                        #(3	2	1	0	7	6	5	4)
                        #(4	5	6	7	0	1	2	3)
                        #(5	4	7	6	1	0	3	2)
                        #(6	7	4	5	2	3	0	1)
                        #(7	6	5	4	3	2	1	0))))
    (vector-ref (vector-ref graph p2) p1)))

(define (new+- p1 p2)
  (let ((graph #(       #(1	1	1	1	1	1	1	1)
                        #(1	-1	1	-1	1	-1	-1	1)
                        #(1	-1	-1	1	1	1	-1	-1)
                        #(1	1	-1	-1	1	-1	1	-1)
                        #(1	-1	-1	-1	-1	1	1	1)
                        #(1	1	-1	1	-1	-1	-1	1)
                        #(1	1	1	-1	-1	1	-1	-1)
                        #(1	-1	1	1	-1	-1	1	-1))))
    (vector-ref (vector-ref graph p2) p1)))

(define (add n m)
  (local ((define-values (s l) (if (< (vector-length n) (vector-length m)) (values n m) (values m n))))
    (vector-map + l (build-vector (vector-length l) (λ (x) (if (< x (vector-length s)) (vector-ref s x) 0))))))
(add #(1 0 0 1) #(1 0))

(define (sub n m)
  (local ((define-values (s l) (if (< (vector-length n) (vector-length m)) (values n m) (values m n))))
    (vector-map - l (build-vector (vector-length l) (λ (x) (if (< x (vector-length s)) (vector-ref s x) 0))))))
(sub #(1 0 0 1) #(1 1))

(define (mull n1 m1)
  (local
    ((define-values (n m) (if (< (vector-length n1) (vector-length m1)) (values (build-vector (vector-length m1) (λ (x) (if (< x (vector-length n1)) (vector-ref n1 x) 0))) m1) (values n1 (build-vector (vector-length n1) (λ (x) (if (< x (vector-length m1)) (vector-ref m1 x) 0))))))
     (define acc (build-vector (if (>(vector-length n) (vector-length m)) (vector-length n) (vector-length m)) (λ (x)0))))
  (begin
  (vector-map (λ (v1 p1) (vector-map (λ (v2 p2) (vector-set! acc (new-posn p1 p2) (+ (* (new+- p1 p2) v1 v2) (vector-ref acc (new-posn p1 p2))))) m (build-vector (vector-length m) (λ (x)x)))) n (build-vector (vector-length m) (λ (x)x)))
  acc)))
(mull #(4) #(3 2))
(mull #(4 1 1 1) #(4 -1 -1 -1))
(mull #(4 1 2 3) #(4 -1 -2 -3))

(define (short n)
  (list->vector (reverse 
              ((λ (x l) (if (= (first l) 0) (x x (rest l)) l)) (λ (x l) (cond
                                                                          ((null? (rest l)) l)
                                                                          ((= (first l) 0) (x x (rest l)))
                                                                          (else l)))         
                (reverse (vector->list n))))))
(short #(1 1 0 0))
(add (short #(0 0)) #(1 0 0 2))

(define (conj n)
  (vector-map (λ (x y) (if (= y 0) x (* -1 x)))  n (build-vector (vector-length n) (λ (x) x))))
(conj #(4 2 2 1))
(mull (conj #(4 2 2 1)) #(4 2 2 1))

(define (inv n)
  (let ((m (vector-ref (mull n (conj n))0)))
  (vector-map (λ (x) (/ x m)) (conj n))))
(inv #(10))
(mull #(0 5) (inv #(0 5)))
(mull #(1 5) (inv #(1 5)))
(mull #(1 5 4 7) (inv #(1 5 4 7)))

(define (div n m)
  (mull n (inv m)))
(div #(9) #(3))
(div #(9 6) #(3 2))
(mull #(3 2) (div #(9 12) #(3 2)))
)
