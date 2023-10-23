#lang racket
(provide (all-defined-out))

(struct confg (name place alorm)#:mutable #:transparent)
(struct room (name dscrpt aloex)#:mutable #:transparent)
(struct exit (dir link)#:mutable #:transparent)

(define directions '("north" "south" "east"));"south" "east" "west"

(define (new-state)
       (confg #f #f #f))

(define (mod-confg conf nam at alorm)
  (begin
    (set-confg-name! conf nam)
    (set-confg-place! conf at)
    (set-confg-alorm! conf alorm)
    ))

(define (move conf rm-name)
  (confg
       (confg-name conf)
    rm-name
    (confg-alorm conf)
  ))

(define (get-rm pl alorm)
    (cond
      ((null? alorm) (error 'no-such-room))
      ((equal? (room-name (first alorm)) pl) (first alorm))
      (else (get-rm pl (rest alorm)))
      ))

(define (get-rm-name dir aloex)
  (cond
    ((null? aloex) #f)
    ((equal? (exit-dir (first aloex)) dir) (exit-link (first aloex)))
    (else (get-rm-name dir (rest aloex)))
    ))

(define (move-in-dir conf dir)
  (let ((rm (get-rm-name dir (room-aloex (get-rm (confg-place conf) (confg-alorm conf))))))
    (if rm
  (move conf rm)
  #f)))

(define (make-long-map conf)
  (make-full-map conf (confg-place conf) 15 0 #f '()))

(define (make-full-map conf start max i tf aux)
  
  (cond
    ((not conf) #f)
    ((= i max) #f)
    ((and tf (equal? start (confg-place conf))) (cons (confg-place conf) aux))
    (else
     (let ((nam (confg-place conf)))
     (foldr
       (λ (x y) (let ((z (make-full-map (move-in-dir conf x) start max (add1 i)
                                        #t (cons nam aux))))
                  (cond
                    ((false? z) y)
                    ((< (length z) (length y)) y)
                    (else z))))
       '()
       directions))
    )))

;(make-long-map (confg "" "1" (list (room "1" "" (list (exit "north" "2")))
 ;                     (room "2" "" (list (exit "north" "1")))
                      ;)))
;(make-long-map (confg "" "1" (list (room "1" "" (list (exit "north" "2")(exit "south" "3")))
 ;                                 (room "2" "" (list (exit "north" "1")))
  ;                    (room "3" "" (list (exit "north" "4") (exit "south" "1")))
   ;                   (room "4" "" (list (exit "north" "1") (exit "south" "2")))
    ;                 )))
                      
