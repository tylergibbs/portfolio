#lang racket
(require "../semantics.rkt")

(provide
 large-map
 config
 rom
 ext
 (rename-out [my-module-begin #%module-begin]))

(define worl (make-parameter (new-state)))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([stat (new-state)])
       body ...)))


(define-syntax-rule (large-map)
  (make-long-map (worl)))

(define-syntax-rule (config name at body ...)
  (mod-confg (worl) name at (list body ...)))

(define-syntax-rule (rom name dscript body ...)
  (room name dscript (list body ...)))

(define-syntax-rule (ext dir to)
  (ext dir to))
;(config "" "1" (room "1" "" (list (exit "north" "2")(exit "south" "3")))
 ;                                 (room "2" "" (list (exit "north" "1")))
  ;                    (room "3" "" (list (exit "north" "4") (exit "south" "1")))
   ;                   (room "4" "" (list (exit "north" "1") (exit "south" "2")))
    ;                 )
;(large-map)