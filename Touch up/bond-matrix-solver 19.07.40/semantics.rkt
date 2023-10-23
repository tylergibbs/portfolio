#lang racket
(provide (all-defined-out))

(require redex)
(require redex/reduction-semantics)

(define-language lang
  (confg (nombre desc room ...));nombre place alorm
  (room (nombre desc exit ...));nombre dscrpt aloex
  (exit (dir nombre));dir link
  (dir '( "north" "south" "east" "west" ))
  (nombre string)
  (desc string)
  )

(define-metafunction lang
  lmap : confg (nombre ...) -> (nombre ...)
  ((lmap (nombre_n desc_d room_r ...) (nombre_s ...))
   ,(if (member (term nombre_n) (term (nombre_s ...)))
        (term (nombre_s ...))
        (term (lmap ((move nombre_n room_r ...) desc_d room_r ...) (nombre_n nombre_s ...)))
        ))
  )

(define-metafunction lang
  move : nombre room ... -> nombre
  ((move nombre_n (nombre_n desc_room (dir_1 nombre_1) exit_s ...) room_r ...)
   nombre_1)
  ((move nombre_n (nombre_room desc_room exit_s ...) room_r ...)
   (move nombre_n room_r ...))
  ((move nombre_n)
   ,(error 'move "no such room"))
  )