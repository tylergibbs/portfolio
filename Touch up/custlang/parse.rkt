#lang racket
(require "lang/language.rkt")
(provide (all-defined-out))

(define (read-<> in rec)
    (let ((chr (read-char in)))
    (cond
      ((and (eof-object? chr) (not rec)) eof)
      ((and (eof-object? chr) rec) (error 'unclosed_<>))
      ((equal? #\< chr) (read-<> in #t))
      ((equal? #\> chr) "")
      ((not rec) (read-<> in rec))
      (else (string-append (string chr) (read-<> in rec))))
      ))

(define (peek-<> in rec n)
    (let ((chr (peek-char in n)))
    (cond
      ((and (eof-object? chr) (not rec)) eof)
      ((and (eof-object? chr) rec) (error 'unclosed_<>))
      ((equal? #\< chr) (peek-<> in #t (add1 n)))
      ((equal? #\> chr) "")
      ((not in) (peek-<> in rec (add1 n)))
      (else (string-append (string chr) (peek-<> in rec (add1 n)))))
      ))

(define (parse-expr src in)
 ; (print "parse-expr")(newline)
  (define-values (line column position) (port-next-location in))
  (define next-<> (read-<> in #f))
  (define (decorate sexp span)
    (datum->syntax #f sexp (list src line column position span)))
  (cond
    ((eof-object? next-<>) (begin (decorate '(large-map)0) eof))
    ((regexp-match #rx"^configuration" next-<>)
     (let ((mtch (regexp-match #rx"configuration +name=\"([a-z]+)\" +at=\"([a-z]+)\" *"
                                     next-<>)))
       (if (not mtch) (error 'configuration (string-append
                                             "expects configuration name=\"([a-z]+)\" at=\"([a-z]+)\"| given"
                                             next-<>
                                             ))
           (let (
                 (name (second mtch))
                 (at (third mtch))
                 (body (parse-exprs src in "configuration"))
           )
       (begin
         (define-values (l c tail-position) (port-next-location in))
         (decorate `(config ,name ,at ,@body) (- tail-position position))
         )))))
    ((regexp-match #rx"^room" next-<>)
     (let ((mtch (regexp-match #rx"room +name=\"([a-z]+)\" +description=\"([a-z]+)\" *"
                                     next-<>)))
       (if (not mtch) (error 'configuration "expects configuration name=\"([a-z]+)\" at=\"([a-z]+)\"/")
           (let (
                 (name (second mtch))
                 (dscrpt(third mtch))
                 (body (parse-exprs src in "room"))
                 )
             (begin
               (define-values (l c tail-position) (port-next-location in))
               (decorate `(rom ,name ,dscrpt ,@body) (- tail-position position))
               )))))
    ((regexp-match #rx"^exit" next-<>)
     (let ((mtch (regexp-match #rx"exit +direction=\"([a-z]+)\" +to=\"([a-z]+)\" */ *"
                                     next-<>)))
       (if (not mtch) (error 'exit "expects exit direction=\"([a-z]+)\" to=\"([a-z]+)\"/")
           (let (
                 (dir (second mtch))
                 (to (third mtch))
                 )
             (begin
               (define-values (l c tail-position) (port-next-location in))
               (decorate `(ext ,dir ,to) (- tail-position position))
               )))))
    (else (begin
            (print next-<>)
            (error 'word_not_recognized)))
    ))

(define (parse-exprs scr in /name)
   ;(print "parse-exprs")(newline)
  (define peeked-<> (peek-<> in #f 0))
  ;(print (string-append "/" /name))
  ;(newline)
  ; (print peeked-<>)
  ;(newline)
  ;(newline)
  (cond
    ((eof-object? peeked-<>) (error 'parse-exprs (string-append "unclosed " /name)))
    ((regexp-match (regexp (string-append "/" /name)) peeked-<>)
     (begin (read-<> in #f) null))
    (else(cons (parse-expr scr in) (parse-exprs scr in /name)))
    ))

;(parse-expr ""(open-input-string "<configuration  name=\"tst\" at=\"a\" >  <room  name=\"a\" description=\"piano\"> <exit direction=\"north\" to=\"a\" /> </room>< /configuration>"))
    
    
    