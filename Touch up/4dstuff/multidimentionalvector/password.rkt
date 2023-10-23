#lang web-server/insta
(require racket)
(struct account (name password age))
(define names '("q"))
(define passes'("w"))
(define accounts (hash "q" (account "q" "w" 12)))
(define (f x)x)


(define (add-account request)
  (let*((bind (request-bindings request))
       (name (extract-binding/single 'username bind))
       (pass (extract-binding/single 'password bind))
       (age (extract-binding/single 'age bind)))
    (send/suspend/dispatch
   (λ (embed/url)
      (if (false? (member pass passes))
          (begin
    (set! names (cons name names))
    (hash-set pass (account name pass age) accounts)
     (embed/url start))
    (embed/url eror-page))))))
          
      

(define (form-account request)
  (send/suspend/dispatch
   (λ (embed/url)
     (response/xexpr
     '(html
       (body
        (form ((action ,(embed/url add-account)))
              (input ((name "username")))
              (input ((name "password")))
              (input ((name "age"))))))))))


(define (eror-page request)
  '(html (head "error")
                (body (h4 "your username or password was incorrect")
                   (p "try again")
                (form ((action ,(embed/url check-password)))
                      (input((name "username")))
                      (input((name "password")))
                      (input((type "submit"))))
                (p)
                (form ((acion ,(embed/url add-account)))
                      (input ((name form-account)(type submit)))))))

(define (check-password request)
  (let*((bind (request-bindings request))
       (name (extract-binding/single 'username bind))
       (pass (extract-binding/single 'password bind)))
    (send/suspend/dispatch
     (λ (embed/url)
  (response/xexpr
    (cond
      ((false? (member name names)) (embed/url eror-page))
      ((not (equal? pass (account-password (hash-ref name accounts))))
                                            (embed/url eror-page))
      (else '(html (head "succses")
             (body (p "succses"))))))))))

(define (start request)
  (send/suspend/dispatch 
   (λ (embed/url)
  (response/xexpr
   `(html (head (title "password"))
          (body (form ((action ,(embed/url check-password)))
                      (input((name "username")))
                      (input((name "password")))
                      (input((type "submit"))))
                (p "make an account")
                (form ((acion ,(embed/url form-account)))
                      (input ((type "submit"))))
                ))))))