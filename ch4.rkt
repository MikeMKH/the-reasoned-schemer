#lang racket
(require cKanren/miniKanren)

; https://github.com/rymaju/mykanren/blob/c56318625319b598c0e05cf993e8aafabee83187/mykanren.rkt#L360C1-L366C34
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(define s (== #t #t))
(define u (== #t #f))

(require (except-in rackunit fail))
(require rackunit/text-ui)

; 1
(define (append l t)
  (cond
    ((null? l) t)
    (#t (cons (car l)
              (append (cdr l) t)))))

; 41
(defrel (appendo l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (a d res)
      (conso a d l)
      (conso a res out)
      (appendo d t res)))))

; 43
(defrel (swappendo l t out)
  (conde
    ((fresh (a d res)
       (conso a d l)
       (conso a res out)
       (swappendo d t res)))
    ((nullo l) (== t out))))

; 45
(define (unwrap x)
  (cond
    ((pair? x) (unwrap (car x)))
    (#t x)))

; 47
(defrel (unwrapo x out)
  (conde
    ((fresh (a)
       (caro x a)
       (unwrapo a out)))
    ((== x out))))

(run-tests
 (test-suite "chapter 4"
  (test-equal? "1" (append '(a b c) '(d e)) '(a b c d e))
  (test-equal? "2" (append '() '(d e)) '(d e))
  ;(test-equal? "3" (append 'a '(d e)) '(d e)) ; car: contract violation
  (test-equal? "2" (append '(d e) 'a) '(d e . a))

  ; appendo is defrel outside of run-tests
  (test-equal? "17" (run 6 (x) (fresh (y z) (appendo x y z))) '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3) (_.0 _.1 _.2 _.3 _.4)))
  (test-equal? "18" (run 6 (y) (fresh (x z) (appendo x y z))) '(_.0 _.0 _.0 _.0 _.0 _.0))
  (test-equal? "20" (run 6 (z) (fresh (x y) (appendo x y z))) '(_.0 (_.0 . _.1) (_.0 _.1 . _.2) (_.0 _.1 _.2 . _.3) (_.0 _.1 _.2 _.3 . _.4) (_.0 _.1 _.2 _.3 _.4 . _.5)))
  (test-equal? "21" (run 6 (x y z) (appendo x y z))
    '((() _.0 _.0)
     ((_.0) _.1 (_.0 . _.1))
     ((_.0 _.1) _.2 (_.0 _.1 . _.2))
     ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
     ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
     ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))))
  (test-equal? "22" (run* (x) (appendo '(cake) '(tastes yummy) x)) '((cake tastes yummy)))
  (test-equal? "23" (run* (x) (fresh (y) (appendo `(cake & ice ,y) '(tastes yummy) x))) '((cake & ice _.0 tastes yummy)))
  (test-equal? "24" (run* (x) (fresh (y) (appendo `(cake & ice cream) y x))) '((cake & ice cream . _.0)))
  (test-equal? "25" (run 1 (x) (fresh (y) (appendo `(cake & ice . ,y) '(d t) x))) '((cake & ice d t)))
  (test-equal? "26" (run 5 (x) (fresh (y) (appendo `(cake & ice . ,y) '(d t) x)))
    '((cake & ice d t)
     (cake & ice _.0 d t)
     (cake & ice _.0 _.1 d t)
     (cake & ice _.0 _.1 _.2 d t)
     (cake & ice _.0 _.1 _.2 _.3 d t)))
  (test-equal? "27" (run 5 (y) (fresh (x) (appendo `(cake & ice . ,y) '(d t) x))) '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3)))
  (test-equal? "30" (run 5 (x) (fresh (y) (appendo `(cake & ice . ,y) `(d t . ,y) x)))
    '((cake & ice d t)
     (cake & ice _.0 d t _.0)
     (cake & ice _.0 _.1 d t _.0 _.1)
     (cake & ice _.0 _.1 _.2 d t _.0 _.1 _.2)
     (cake & ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3)))
  (test-equal? "31" (run* (x) (fresh (z) (appendo '(cake & ice cream) `(d t . ,z) x))) '((cake & ice cream d t . _.0)))
  (test-equal? "33" (run 6 (x) (fresh (y) (appendo x y '(cake & ice cream d t)))) '(() (cake) (cake &) (cake & ice) (cake & ice cream) (cake & ice cream d)))
  (test-equal? "35" (run 6 (y) (fresh (x) (appendo x y '(cake & ice cream d t)))) '((cake & ice cream d t) (& ice cream d t) (ice cream d t) (cream d t) (d t) (t)))
  (test-equal? "37" (run 6 (x y) (appendo x y '(cake & ice cream d t)))
    '((() (cake & ice cream d t))
     ((cake) (& ice cream d t))
     ((cake &) (ice cream d t))
     ((cake & ice) (cream d t))
     ((cake & ice cream) (d t))
     ((cake & ice cream d) (t))))
  ;(test-equal? "39" (run 7 (x y) (appendo x y '(cake & ice d t))) '(has-no-result)) ; finds no result
  (test-equal? "39 with cream" (run 7 (x y) (appendo x y '(cake & ice cream d t)))
    '((() (cake & ice cream d t))
     ((cake) (& ice cream d t))
     ((cake &) (ice cream d t))
     ((cake & ice) (cream d t))
     ((cake & ice cream) (d t))
     ((cake & ice cream d) (t))
     ((cake & ice cream d t) ())))
  (test-equal? "42 with cream" (run* (x y) (appendo x y '(cake & ice cream d t)))
    '((() (cake & ice cream d t))
     ((cake) (& ice cream d t))
     ((cake &) (ice cream d t))
     ((cake & ice) (cream d t))
     ((cake & ice cream) (d t))
     ((cake & ice cream d) (t))
     ((cake & ice cream d t) ())))

  ; swappendo is defrel outside of run-tests
  (test-equal? "44" (run 6 (x y) (swappendo x y '(cake & ice cream d t)))
    ; same output as 37
    ; swapping two conde lines does not affect the values contributed by conde
    '((() (cake & ice cream d t))
     ((cake) (& ice cream d t))
     ((cake &) (ice cream d t))
     ((cake & ice) (cream d t))
     ((cake & ice cream) (d t))
     ((cake & ice cream d) (t))))

  ; unwrap is define outside of run-tests
  (test-equal? "45" (unwrap '((((pizza))))) 'pizza)
  (test-equal? "46" (unwrap '((((pizza pie) with)) gralic)) 'pizza)

  ; unwrapo is defrel outside of run-tests
  (test-equal? "47" (run* (x) (unwrapo '((((pizza)))) x)) '(((((pizza)))) (((pizza))) ((pizza)) (pizza) pizza))
  (test-equal? "50" (run 1 (x) (unwrapo 'pizza x)) '(pizza))
  (test-equal? "51" (run 1 (x) (unwrapo `((,x)) 'pizza)) '(pizza))
  (test-equal? "52" (run 5 (x) (unwrapo x 'pizza)) '(pizza (pizza . _.0) ((pizza . _.0) . _.1) (((pizza . _.0) . _.1) . _.2) ((((pizza . _.0) . _.1) . _.2) . _.3)))
  (test-equal? "53" (run 5 (x) (unwrapo x '((pizza)))) '(((pizza)) (((pizza)) . _.0) ((((pizza)) . _.0) . _.1) (((((pizza)) . _.0) . _.1) . _.2) ((((((pizza)) . _.0) . _.1) . _.2) . _.3)))
  (test-equal? "54" (run 5 (x) (unwrapo `((,x)) 'pizza)) '(pizza (pizza . _.0) ((pizza . _.0) . _.1) (((pizza . _.0) . _.1) . _.2) ((((pizza . _.0) . _.1) . _.2) . _.3)))
 ))