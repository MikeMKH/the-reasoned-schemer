#lang racket

(require (except-in rackunit fail))
(require rackunit/text-ui)

; 4
(define (var name) (vector name))
(define (var? x) (vector? x))

; 5
;(define u (var 'u)) ; we define u as unsuccessful below
(define v (var 'v))
(define w (var 'w))

(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

; 11
(define empty-s '())

; 18
(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
  (cond
    ((pair? a) (walk (cdr a) s))
    (else v))))

; 20
(define (ext-s x v s)
  (cond
    ((occurs? x v s) #f)
    (else (cons `(,x . ,v) s))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eqv? v x))
      ((pair? v)
       (or (occurs? x (car v) s)
           (occurs? v (cdr v) s)))
      (else #f))))

; 29
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((eqv? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else #f))))

; 43
(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s `(,s) '()))))

; 44
(define success
  (lambda (s1)
    `(,s1)))

(define unsuccessful
  (lambda (s)
    '()))

; 54
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

; 56
(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf)))
    (else (lambda () (append-inf t-inf (s-inf))))))

; 61
(define (nevero)
  (lambda (s)
    (lambda ()
      ((nevero) s))))

; 69
(define (alwayso)
  (lambda (s)
    (lambda ()
      ((disj2 success (alwayso)) s))))

; 74
(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null? s-inf) '())
    ((pair? s-inf)
     (cons (car s-inf)
           (take-inf (and n (sub1 n)) (cdr s-inf))))
    (else (take-inf n (s-inf)))))

(run-tests
 (test-suite "chapter 10"
  (test-equal? "6" (cdr `(,z . a)) 'a)
  (test-equal? "7" (cdr `(,z . b)) 'b)
  (test-equal? "8" (cdr `(,z . (,x e ,y))) `(,x e ,y))
  (test-equal? "10" `((,x . ,z)) '((#(x) . #(z))))
  (test-equal? "13" (walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a)
  (test-equal? "14" (walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a)
  (test-equal? "15" (walk x `((,z . a) (,x . ,w) (,y . ,z))) w)
  (test-equal? "16 x" (walk x `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (test-equal? "16 v" (walk v `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (test-equal? "16 w" (walk w `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (test-equal? "17" (walk w `((,x . b) (,z . ,y) (,w . (,x e ,z)))) `(,x e ,z))
  (test-equal? "24" (occurs? x x '()) #t)
  (test-equal? "25" (occurs? x `(,y) `((,y . ,x))) #t)
  (test-equal? "26" (ext-s x `(,x) empty-s) #f)
  (test-equal? "27" (ext-s x `(,y) `((,y . ,x))) #f)
  (test-equal? "28"
     (let ((s `((,z .  ,x) (,y . ,z))))
       (let ((s (ext-s x 'e s)))
         (and s (walk y s))))
     'e)
  (test-equal? "48" ((== #t #f) empty-s) '())
  (test-equal? "49" (unsuccessful empty-s) '())
  (test-equal? "50" (success empty-s) '(()))
  (test-equal? "51" ((== x y) empty-s) `(((,x . ,y))))
  (test-equal? "53" ((disj2 (== 'olive x) (== 'oil x)) empty-s) `(((,x . olive)) ((,x . oil))))
  ; not really sure how to unit test these
  (test-not-false "62" ((nevero) empty-s))
  (test-not-false "63" (let ((s8 ((disj2 (== 'olive x) (nevero)) empty-s))) s8)) ; it produces `(((,x . olive)) . (nevero))
  (test-not-false "64" (let ((s8 ((disj2 (nevero) (== 'olive x)) empty-s))) s8)) ; it produces ((lambda () (append8 (nevero) ((,x . olive)))))
  (test-not-false "66" (let ((s8 ((disj2 (nevero) (== 'olive x)) empty-s))) (s8))) ; it produces `(((,x . olive)) . (nevero))
  (test-equal? "69" (car (((alwayso) empty-s))) '())
  ;(test-equal? "75" (take-inf 1 (((nevero) empty-s))) '(endless-nothing)) ; endless loop on nevero
  (test-equal? "76" (take-inf #f '(1 2 3)) '(1 2 3))
  (test-equal? "77" (take-inf 3 (((alwayso) empty-s))) '(() () ()))
  ;(test-equal? "78" (take-inf #f (((alwayso) empty-s))) '(endless-nothing)) ; endless loop on alwayso
  (test-equal? "79"
    (let ((k (length (take-inf 5 ((disj2 (== 'olive x) (== 'oil x)) empty-s))))) `(Found ,k not 5 substitutions))
    '(Found 2 not 5 substitutions))
  (test-equal? "80" (map length (take-inf 5 ((disj2 (== 'olive x) (== 'oil x)) empty-s))) '(1 1))
 ))