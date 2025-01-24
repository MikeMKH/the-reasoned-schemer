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

; 13
(defrel (appendo l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (a d res)
      (conso a d l)
      (appendo d t res)
      (conso a res out)))))

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
  ; 22
 ))