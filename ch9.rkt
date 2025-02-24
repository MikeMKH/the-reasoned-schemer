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

; 11
(defrel (not-pastao x)
  (conda
   ((== 'pasta x) u)
   (s s)))

(run-tests
 (test-suite "chapter 9"
  (test-equal? "1" (run* (q) (conda (u s) (s u))) '())
  (test-equal? "2" (run* (q) (conda (u s) (s s))) '(_.0))
  (test-equal? "3" (run* (q) (conda (s u) (s s))) '())
  (test-equal? "4" (run* (q) (conda (s s) (s u))) '(_.0))
  (test-equal? "5" (run* (x) (conda ((== 'olive x) s) (s (== 'oil x)))) '(olive))
  (test-equal? "7" (run* (x) (conda ((== 'virgin x) u) ((== 'olive x) s) (s (== 'oil x)))) '())
  (test-equal? "8" (run* (q) (fresh (x y) (== 'split x) (== 'pea y)  (conda ((== 'split x) (== x y)) (s s)))) '())
  (test-equal? "9" (run* (q) (fresh (x y) (== 'split x) (== 'pea y)  (conda ((== x y) (== 'split x)) (s s)))) '(_.0))

  ; not-pastao is defrel outside of run-tests
  (test-equal? "11" (run* (x) (conda ((not-pastao x) u) ((== 'spaghetti x) s))) '(spaghetti))
  (test-equal? "12" (run* (x) (== 'spaghetti x) (conda ((not-pastao x) u) ((== 'spaghetti x) s))) '())
 ))