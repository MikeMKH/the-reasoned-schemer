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

; 5
(defrel (bit-xoro x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 1 r))
   ((== 1 x) (== 0 y) (== 1 r))
   ((== 1 x) (== 1 y) (== 0 r))))

; 10
(defrel (bit-ando x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 0 r))
   ((== 1 x) (== 0 y) (== 0 r))
   ((== 1 x) (== 1 y) (== 1 r))))

; 12
(defrel (half-addero x y r c)
  (bit-xoro x y r)
  (bit-ando x y c))

(run-tests
 (test-suite "chapter 7"
  ; bit-xoro is defrel outside of run-tests
  (test-equal? "6" (run* (x y) (bit-xoro x y 0)) '((0 0) (1 1)))
  (test-equal? "8" (run* (x y) (bit-xoro x y 1)) '((0 1) (1 0)))
  (test-equal? "9" (run* (x y r) (bit-xoro x y r)) '((0 0 0) (0 1 1) (1 0 1) (1 1 0)))

  ; bit-ando is defrel outside of run-tests
  (test-equal? "11" (run* (x y) (bit-ando x y 1)) '((1 1)))

  ; half-addero is defrel outside of run-tests
  (test-equal? "12" (run* (r) (half-addero 1 1 r 1)) '(0))
  (test-equal? "13" (run* (x y r c) (half-addero x y r c)) '((0 0 0 0) (0 1 1 0) (1 0 1 0) (1 1 0 1)))
 ))