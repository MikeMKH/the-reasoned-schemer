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
(defrel (alwayso)
  (conde
   (s)
   ((alwayso))))

; 14
(defrel (nevero)
  (nevero))

; 24
(defrel (very-recursiveo)
  (conde
   ((nevero))
   ((very-recursiveo))
   ((alwayso))
   ((very-recursiveo))
   ((nevero))))

(run-tests
 (test-suite "chapter 6"
  ; alwayso is defrel outside of run-tests
  (test-equal? "1" (run 1 (q) (alwayso)) '(_.0))
  (test-equal? "2" (run 1 (q) (conde (s) ((alwayso)))) '(_.0))
  ;(test-equal? "3" (run* (q) (alwayso)) '(_.0 'endlessly))
  ;(test-equal? "4" (run* (q) (conde (s) ((alwayso)))) '(_.0 'endlessly))
  (test-equal? "6" (run 5 (q) (alwayso)) '(_.0 _.0 _.0 _.0 _.0))
  (test-equal? "7" (run 5 (q) (== 'onion q) (alwayso)) '(onion onion onion onion onion))
  ;(test-equal? "8" (run 1 (q) (alwayso) u) '('endlessly-nothing))
  (test-equal? "9" (run 1 (q) (== 'garlic q) s (== 'onion q)) '())
  ;(test-equal? "10" (run 1 (q) (== 'garlic q) (alwayso) (== 'onion q)) '('endlessly-nothing))
  (test-equal? "11" (run 1 (q) (conde ((== 'garlic q) (alwayso)) ((== 'onion q))) (== 'onion q)) '(onion))
  ;(test-equal? "12" (run 2 (q) (conde ((== 'garlic q) (alwayso)) ((== 'onion q))) (== 'onion q)) '(onion endlessly-nothing))
  (test-equal? "13" (run 5 (q) (conde ((== 'garlic q) (alwayso)) ((== 'onion q) (alwayso))) (== 'onion q)) '(onion onion onion onion onion))

  ; nevero is defrel outside of run-tests
  ;(test-equal? "16" (run 1 (q) (nevero)) '(endlessly))
  (test-equal? "17" (run 1 (q) u (nevero)) '())
  (test-equal? "18" (run 1 (q) (conde (s) ((nevero)))) '(_.0))
  (test-equal? "19" (run 1 (q) (conde ((nevero)) (s))) '(_.0))
  ;(test-equal? "20" (run 2 (q) (conde (s) ((nevero)))) '(_.0 endlessly))
  ;(test-equal? "21" (run 1 (q) (conde (s) ((nevero))) u) '(endlessly))
  (test-equal? "22" (run 5 (q) (conde ((nevero)) ((alwayso)) ((nevero)))) '(_.0 _.0 _.0 _.0 _.0))
  (test-equal? "23" (run 6 (q) (conde ((== 'spicy q)(nevero)) ((== 'hot q)(nevero)) ((== 'apple q) (alwayso)) ((== 'cider q)(alwayso)))) '(apple cider apple cider apple cider))

  ; very-recursiveo is defrel outside of run-tests and is very silly
  (test-equal? "25 only 3" (run 3 (q) (very-recursiveo)) '(_.0 _.0 _.0))
 ))