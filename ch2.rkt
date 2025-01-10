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

(defrel (caro p a)
  (fresh (d)
    (== (cons a d) p)))

(run-tests
 (test-suite "chapter 2"
  (test-equal? "1" (car '(grape rasin pear)) 'grape)
  (test-equal? "2" (car '(a c o r n)) 'a)
  
  ; caro is defrel outside of run-tests
  (test-equal? "3" (run* (q) (caro '(a c o r n) q)) '(a))
  (test-equal? "4" (run* (q) (caro '(a c o r n) 'a)) '(_.0))
  (test-equal? "5" (run* (r) (fresh (x y) (caro (cons r y) x) (== 'pear x))) '(pear))
  (test-equal? "7" (cons (car '(grape rasin pear)) (car '((a) (b) (c)))) '(grape a))
  (test-equal? "8" (run* (r) (fresh (x y) (caro '(grape rasin pear) x) (caro '((a) (b) (c)) y) (== (cons x y) r))) '((grape a)))

  (test-equal? "10" (cdr '(grape rasin pear)) '(rasin pear))
  (test-equal? "11" (car (cdr (cdr '(a c o r n)))) 'o)
 ))