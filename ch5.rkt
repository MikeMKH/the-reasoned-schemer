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
(define (mem x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) l)
    (#t (mem x (cdr l)))))

; 4
(defrel (memo x l out)
  (conde
   ((caro l x) (== l out))
   ((fresh (d)
      (cdro l d)
      (memo x d out)))))

(run-tests
 (test-suite "chapter 5"
  ; mem is define outside of run-tests
  (test-equal? "1" (mem 'fig '(roll okra fig beet roll pea)) '(fig beet roll pea))
  (test-equal? "2" (mem 'fig '(roll okra beet beet roll pea)) #f)
  (test-equal? "3" (mem 'roll (mem 'fig '(roll okra fig beet roll pea))) '(roll pea))

  ; memo is defrel outside of run-tests
  (test-equal? "5" (run* (q) (memo 'fig '(pea) '(pea))) '())
  (test-equal? "6" (run* (out) (memo 'fig '(fig) out)) '((fig)))
  (test-equal? "7" (run* (out) (memo 'fig '(fig pea) out)) '((fig pea)))
  (test-equal? "8" (run* (r) (memo r '(roll okra fig beet roll pea) '(fig beet roll pea))) '(fig))
  (test-equal? "9" (run* (x) (memo 'fig '(fig pea) `(pea ,x))) '())
  (test-equal? "10" (run* (x) (memo 'fig '(fig pea) `(,x pea))) '(fig))
  (test-equal? "11" (run* (out) (memo 'fig '(beet fig pea) out)) '((fig pea)))
  (test-equal? "13" (run 1 (out) (memo 'fig '(fig fig pea) out)) '((fig fig pea)))
  (test-equal? "14" (run* (out) (memo 'fig '(fig fig pea) out)) '((fig fig pea) (fig pea)))
  (test-equal? "18" (run* (out) (fresh (x) (memo 'fig `(a ,x c fig e) out))) '((fig c fig e) (fig e)))
  (test-equal? "19" (run 5 (x y) (memo 'fig `(fig d fig e . ,y) x))
    '(((fig d fig e . _.0) _.0)
     ((fig e . _.0) _.0)
     ((fig . _.0) (fig . _.0))
     ((fig . _.0) (_.1 fig . _.0))
     ((fig . _.0) (_.1 _.2 fig . _.0))))
 ))