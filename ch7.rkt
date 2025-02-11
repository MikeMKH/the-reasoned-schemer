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

; 15
(defrel (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

; 43
(define (build-num n)
  (cond
    ((odd? n)
     (cons 1 (build-num (/ (- n 1) 2))))
    ((and (not (zero? n)) (even? n))
     (cons 0 (build-num (/ n 2))))
    ((zero? n) '())))

; 77
(defrel (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

; 83
(defrel (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

; 104
(defrel (addero b n m r)
  (conde
   ((== 0 b) (== '() m) (== n r))
   ((== 0 b) (== '() n) (== m r) (poso m))
   ((== 1 b) (== '() m) (addero 0 n '(1) r))
   ((== 1 b) (== '() n) (poso m) (addero 0 '(1) m r))
   ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero b 1 1 a c)))
   ((== '(1) n) (gen-addero b n m r))
   ((== '(1) m) (>1o n) (>1o r) (addero b '(1) n r))
   ((>1o n) (gen-addero b n m r))))

(defrel (gen-addero b n m r)
  (fresh (a c d e x y z)
    (== `(,a . ,x) n)
    (== `(,d . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero b a d c e)
    (addero e x y z)))

; 114
(defrel (+o n m k)
  (addero 0 n m k))

; 116
(defrel (-o n m k)
  (+o m k n))

; 120
 (define (length l)
   (cond
     ((null? l) 0)
     (#t (+ 1 (length (cdr l))))))

(defrel (lengtho l n)
  (conde
    ((nullo l) (== '() n))
    ((fresh (d res)
       (cdro l d)
       (+o '(1) res n)
       (lengtho d res)))))

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

  ; full-addero is defrel outside of run-tests
  (test-equal? "15" (run* (r c) (full-addero 0 1 1 r c)) '((0 1)))
  (test-equal? "16" (run* (r c) (full-addero 1 1 1 r c)) '((1 1)))
  (test-equal? "17" (run* (b x y r c) (full-addero b x y r c))
    '((0 0 0 0 0)
      (1 0 0 1 0)
      (0 0 1 1 0)
      (1 0 1 0 1)
      (0 1 0 1 0)
      (0 1 1 0 1)
      (1 1 0 0 1)
      (1 1 1 1 1)))

  ; build-num is define outside of run-tests
  (test-equal? "21" (build-num 0) '())
  (test-equal? "23" (build-num 1) '(1))
  (test-equal? "24" (build-num 5) '(1 0 1))
  (test-equal? "25" (build-num 7) '(1 1 1))
  (test-equal? "26" (build-num 9) '(1 0 0 1))
  (test-equal? "28" (build-num 6) '(0 1 1))
  (test-equal? "30" (build-num 19) '(1 1 0 0 1))
  (test-equal? "31" (build-num 1729) '(1 0 0 0 0 0 1 1 0 1 1))
  (test-equal? "40" (build-num 36) '(0 0 1 0 0 1))

  ; poso is defrel outside of run-tests
  (test-equal? "77" (run* (q) (poso '(0 1 1))) '(_.0))
  (test-equal? "78" (run* (q) (poso '(1))) '(_.0))
  (test-equal? "79" (run* (q) (poso '())) '())
  (test-equal? "80" (run* (r) (poso r)) '((_.0 . _.1)))

  ; >1o is defrel outside of run-tests
  (test-equal? "83" (run* (q) (>1o '(0 1 1))) '(_.0))
  (test-equal? "84" (run* (q) (>1o '(0 1))) '(_.0))
  (test-equal? "85" (run* (q) (>1o '(1))) '())
  (test-equal? "86" (run* (q) (>1o '())) '())
  (test-equal? "87" (run* (r) (>1o r)) '((_.0 _.1 . _.2)))

  ; addero is defrel outside of run-tests
  (test-equal? "90" (run 3 (x y r) (addero 0 x y r)) '((_.0 () _.0) (() (_.0 . _.1) (_.0 . _.1)) ((1) (1) (0 1))))
  (test-equal? "112" (run* (x y) (addero 0 x y '(1 0 1)))
    '(((1 0 1) ())
      (() (1 0 1))
      ((1) (0 0 1))
      ((0 0 1) (1))
      ((0 1) (1 1))
      ((1 1) (0 1))))

  ; gen-addero is defrel outside of run-tests
  (test-equal? "106" (run* (s) (gen-addero 1 '(0 1 1) '(1 1) s)) '((0 1 0 1)))

  ; +o is defrel outside of run-tests
  (test-equal? "114" (run* (x y) (+o x y '(1 0 1)))
    '(((1 0 1) ())
      (() (1 0 1))
      ((1) (0 0 1))
      ((0 0 1) (1))
      ((0 1) (1 1))
      ((1 1) (0 1))))

  ; -o is defrel outside of run-tests
  (test-equal? "117" (run* (q) (-o '(0 0 0 1) '(1 0 1) q)) '((1 1)))
  (test-equal? "118" (run* (q) (-o '(0 1 1) '(0 1 1) q)) '(()))
  (test-equal? "119" (run* (q) (-o '(1 0 1) '(0 0 0 1) q)) '()) ; negative is not defined
  
  (test-equal? "+o extra" (run 3 (x y z) (+o x y z)) '((_.0 () _.0) (() (_.0 . _.1) (_.0 . _.1)) ((1) (1) (0 1))))
  (test-equal? "-o extra" (run 3 (x y z) (-o x y z)) '((_.0 _.0 ()) ((_.0 . _.1) () (_.0 . _.1)) ((0 1) (1) (1))))

  ; lengtho is defrel outside of run-tests
  (test-equal? "121" (run 1 (n) (lengtho '(jicama rhubarb guava) n)) '((1 1)))
  (test-equal? "122" (run* (ls) (lengtho ls '(1 0 1))) '((_.0 _.1 _.2 _.3 _.4)))
  (test-equal? "123" (run* (q) (lengtho '(1 0 1) 3)) '())
  (test-equal? "123 extra" (run* (q) (lengtho '(1 0 1) (build-num 3))) '(_.0))
  (test-equal? "124" (run 3 (q) (lengtho q q)) '(() (1) (0 1)))
  ;(test-equal? "125" (run 4 (q) (lengtho q q)) '(() (1) (0 1) nothing-else)) ; there is not a 4th
 ))