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

(defrel (bit-xoro x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 1 r))
   ((== 1 x) (== 0 y) (== 1 r))
   ((== 1 x) (== 1 y) (== 0 r))))

(defrel (bit-ando x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 0 r))
   ((== 1 x) (== 0 y) (== 0 r))
   ((== 1 x) (== 1 y) (== 1 r))))

(defrel (half-addero x y r c)
  (bit-xoro x y r)
  (bit-ando x y c))

(defrel (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

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

(defrel (+o n m k)
  (addero 0 n m k))

(defrel (-o n m k)
  (+o k m n))

(require (except-in rackunit fail))
(require rackunit/text-ui)

; 11
(defrel (not-pastao x)
  (conda
   ((== 'pasta x) u)
   (s s)))

; 20
(defrel (teacupo t)
  (conde
   ((== 'tea t))
   ((== 'cup t))))

; 21
(defrel (onceo g)
  (condu
   (g s)
   (s u)))

; 26
(defrel (bumpo n x)
  (conde
   ((== n x))
   ((fresh (m)
      (-o n '(1) m)
      (bumpo m x)))))

; 27
(defrel (gen&test+o i j k)
  (onceo
   (fresh (x y z)
     (+o x y z)
     (== i x)
     (== j y)
     (== k z))))

; 58
(defrel (enumerate+o r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (+o i j k)
    (onceo
     (fresh (x y z)
       (+o x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== `(,i ,j ,k) r)))

; 59
(defrel (enumerateo op r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (onceo
     (fresh (x y z)
       (op x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== `(,i ,j ,k) r)))

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
  ;(test-equal? "13" (run* (q) (conda (alwayso s) (s u))) '(endless-loop)) ; builds an infinite list of _.0
  (test-equal? "14" (run* (q) (condu (alwayso s) (s u))) '(_.0))
  ;(test-equal? "15" (run* (q) (condu (s alwayso) (s u))) '(endless-loop))
  ;(test-equal? "17" (run 1 (q) (conda (alwayso s) (s u)) u) '(endless-loop)) ; continously fails
  (test-equal? "18" (run 1 (q) (condu (alwayso s) (s u)) u) '())

  ; teacupo and onceo are defrel outside of run-tests
  (test-equal? "21" (run* (x) (onceo(teacupo x))) '(tea))
  (test-equal? "22" (run* (r) (conde ((teacupo r) s) ((== #f r) s))) '(tea cup)) ; text has '(#f tea cup)
  (test-equal? "23" (run* (r) (conda ((teacupo r) s) (s (== #f r)))) '(tea cup))
  (test-equal? "24" (run* (r) (== #f r) (conda ((teacupo r) s) ((== #f r) s) (s u))) '()) ; text has '(#f)
  (test-equal? "25" (run* (r) (== #f r) (condu ((teacupo r) s) ((== #f r) s) (s u))) '()) ; text has '(#f)

  ; bumpo is defrel outside of run-tests
  (test-equal? "26" (run* (x) (bumpo '(1 1 1) x)) '((1 1 1) (0 1 1) (1 0 1) (0 0 1) (1 1) (0 1) (1) ()))

  ; gen&test+o is defrel outside of run-tests
  (test-equal? "27" (run* (q) (gen&test+o '(0 0 1) '(1 1) '(1 1 1))) '(_.0))
  ;(test-equal? "40" (run 1 (q) (gen&test+o '(0 0 1) '(1 1) '(0 1 1))) '(endless-loop)) ; continously fails

  ; enumerate+o is defrel outside of run-tests
  (test-equal? "43" (run* (s) (enumerate+o s '(1 1)))
    '(((1 1) () (1 1))
     ((1 1) (1 1) (0 1 1))
     ((1 1) (0 1) (1 0 1))
     ((0 1) (1 1) (1 0 1))
     ((1 1) (1) (0 0 1))
     ((1) (1 1) (0 0 1))
     (() (1 1) (1 1))
     ((0 1) () (0 1))
     ((0 1) (0 1) (0 0 1))
     ((0 1) (1) (1 1))
     ((1) (0 1) (1 1))
     ((1) (1) (0 1))
     ((1) () (1))
     (() (0 1) (0 1))
     (() (1) (1))
     (() () ())))
  (test-equal? "56" (run 1 (s) (enumerate+o s '(1 1 1))) '(((1 1 1) (1 1 1) (0 1 1 1))))

  ; enumerateo is defrel outside of run-tests
  (test-equal? "-o" (run* (s) (enumerateo -o s '(1 1)))
    '(((1 1) (1 1) ())
      ((1 1) (0 1) (1))
      ((1 1) () (1 1))
      ((0 1) (0 1) ())
      ((1 1) (1) (0 1))
      ((0 1) (1) (1))
      ((0 1) () (0 1))
      ((1) (1) ())
      ((1) () (1))
      (() () ())))
 ))