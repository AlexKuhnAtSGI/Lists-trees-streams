#lang racket
;Alexander Kuhn
;ID 101023154
(define (make-interval a b)
  (cons a b))

(define (lower-bound I)
  (if (> (cdr I) (car I)) (car I) (cdr I)))

(define (upper-bound I)
  (if (= (cdr I) (lower-bound I)) (car I) (cdr I)))

(define (display-interval I)
  (if (not (null? I))
  (printf "~a, ~a" (lower-bound I) (upper-bound I))
  (printf "error")))

(define (contains i x)
  (if (or (= (car i) x) (= (cdr i) x)) #t #f))

(define (add-interval i1 i2)
  (make-interval (+ (upper-bound i1) (upper-bound i2)) (+ (lower-bound i1) (lower-bound i2))))

(define (subtract-interval i1 i2)
  (make-interval (- (upper-bound i1) (lower-bound i2)) (- (lower-bound i1) (upper-bound i2))))

;upper bound/lower bound are irrelevant here, as we multiply together all possible combinations
(define (multiply-interval i1 i2)
  (make-interval (min (* (car i1) (car i2)) (* (car i1) (cdr i2)) (* (cdr i1) (car i2)) (* (cdr i1) (cdr i2)))
                 (max (* (car i1) (car i2)) (* (car i1) (cdr i2)) (* (cdr i1) (car i2)) (* (cdr i1) (cdr i2)))))

(define (divide-interval i1 i2)
  (if (contains i2 0) '()
  (multiply-interval i1 (make-interval (/ 1 (upper-bound i2)) (/ 1 (lower-bound i2))))))

(define l1 (make-interval 2 3))
(define l2 (make-interval 4 5))
(define l3 (make-interval 0 12))
(define l4 (make-interval 19 21))

;add-interval tests
(display "(add-interval l1 l2)=> ")(newline)
   (display "Expected: 6, 8")(newline)
   (display "Actual: ")(display-interval (add-interval l1 l2))(newline)

(display "(add-interval l1 l3)=> ")(newline)
   (display "Expected: 2, 15")(newline)
   (display "Actual: ")(display-interval (add-interval l1 l3))(newline)

(display "(add-interval l1 l4)=> ")(newline)
   (display "Expected: 21, 24")(newline)
   (display "Actual: ")(display-interval (add-interval l1 l4))(newline)

;subtract-interval tests
(newline)
(display "(subtract-interval l1 l2)=> ")(newline)
   (display "Expected: -3, -1")(newline)
   (display "Actual: ")(display-interval (subtract-interval l1 l2))(newline)

(display "(subtract-interval l1 l3)=> ")(newline)
   (display "Expected: -10, 3")(newline)
   (display "Actual: ")(display-interval (subtract-interval l1 l3))(newline)

(display "(subtract-interval l1 l4)=> ")(newline)
   (display "Expected: -19, -16")(newline)
   (display "Actual: ")(display-interval (subtract-interval l1 l4))(newline)

;multiply-interval tests
(newline)
(display "(multiply-interval l1 l2)=> ")(newline)
   (display "Expected: 8, 15")(newline)
   (display "Actual: ")(display-interval (multiply-interval l1 l2))(newline)

(display "(multiply-interval l2 l3)=> ")(newline)
   (display "Expected: 0, 60")(newline)
   (display "Actual: ")(display-interval (multiply-interval l2 l3))(newline)

(display "(multiply-interval l1 l4)=> ")(newline)
   (display "Expected: 38, 63")(newline)
   (display "Actual: ")(display-interval (multiply-interval l1 l4))(newline)

;divide-interval tests
(newline)
(display "(divide-interval l1 l2)=> ")(newline)
   (display "Expected: 2/5, 3/4")(newline)
   (display "Actual: ")(display-interval (divide-interval l1 l2))(newline)

(display "(divide-interval l2 l3)=> ")(newline)
   (display "Expected: error")(newline)
   (display "Actual: ")(display-interval (divide-interval l2 l3))(newline)

(display "(divide-interval l1 l4)=> ")(newline)
   (display "Expected: 2/21, 3/19")(newline)
   (display "Actual: ")(display-interval (divide-interval l1 l4))(newline)