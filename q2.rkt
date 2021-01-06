#lang racket
;Alexander Kuhn
;ID 101023154
(define (special-cons x y)
  (lambda (m) (m x y)))

(define (special-car a)
   (a (lambda (x y) x)))

(define (special-cdr b)
   (b (lambda (x y) y)))

(define (triple x y z)
  (lambda (m) (m x y z)))

(define (first f)
   (f (lambda (x y z) x)))

(define (second g)
   (g (lambda (x y z) y)))

(define (third h)
   (h (lambda (x y z) z)))

;special-car tests
(display "(special-car (special-cons 1 2))=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(special-car (special-cons 1 2))
(display "(special-car (special-cons 180 360))=> ")(newline)
   (display "Expected: 180")(newline)
   (display "Actual: ")(special-car (special-cons 180 360))(newline)

;special-cdr tests
(display "(special-cdr (special-cons 1 2))=> ")(newline)
   (display "Expected: 2")(newline)
   (display "Actual: ")(special-cdr (special-cons 1 2))
(display "(special-cdr (special-cons 180 360))=> ")(newline)
   (display "Expected: 360")(newline)
   (display "Actual: ")(special-cdr (special-cons 180 360))(newline)

;triple tests
(define a (triple 1 2 3))
(display "(first a)=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(first a)

(display "(second a)=> ")(newline)
   (display "Expected: 2")(newline)
   (display "Actual: ")(second a)

(display "(third a)=> ")(newline)
   (display "Expected: 3")(newline)
   (display "Actual: ")(third a)