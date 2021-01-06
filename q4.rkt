#lang racket
;Alexander Kuhn
;ID 101023154
(define (height t)
  (cond ((null? t) 0)
        ((list? t) (max (+ 1 (height (car t))) (height (cdr t))))
        (else 0)))

(define (reverse L)
  (define (iter forward reverse)
    (if (null? forward) reverse
        (iter (cdr forward) (cons (car forward) reverse))))
  (iter L '()))

(define (tree-filter p t)
  (cond ((null? t) '())
        ((list? (car t)) (cons (tree-filter p (car t)) (tree-filter p (cdr t))))
        ((p (car t)) (cons (car t) (tree-filter p (cdr t))))
        (else (tree-filter p (cdr t)))))

(define (flattenList t)
  (define (flatten-iter t X)
    (cond ((null? t) X)
        ((list? t) (flatten-iter (cdr t) (flatten-iter (car t) X)))
        (else (cons t X))))
  (reverse (flatten-iter t '())))

;height tests
(display "(height 'a)=> ")(newline)
   (display "Expected: 0")(newline)
   (display "Actual: ")(height 'a)
(display "(height '(a))=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(height '(a))
(display "(height '(a (b) c))=> ")(newline)
   (display "Expected: 2")(newline)
   (display "Actual: ")(height '(a (b) c))
(display "(height '(((((a(((b)))))))))=> ")(newline)
   (display "Expected: 8")(newline)
   (display "Actual: ")(height '(((((a(((b)))))))))(newline)

;tree-filter tests
(display "(tree-filter even? '(1 (2 3) ((4 5) (6 7)) (((8 (9))))))=> ")(newline)
   (display "Expected: '((2) ((4) (6)) (((8 ()))))")(newline)
   (display "Actual: ")(tree-filter even? '(1 (2 3) ((4 5) (6 7)) (((8 (9))))))
(display "(tree-filter odd '(1 (2 3) ((4 5) (6 7)) (((8 (9))))))=> ")(newline)
   (display "Expected: '(1 (3) ((5) (7)) ((((9)))))")(newline)
   (display "Actual: ")(tree-filter odd? '(1 (2 3) ((4 5) (6 7)) (((8 (9))))))(newline)

;flattenList tests
(display "(flattenList '(1 (2 3) ((4 5 6 (7)))(((8 (9))))))=> ")(newline)
   (display "Expected: '(1 2 3 4 5 6 7 8 9)")(newline)
   (display "Actual: ")(flattenList '(1 (2 3) ((4 5 6 (7)))(((8 (9))))))
(display "(flattenList '(((((a(((b)))))))))=> ")(newline)
   (display "Expected: '(a b)")(newline)
   (display "Actual: ")(flattenList '(((((a(((b)))))))))(newline)