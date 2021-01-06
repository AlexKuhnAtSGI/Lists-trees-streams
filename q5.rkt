#lang racket
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons a b)(cons a (delay b)))))

(define (stream-car s)(car s))
(define (stream-cdr s)(force (cdr s)))

(define the-empty-stream '())
(define (stream-null? str)
  (null? str))


(define (stream-filter predicate stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((predicate (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter predicate (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (reverse L)
  (define (iter forward reverse)
    (if (null? forward) reverse
        (iter (cdr forward) (cons (car forward) reverse))))
  (iter L '()))

;my functions

(define (stream-first n str)
  (define (iter n str lis)
    (if (= n 0)
        (list->stream (reverse lis))
        (iter (- n 1) (stream-cdr str) (cons (stream-car str) lis))))
  (iter n str '())
  )

(define (list->stream lis)
  (if (null? lis)
      the-empty-stream
      (stream-cons (car lis) (list->stream (cdr lis)))))

(define (stream->list str)
  (if (stream-null? str)
      '()
      (cons (stream-car str) (stream->list (stream-cdr str)))))

(define infinite-ones
  (stream-cons 1 infinite-ones))

(define odds
  (stream-cons 1 (stream-filter odd? (integers-starting-from 3))))

(define (fIterative x)
  (define (f-Step a b c curr)
    (cond ((< curr 4) a)
          (else (f-Step (+ a (* 2 b) (* 3 c)) a b (- curr 1))))
    )
  (if (< x 4) x
  (f-Step 3 2 1 x)
  ))

(define (value-gen z)
  (stream-cons (fIterative z) (value-gen (+ 1 z))))

(define values
  (value-gen 1))

(define (combine f str1 str2)
  (stream-cons (f (stream-car str1) (stream-car str2))
               (combine f (stream-cdr str1) (stream-cdr str2))))

;stream generator tests (makes more sense doing these first, don't you think?)
(display "(stream-ref infinite-ones 1)=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(stream-ref infinite-ones 1)
(display "(stream-ref infinite-ones 1000)=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(stream-ref infinite-ones 1000)

(display "(stream-ref odds 0)=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(stream-ref odds 0)
(display "(stream-ref odds 1000)=> ")(newline)
   (display "Expected: 2001")(newline)
   (display "Actual: ")(stream-ref odds 1000)
;N.B.: because streams are 0-indexed, (stream-ref values n) will be equivalent to (fIterative (+ n 1)
(display "(stream-ref values 2)=> ")(newline)
   (display "Expected: 3")(newline)
   (display "Actual: ")(stream-ref values 2)
(display "(stream-ref values 19)=> ")(newline)
   (display "Expected: 9426875")(newline)
   (display "Actual: ")(stream-ref values 19)(newline)

;stream processing functions (testing stream->list in conjunction with stream-first)
(display "(stream->list (stream-first 4 values))=> ")(newline)
   (display "Expected: '(1 2 3 10)")(newline)
   (display "Actual: ")(stream->list (stream-first 4 values))
(display "(stream->list (stream-first 7 infinite-ones))=> ")(newline)
   (display "Expected: '(1 1 1 1 1 1 1)")(newline)
   (display "Actual: ")(stream->list(stream-first 7 infinite-ones))(newline)
(display "(stream->list (stream-first 10 odds))=> ")(newline)
   (display "Expected: '(1 3 5 7 9 11 13 15 17 19)")(newline)
   (display "Actual: ")(stream->list(stream-first 10 odds))(newline)

(display "(stream-cdr(list->stream '(1 2 3 10)))=> ")(newline)
   (display "Expected: '(2. #<promise:...l/comp3007/a3/q5.rkt:4:30>)")(newline)
   (display "Actual: ")(stream-cdr(list->stream '(1 2 3 10)))
(display "(stream-cdr (stream-cdr(list->stream '(1 2 7 10 19 77))))=> ")(newline)
   (display "Expected: '(7. #<promise:...l/comp3007/a3/q5.rkt:4:30>)")(newline)
   (display "Actual: ")(stream-cdr (stream-cdr(list->stream '(1 2 7 10 19 77))))(newline)

;combine testing
(display "(stream-car (combine + (integers-starting-with 1) (integers-starting-from 4)))=> ")(newline)
   (display "Expected: 5")(newline)
   (display "Actual: ")(stream-car (combine + (integers-starting-from 1) (integers-starting-from 4)))
(display "(stream-cdr (combine - (integers-starting-with 1) (integers-starting-from 4)))=> ")(newline)
   (display "Expected: '(-3. #<promise:...l/comp3007/a3/q5.rkt:4:30>)")(newline)
   (display "Actual: ")(stream-cdr (combine - (integers-starting-from 1) (integers-starting-from 4)))
(display "(stream-cdr (combine * (integers-starting-with 1) (integers-starting-from 4)))=> ")(newline)
   (display "Expected: '(18. #<promise:...l/comp3007/a3/q5.rkt:4:30>)")(newline)
   (display "Actual: ")(stream-cdr (stream-cdr (combine * (integers-starting-from 1) (integers-starting-from 4))))