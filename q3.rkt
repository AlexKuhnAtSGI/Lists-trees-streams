#lang racket
;Alexander Kuhn
;ID 101023154
(define (count x L)
  (define (count-iter x L num)
    (cond ((null? L) num)
          ((equal? (car L) x) (count-iter x (cdr L) (+ 1 num)))
          (else (count-iter x (cdr L) num))
          )
    )
  (count-iter x L 0))

(define (mode L)
  (define (mode-iter L high curr)
    (cond ((null? L) curr)
          ((> (count (car L) L) high) (mode-iter (cdr L) (count (car L) L) (car L)))
          (else (mode-iter (cdr L) high curr))
          )
    )
    (mode-iter L 0 0))

(define (after L x)
  (define (last-iter x L num)
    (cond ((null? L) '())
          ((= x num) L)
          (else (last-iter x (cdr L) (+ 1 num)))))
  (last-iter x L 0))

;reverse is taken from the study guide to the midterm
(define (reverse L)
  (define (iter forward reverse)
    (if (null? forward) reverse
        (iter (cdr forward) (cons (car forward) reverse))))
  (iter L '()))

;append is taken from  section 4.4 of the course notes
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1)
              (append (cdr list1) list2))))

(define (splice L i A)
  (define (splice-iter L i A Z count)
    (cond ((= i count) (append (reverse Z) (append A L)))
          (else (splice-iter (cdr L) i A (cons (car L) Z) (+ 1 count)))))
  (splice-iter L i A '() 0))

(define (splice2 L i n A)
  (define (splice2-iter L i n A countN countI top)
    (cond ((= n countN) (splice (append (reverse top) L) i A))
          ((and (= i countI) (null? L)) (splice2-iter L i n A (+ 1 countN) countI top))
          ((= i countI) (splice2-iter (cdr L) i n A (+ 1 countN) countI top))
          (else (splice2-iter (cdr L) i n A countN (+ 1 countI) (cons (car L) top)))))
  (splice2-iter L i n A 0 0 '()))

;count tests
(display "(count 3 '(1 4 3 6 2 3 3 1 4 3 5 7))=> ")(newline)
   (display "Expected: 4")(newline)
   (display "Actual: ")(count 3 '(1 4 3 6 2 3 3 1 4 3 5 7))
(display "(count 'b '(4 b a 3 2 c b 1 b 2 d a))=> ")(newline)
   (display "Expected: 3")(newline)
   (display "Actual: ")(count 'b '(4 b a 3 2 c b 1 b 2 d a))(newline)

;mode tests
(display "(mode '(a b a c a d d a b c a b))=> ")(newline)
   (display "Expected: 'a")(newline)
   (display "Actual: ")(mode '(a b a c a d d a b c a b))
(display "(mode '(2 b a 3 2 c b 1 b 2 d a))=> ")(newline)
   (display "Expected: 2")(newline)
   (display "Actual: ")(mode '(2 b a 3 2 c b 1 b 2 d a))(newline)

;after tests
(display "(after '(a b c d e f g h) 3)=> ")(newline)
   (display "Expected: '(d e f g h)")(newline)
   (display "Actual: ")(after '(a b c d e f g h) 3)
(display "(after '(a b c d e f g h) 0)=> ")(newline)
   (display "Expected: '(a b c d e f g h)")(newline)
   (display "Actual: ")(after '(a b c d e f g h) 0)(newline)

;splice tests
(display "(splice '(1 2 3 4 5) 2 '(a b c))=> ")(newline)
   (display "Expected: '(1 2 a b c 3 4 5)")(newline)
   (display "Actual: ")(splice '(1 2 3 4 5) 2 '(a b c))
(display "(splice '(1 2 3 4 5) 0 '(a b c))=> ")(newline)
   (display "Expected: '(a b c 1 2 3 4 5)")(newline)
   (display "Actual: ")(splice '(1 2 3 4 5) 0 '(a b c))(newline)

;splice2 tests
(display "(splice2 '(1 2 3 4 5) 2 1 '(a b c))=> ")(newline)
   (display "Expected: '(1 2 a b c 4 5)")(newline)
   (display "Actual: ")(splice2 '(1 2 3 4 5) 2 1 '(a b c))
(display "(splice2 '(1 2 3 4 5) 2 0 '(a b c))=> ")(newline)
   (display "Expected: '(1 2 a b c 3 4 5)")(newline)
   (display "Actual: ")(splice2 '(1 2 3 4 5) 2 0 '(a b c))
(display "(splice2 '(1 2 3 4 5) 3 4 '(a b c))=> ")(newline)
   (display "Expected: '(1 2 3 a b c)")(newline)
   (display "Actual: ")(splice2 '(1 2 3 4 5) 3 4 '(a b c))(newline)