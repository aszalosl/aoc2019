#lang racket

(define (num str)
  (string->number (substring str 1)))

(define (seq x y dx dy c N)
  (for/list
      ([i (in-range 1 (add1 N))])
    (list (+ x (* dx i)) (+ y (* dy i)) (+ c i))))

(define (wire list x y c points)
  (if (empty? list)
      points
      (let* ([letter (string-ref (car list) 0)]
             [N (num (car list))])
        (cond
          [(eq? letter #\R) (wire (cdr list) (+ x N) y (+ c N) (append (seq x y  1 0 c N) points))]
          [(eq? letter #\L) (wire (cdr list) (- x N) y (+ c N) (append (seq x y -1 0 c N) points))]
          [(eq? letter #\U) (wire (cdr list) x (+ y N) (+ c N) (append (seq x y 0  1 c N) points))]
          [(eq? letter #\D) (wire (cdr list) x (- y N) (+ c N) (append (seq x y 0 -1 c N) points))]))))

(define (less-triple? t1 t2)
  (cond [(< (first t1) (first t2)) #t]
        [(> (first t1) (first t2)) #f]
        [(< (second t1) (second t2)) #t]
        [else #f]))

(define (common t1 t2 both)
  (cond [(empty? t1) both]
        [(empty? t2) both]
        [(less-triple? (first t1) (first t2)) (common (rest t1) t2 both)]
        [(less-triple? (first t2) (first t1)) (common t1 (rest t2) both)]
        [(equal? (take (first t1) 2) (take (first t2) 2))
         (common (rest t1) (rest t2) (cons (append (first t1) (list (third (first t2)))) both))]))
        
(define (common-wire wire1 wire2)
  (let* ([w1 (string-split wire1 ",")]
         [w2 (string-split wire2 ",")]
         [t1 (wire w1 0 0 0 '())]
         [ot1 (sort t1 less-triple?)]
         [t2 (wire w2 0 0 0 '())]
         [ot2 (sort t2 less-triple?)])
    (common ot1 ot2 '())))

(define (common-wire1 wire1 wire2)
  (apply min (map (lambda (x) (+ (abs (first x)) (abs (second x)))) (common-wire wire1 wire2))))

(define (common-wire2 wire1 wire2)
  (let* ([both (common-wire wire1 wire2)])
         (apply min (map (lambda (x) (+ (third x) (fourth x))) both))))
  
(module+ test
  (require rackunit)
  (check-equal?  (common-wire1 "R8,U5,L5,D3" "U7,R6,D4,L4") 6)
  (check-equal?  (common-wire1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83") 159)
  (check-equal?  (common-wire1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 135)
  (check-equal?  (common-wire2 "R8,U5,L5,D3" "U7,R6,D4,L4") 30)
  (check-equal?  (common-wire2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83") 610)
  (check-equal?  (common-wire2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 410))

(define (puzzle1 filename)
  (let ([lines (file->lines filename)])
    (common-wire1 (first lines) (second lines))))
(define (puzzle2 filename)
  (let ([lines (file->lines filename)])
    (common-wire2 (first lines) (second lines))))

(puzzle1 "d03.txt")
(puzzle2 "d03.txt")