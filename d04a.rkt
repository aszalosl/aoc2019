#lang racket
(define (to-digits n list)
  (if (= n 0) list
      (to-digits (quotient n 10) (cons (remainder n 10) list))))
(define (increasing list)
  (define (inner x list)
    (cond
     [(empty? list) #t]
     [(<= x (first list)) (inner (first list) (rest list))]
     [else #f]))
  (inner (first list) (rest list)))

(define (double list)
  (define (inner x list counter)
    (cond
      [(empty? list) counter]
      [(= x (first list)) (inner (first list) (rest list) (add1 counter))]
      [else (inner (first list) (rest list) counter)]))
  (< 0 (inner (first list) (rest list) 0)))

(define (double2 list)
  (define (inner x list counter multi)
    (cond
      [(empty? list) (cons counter multi)]
      [(and (not (= x (first list))) (< 0 counter))
       (inner (first list) (rest list) 0 (cons counter multi))]
      [(and (not (= x (first list))) (= 0 counter))
       (inner (first list) (rest list) 0 multi)]
      [(= x (first list))
       (inner x (rest list) (add1 counter) multi)]))
  (not (not (member 1  (inner (first list) (rest list) 0 '())))))
    
(define (condition number)
  (let ([list (to-digits number '())])
    (and (increasing list) (double list))))

(define (condition2 number)
  (let ([list (to-digits number '())])
    (and (increasing list) (double2 list))))

(define (puzzle bottom top condition)
  (length (for/list ([i (in-range bottom (add1 top))] #:when (condition i)) i)))

;(puzzle 136760 595730 condition)
;(puzzle 136760 595730 condition2)
(module+ test
  (require rackunit)
  (check-equal? (condition 111111) #t)
  (check-equal? (condition 223450) #f)
  (check-equal? (condition 123789) #f)
  (check-equal? (condition2 112233) #t)
  (check-equal? (condition2 123444) #f)
  (check-equal? (condition2 111122) #t))
