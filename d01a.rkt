#lang racket
(define (numbers file-path)
  (call-with-input-file file-path
    (lambda (port)
     (for/list ([line (in-lines port)])
       (string->number line)))))

(define (calc n)
  (- (floor (/ n 3)) 2))

(define (rec-calc n)
  (define (rc n acc)
    (let [(new (calc n))]
      (if (<= new 0)
          acc
          (rc new (+ acc new)))))
  (rc n 0))

(module+ test
  (require rackunit)
  (check-equal? (calc  12) 2)
  (check-equal? (calc 14) 2)
  (check-equal? (calc 1969) 654)
  (check-equal? (calc 100756) 33583)
  (check-equal? (rec-calc 14) 2)
  (check-equal? (rec-calc 1969) 966)
  (check-equal? (rec-calc 100756) 50346))

(define (puzzle1)
  (apply + (map calc (numbers "d01.txt"))))
(puzzle1)
(define (puzzle2)
  (apply + (map rec-calc (numbers "d01.txt"))))
(puzzle2)

