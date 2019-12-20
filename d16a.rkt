#lang racket
(define signal '(1 2 3 4 5 6 7 8))
(define pattern '(0 1 0 -1))
(define (digit signal signal-length pattern i)
  (remainder (abs (apply +(for/list ([s signal]
                                     [k (in-range 1 signal-length)])
                            (* s (list-ref pattern (remainder (floor (/ k i)) 4)))))) 10))
(define (phrase signal pattern)
  (let ([signal-length (add1 (length signal))])
  (for/list ([i (in-range 1 signal-length)])
      (digit signal signal-length pattern i))))

(define (phrases signal pattern n)
  (cond
    [(zero? n) (take signal 8)]
    [else (phrases (phrase signal pattern) pattern (sub1 n))]))

(define (print-phrases signal pattern n)
  (cond
    [(zero? n) (take signal 8)]
    [else
     (begin
       (printf "~a\n" signal)
       (print-phrases (phrase signal pattern) pattern (sub1 n)))]))

(define (string->signal str)
   (for/list ([i (in-range (string-length str))])
    (- (char->integer (string-ref str i)) 48)))

(module+ test
  (require rackunit)
  (check-equal? (phrases (string->signal "80871224585914546619083218645595") pattern 100) '(2 4 1 7 6 1 7 6))
  (check-equal? (phrases (string->signal "19617804207202209144916044189917") pattern 100) '(7 3 7 4 5 4 1 8))
  (check-equal? (phrases (string->signal "69317163492948606335995924319873") pattern 100) '(5 2 4 3 2 1 3 3)))

;(phrases (string->signal (first (file->lines "d16.txt"))) pattern 100)