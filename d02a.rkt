#lang racket
(define (numbers file-path)
  (call-with-input-file file-path
    (lambda (port)
     (map string->number (string-split (read-line port) ",")))))

(define (replace-nth n item list)
  (cond [(= n 0) (cons item (cdr list))]
        [else (cons (car list) (replace-nth (- n 1) item (cdr list)))]))

(define (nth n list)
  (cond [(= n 0) (car list)]
        [else (nth (- n 1) (cdr list))]))

(define (value n memory)
  (nth (nth n memory) memory))

(define (step pc memory)
  (let [(next (nth pc memory))]
    (cond [(= next 1)
           (let* ([op1 (value (+ pc 1) memory)]
                 [op2 (value (+ pc 2) memory)]
                 [op3 (nth (+ pc 3) memory)]
                 [new-memory (replace-nth op3 (+ op1 op2) memory)])
                 (step (+ 4 pc) new-memory))]
          [(= next 2)
           (let* ([op1 (value (+ pc 1) memory)]
                 [op2 (value (+ pc 2) memory)]
                 [op3 (nth (+ pc 3) memory)]
                 [new-memory (replace-nth op3 (* op1 op2) memory)])
                 (step (+ 4 pc) new-memory))]
          [else memory])))

(module+ test
  (require rackunit)
  (check-equal? (step 0 '(1 0 0 0 99)) '(2 0 0 0 99))
  (check-equal? (step 0 '(2 3 0 3 99)) '(2 3 0 6 99))
  (check-equal? (step 0 '(2 4 4 5 99 0)) '(2 4 4 5 99 9801))
  (check-equal? (step 0 '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99)))
(define (puzzle1 x y memory)
  (let [(newprogram (append (list 1 x y) (cdddr memory)))]
    (car (step 0 newprogram))))
(puzzle1 12 2 (numbers "d02.txt"))
(define (puzzle2 magic)
  (let [(memory (numbers "d02.txt"))]
    (for*/list ([i (in-range 100)] [j (in-range 100)] #:when (= (puzzle1 i j memory) magic)
                               ) (+ (* 100 i) j))))