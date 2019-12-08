#lang racket
(define (numbers file-path)
  (call-with-input-file file-path
    (lambda (port)
     (map string->number (string-split (read-line port) ",")))))

(define (replace-nth n item list)
  ;(printf "modify to ~a @ ~a \n" item n)
  (append (take list n) (cons item (drop list (add1 n)))))

(define (nth n list)
  (cond [(= n 0) (car list)]
        [else (nth (- n 1) (cdr list))]))

(define (value n memory)
  (nth (nth n memory) memory))

(define (step pc memory)
  (printf "@ ~a execute ~a in ~a \n"  pc (nth pc memory) (take (drop memory pc) 10))
  (let* [(next (nth pc memory))
        (mode1 (remainder (quotient next 100) 10))
        (mode2 (remainder (quotient next 1000) 10))
        (mode3 (quotient next 10000))]
    (cond [(= (remainder next 100) 1) ; ADD
           (let* ([op1 (if (= 0 mode1) (value (+ pc 1) memory) (nth (+ pc 1) memory))]
                  [op2 (if (= 0 mode2) (value (+ pc 2) memory) (nth (+ pc 2) memory))]
                  [addr (if (= 0 mode3) (nth (+ pc 3) memory) (raise 'failed))]
                  [new-memory (replace-nth addr (+ op1 op2) memory)])
             (step (+ 4 pc) new-memory))]
          [(= (remainder next 100) 2) ; MUL
           (let* ([op1 (if (= 0 mode1) (value (+ pc 1) memory) (nth (+ pc 1) memory))]
                  [op2 (if (= 0 mode2) (value (+ pc 2) memory) (nth (+ pc 2) memory))]
                  [addr (if (= 0 mode3) (nth (+ pc 3) memory) (raise 'failed ))]
                  [new-memory (replace-nth addr (* op1 op2) memory)])
                 (step (+ 4 pc) new-memory))]
          [(= next 3)                 ; INPUT
           (let* ([addr (nth (+ pc 1) memory)]
                  [new-memory (replace-nth addr 1 memory)])  ; we give 1 as an input
             (step (+ 2 pc) new-memory))]
          [(= (remainder next 100) 4) ; OUTPUT
           (let* ([value (if (= mode1 0) (value (+ pc 1) memory) (nth (+ pc 1) memory))])
             (printf "Output: ~a\n" value)
             (step (+ 2 pc) memory))]
          [else (begin (printf "exit: ~a\n" next)
                  memory)])))

;(module+ test
;  (require rackunit)
;  (check-equal? (step 0 '(1002 4 3 4 33)) '(1002 4 3 4 99)))

(define (puzzle1 memory)
  (car (step 0 memory)))
(puzzle1 (numbers "d05.txt"))
