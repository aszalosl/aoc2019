#lang racket
(define (numbers file-path)
  (call-with-input-file file-path
    (lambda (port)
     (map string->number (string-split (read-line port) ",")))))

(define (replace-nth n item list)
  (append (take list n) (cons item (drop list (add1 n)))))

(define (nth n list)  (first (drop list n)))

(define (indirect n memory) (nth (nth n memory) memory))

(define (get-mode code id)
  (cond
    [(= id 1) (remainder (quotient code 100) 10)]
    [(= id 2) (remainder (quotient code 1000) 10)]
    [else (quotient code 10000)]))
         
(define (get-arg memory mode addr)
  (if (zero? mode) (indirect addr memory) (nth addr memory)))

(define (step pc memory in out type)
  ;(printf "@ ~a execute ~a in ~a \n"  pc (nth pc memory) memory)
  (let* [(next (nth pc memory))
         (op-code (remainder next 100))]
    (cond [(= op-code 1) ; ADD
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2))]
                  [addr (if (zero? (get-mode next 3)) (nth (+ pc 3) memory) (raise 'failed))]
                  [new-memory (replace-nth addr (+ op1 op2) memory)])
             (step (+ 4 pc) new-memory in out type))]
          [(= op-code 2) ; MUL
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2))]
                  [addr (if (zero? (get-mode next 3)) (nth (+ pc 3) memory) (raise 'failed))]
                  [new-memory (replace-nth addr (* op1 op2) memory)])
             (step (+ 4 pc) new-memory in out type))]
          [(= op-code 3)                 ; INPUT
           (let* ([addr (nth (+ pc 1) memory)]
                  [new-memory (replace-nth addr (first in) memory)])  ; we give 1 as an input
             (step (+ 2 pc) new-memory (rest in) out type))]
          [(= op-code 4) ; OUTPUT
           (let* ([value (get-arg memory (get-mode next 1) (+ pc 1))])
             ;(printf "Output: ~a\n" value)
             (step (+ 2 pc) memory in (cons value out) type))]
          [(= op-code 5)                 ; J-True
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [addr  (get-arg memory (get-mode next 2) (+ pc 2))])
             (if (zero? value) (step (+ 3 pc) memory in out type) (step addr memory in out type)))]
          [(= op-code 6)                 ; J-False
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [addr  (get-arg memory (get-mode next 2) (+ 2 pc))])
             (if (zero? value) (step addr memory in out type) (step (+ 3 pc) memory in out type)))]
          [(= (remainder next 100) 7)                 ; less-than
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc))]
                  [addr (if (= 0 (get-mode next 3)) (nth (+ pc 3) memory) (raise 'failed))]
                  [new-memory (replace-nth addr (if (< op1 op2) 1 0) memory)])
             (step (+ 4 pc) new-memory in out type))]
          [(= op-code 8)                 ; equal
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc))]
                  [addr (if (= 0 (get-mode next 3)) (nth (+ pc 3) memory) (raise 'failed))]
                  [new-memory (replace-nth addr (if (= op1 op2) 1 0) memory)])
             (step (+ 4 pc) new-memory in out type))]
          
          [else (if (equal? type 'debug) memory out)])))

(module+ test
  (require rackunit)
  (check-equal? (step 0 '(1002 4 3 4 33) '(1) '() 'debug) '(1002 4 3 4 99))
  (check-equal? (step 0 '(3 9 8 9 10 9 4 9 99 -1 8) '(1) '() 'debug) '(3 9 8 9 10 9 4 9 99 0 8)) ; output 0
  (check-equal? (step 0 '(3 9 8 9 10 9 4 9 99 -1 8) '(8) '() 'debug) '(3 9 8 9 10 9 4 9 99 1 8)) ; output 1
  (check-equal? (step 0 '(3 9 7 9 10 9 4 9 99 -1 8) '(1) '() 'debug) '(3 9 7 9 10 9 4 9 99 1 8)) ; output 0
  (check-equal? (step 0 '(3 9 7 9 10 9 4 9 99 -1 8) '(8) '() 'debug) '(3 9 7 9 10 9 4 9 99 0 8)) ; output 1
  (check-equal? (step 0 '(3 3 1108 -1 8 3 4 3 99 0 0) '(1) '() 'debug) '(3 3 1108 0 8 3 4 3 99 0 0)) ; output 0
  (check-equal? (step 0 '(3 3 1108 -1 8 3 4 3 99 0 0) '(8) '() 'debug) '(3 3 1108 1 8 3 4 3 99 0 0)) ; output 1
  (check-equal? (step 0 '(3 3 1107 -1 8 3 4 3 99 0 0) '(1) '() 'debug) '(3 3 1107 1 8 3 4 3 99 0 0)) ; output 1
  (check-equal? (step 0 '(3 3 1107 -1 8 3 4 3 99 0 0) '(8) '() 'debug) '(3 3 1107 0 8 3 4 3 99 0 0)) ; output 0
  )

;puzzle 1
(step 0 (numbers "d05.txt") '(1) '() 'run)
(step 0 (numbers "d05.txt") '(5) '() 'run)

