#lang racket
(define (numbers file-path) (map string->number (string-split (first (file->lines file-path)) ",")))

(define (indirect n memory) (list-ref  memory (list-ref memory n)))

(define (get-mode code id)
  (cond
    [(= id 1) (remainder (quotient code 100) 10)]
    [(= id 2) (remainder (quotient code 1000) 10)]
    [else (quotient code 10000)]))
         
(define (get-arg memory mode addr)
  (if (zero? mode) (indirect addr memory) (list-ref memory addr)))

(define (step pc memory in out type)
  ;(printf "@ ~a execute ~a in ~a \n"  pc (list-ref memory pc) memory)
  (let* [(next (list-ref memory pc))
         (op-code (remainder next 100))]
    (cond [(= op-code 1) ; ADD
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2))]
                  [addr (if (zero? (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (+ op1 op2))])
             (step (+ 4 pc) new-memory in out type))]
          [(= op-code 2) ; MUL
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2))]
                  [addr (if (zero? (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (* op1 op2))])
             (step (+ 4 pc) new-memory in out type))]
          [(= op-code 3)                 ; INPUT
           (let* ([addr (list-ref memory (+ pc 1))]
                  [new-memory (list-set memory addr (first in))])  ; we give 1 as an input
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
                  [addr (if (= 0 (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (if (< op1 op2) 1 0))])
             (step (+ 4 pc) new-memory in out type))]
          [(= op-code 8)                 ; equal
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc))]
                  [addr (if (= 0 (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (if (= op1 op2) 1 0))])
             (step (+ 4 pc) new-memory in out type))]
          
          [else (if (equal? type 'debug) memory out)])))
(define (step5 sequence memory start)
  (let* ([output1 (step 0 memory (list (first  sequence) start) '() 'run)]
         [output2 (step 0 memory (list (second sequence) (first output1)) '() 'run)]
         [output3 (step 0 memory (list (third  sequence) (first output2)) '() 'run)]
         [output4 (step 0 memory (list (fourth sequence) (first output3)) '() 'run)]
         [output5 (step 0 memory (list (fifth  sequence) (first output4)) '() 'run)])
    (first output5)))

(define (remove-first x lst)
  (cond
    [(= x (first lst)) (rest lst)]
    [else (cons (first lst) (remove-first x (rest lst)))]))

(define (permute lst)
  (cond
    [(= (length lst) 1) (list lst)]
    [else (apply append (map 
                          (lambda (i) (map (lambda (j) (cons i j)) 
                                           (permute (remove-first i lst)))) lst))]))
(define (find-max memory start)
  (apply max (map (lambda (lst) (step5 lst memory start)) (permute '(0 1 2 3 4)))))
(module+ test
  (require rackunit)
  (check-equal? (step5 '(4 3 2 1 0) '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0 ) 0) 43210)
  (check-equal? (step5 '(0 1 2 3 4) '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0) 0) 54321)
  (check-equal? (step5 '(1 0 4 3 2) '(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0) 0) 65210)
  ;(check-equal? (step5 '(9 8 7 6 5) '(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5) 0) 139629729)
  (check-equal? (permute '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
  (check-equal? (find-max'(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0 ) 0) 43210)
  (check-equal? (find-max '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0) 0) 54321)
  (check-equal? (find-max '(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0) 0) 65210)
  )


;puzzle 1
;(step 0 (numbers "d05.txt") '(5) '() 'run)
;(find-max (numbers "d07.txt") 0)
