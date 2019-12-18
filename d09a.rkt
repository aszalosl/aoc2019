#lang racket
(define (numbers file-path) (map string->number (string-split (first (file->lines file-path)) ",")))

(define (my-hash ns) (make-hash (map cons (range (length ns)) ns)))

(define (indirect n memory) (hash-ref  memory (hash-ref memory n 0) 0))

(define (get-mode code id)
  (cond
    [(= id 1) (remainder (quotient code 100) 10)]
    [(= id 2) (remainder (quotient code 1000) 10)]
    [else (quotient code 10000)]))
         
(define (get-arg memory mode addr rb)
  (cond
    [(= mode 0) (indirect addr memory)]
    [(= mode 1) (hash-ref memory addr 0)]
    [(= mode 2) (hash-ref memory (+ rb (hash-ref memory addr)) 0)]))

(define (get-addr memory mode addr rb)
  (cond
    [(= mode 0) (hash-ref memory addr 0)]
    [(= mode 1) (raise 'failed)]
    [(= mode 2) (hash-ref memory (+ (hash-ref memory addr) rb) 0)]))

(define (step pc rb memory in out type)
  ;(printf "@ (~a,~a) execute ~a \n"  pc rb (hash-ref! memory pc 0))
  (let* [(next (hash-ref memory pc 0))
         (op-code (remainder next 100))]
    (cond [(= op-code 1) ; ADD
           (printf "@ (~a,~a) execute ADD (~a : ~a ~a ~a) \n"  pc rb
                   (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0) (hash-ref! memory (+ 2 pc) 0) (hash-ref! memory (+ 3 pc) 0))
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (+ op1 op2))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 2) ; MUL
           (printf "@ (~a,~a) execute MUL (~a : ~a ~a ~a) \n"  pc rb
                   (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0) (hash-ref! memory (+ 2 pc) 0) (hash-ref! memory (+ 3 pc) 0))
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (* op1 op2))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 3)                 ; INPUT
           (printf "@ (~a,~a) execute IN (~a : ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0))
           (let* ([addr (get-arg memory (get-mode next 1) (+ pc 1) rb)])
             (hash-set! memory addr (first in))
             (step (+ 2 pc) rb memory (rest in) out type))]
          [(= op-code 4) ; OUTPUT
           (printf "@ (~a,~a) execute OUT (~a : ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0))
           (let* ([value (get-arg memory (get-mode next 1) (+ pc 1) rb)])
             ;(printf "Output: v: ~a m:~a argv: ~a\n " value (get-mode next 1) (hash-ref memory (+ pc 1)))
             (step (+ 2 pc) rb memory in (cons value out) type))]
          [(= op-code 5)                 ; J-True
           (printf "@ (~a,~a) execute JNZ (~a : ~a ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0) (hash-ref! memory (+ 2 pc) 0))
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ pc 2) rb)])
             (if (zero? value)
                 (step (+ 3 pc) rb memory in out type)
                 (step addr rb memory in out type)))]
          [(= op-code 6)                 ; J-False
           (printf "@ (~a,~a) execute JZ (~a : ~a ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0) (hash-ref! memory (+ 2 pc) 0))
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ 2 pc) rb)])
             (if (zero? value)
                 (step addr rb memory in out type)
                 (step (+ 3 pc) rb memory in out type)))]
          [(= (remainder next 100) 7)                 ; less-than
           (printf "@ (~a,~a) execute < (~a : ~a ~a ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0) (hash-ref! memory (+ 2 pc) 0) (hash-ref! memory (+ 3 pc) 0))
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 1) (+ pc 3) rb)])
             (hash-set! memory addr (if (< op1 op2) 1 0))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 8)                 ; equal
           (printf "@ (~a,~a) execute = (~a : ~a ~a ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0) (hash-ref! memory (+ 2 pc) 0) (hash-ref! memory (+ 3 pc) 0))
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 1) (+ pc 3) rb)])
             ;(printf "= ~a ~a ~a\n" op1 op2 addr)
             (hash-set! memory addr (if (= op1 op2) 1 0))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 9)
           (printf "@ (~a,~a) execute MB (~a : ~a) \n"  pc rb (hash-ref! memory pc 0) (hash-ref! memory (+ 1 pc) 0))
           (let ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)])
             (step (+ 2 pc) (+ op1 rb) memory in out type))]
          [else (if (equal? type 'debug) memory (reverse out))])))

;(module+ test
;  (require rackunit)
;  (check-equal? (step 0 0 (my-hash '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)) '() '() 'run) '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))
;  (check-equal? (step 0 0 (my-hash '(1102 34915192 34915192 7 4 7 99 0)) '() '() 'run) '(1219070632396864))
;  (check-equal? (step 0 0 (my-hash '(104 1125899906842624 99)) '() '() 'run) '(1125899906842624)))

(define (puzzle1 file-name)
  (step 0 0 (my-hash (numbers file-name)) '(1) '() 'run))

(puzzle1 "d09.txt")