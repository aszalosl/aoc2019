#lang racket

; 4 directions
(define dx '(0 1 0 -1))
(define dy '(-1 0 1 0))
; read input
(define (numbers file-path) (map string->number (string-split (first (file->lines file-path)) ",")))

; model the memory of Intcode computer
(define (my-hash ns) (make-hash (map cons (range (length ns)) ns)))

; indirect read from memory (based on value of a register)
(define (indirect n memory) (hash-ref  memory (hash-ref memory n 0) 0))

; mode of an argument
(define (get-mode code id)
  (cond
    [(= id 1) (remainder (quotient code 100) 10)]
    [(= id 2) (remainder (quotient code 1000) 10)]
    [else (quotient code 10000)]))

; how to understand the mode
(define (get-arg memory mode addr rb)
  (cond
    [(= mode 0) (indirect addr memory)]
    [(= mode 1) (hash-ref memory addr 0)]
    [(= mode 2) (hash-ref memory (+ rb (hash-ref memory addr 0)) 0)]))

; how to store the result
(define (get-addr memory mode addr rb)
  (cond
    [(= mode 0) (hash-ref memory addr 0)]
    [(= mode 1) (raise 'failed)]
    [(= mode 2) (+ (hash-ref memory addr) rb)]))

; one step of the machine
; pc - program counter
; rb - relative base
; memory - memory as a hash
; hull - as a hash
; x y - actual position
; dir - direction and status
; in - buffered input - removed !
; out - buffered output - removed !
; type - debug/run - removed !

(define (step pc rb memory hull x y dir)
  (let* [(next (hash-ref memory pc 0))
         (op-code (remainder next 100))]
    (cond [(= op-code 1) ; ADD
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (+ op1 op2))
             (step (+ 4 pc) rb memory hull x y dir))]
          [(= op-code 2) ; MUL
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (* op1 op2))
             (step (+ 4 pc) rb memory hull x y dir))]
          [(= op-code 3)                 ; INPUT
           (let* ([addr (get-addr memory (get-mode next 1) (+ pc 1) rb)])
             (hash-set! memory addr (hash-ref hull (cons x y) 0))
             (step (+ 2 pc) rb memory hull x y dir))]
          [(= op-code 4) ; OUTPUT
           (let* ([value (get-arg memory (get-mode next 1) (+ pc 1) rb)])
             (if (<  dir 4)
                 (begin 
                   (hash-set! hull (cons x y) value)
                   ;(printf "~a ~a to ~a\n" x y value)
                   (step (+ 2 pc) rb memory hull x y (+ (modulo dir 4) 4)))
                 (let ([new-dir (modulo (+ dir value value -1) 4)])
                   ;(printf "new dir: ~a (~a)\n" new-dir value)
                   (step (+ 2 pc) rb memory hull (+ x (list-ref dx (modulo new-dir 4))) (+ y (list-ref dy (modulo new-dir 4))) new-dir))))]
          [(= op-code 5)                 ; J-True
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ pc 2) rb)])
             (if (zero? value)
                 (step (+ 3 pc) rb memory hull x y dir)
                 (step addr rb memory hull x y dir)))]
          [(= op-code 6)                 ; J-False
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ 2 pc) rb)])
             (if (zero? value)
                 (step addr rb memory hull x y dir)
                 (step (+ 3 pc) rb memory hull x y dir)))]
          [(= (remainder next 100) 7)                 ; less-than
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (if (< op1 op2) 1 0))
             (step (+ 4 pc) rb memory hull x y dir))]
          [(= op-code 8)                 ; equal
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (if (= op1 op2) 1 0))
             (step (+ 4 pc) rb memory hull x y dir))]
          [(= op-code 9)
           (let ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)])
             (step (+ 2 pc) (+ op1 rb) memory hull x y dir))]
          [else  hull])))


(define (puzzle1 file-name)
  ;    pc rb memory                       hull        x y dir 
  (step 0 0 (my-hash (numbers file-name)) (make-hash) 0 0 0))

(define (puzzle2 file-name)
  (let* ([h (step 0 0 (my-hash (numbers file-name)) (make-hash (list (cons (cons 0 0) 1))) 0 0 0)]
         [hk (hash-keys h)]
         [hkx (map car hk)]
         [hky (map cdr hk)]
         [minx (apply min hkx)]
         [maxx (apply max hkx)]
         [miny (apply min hky)]
         [maxy (apply max hky)])
    (for ([y (in-range miny (add1 maxy))])
      (for ([x (in-range minx (add1 maxx))])
        (if (zero? (hash-ref h (cons x y) 0)) (printf ".") (printf "*")))
      (printf "\n"))))
         
    

     
  
