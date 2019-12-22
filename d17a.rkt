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
    [(= mode 2) (hash-ref memory (+ rb (hash-ref memory addr 0)) 0)]))

(define (get-addr memory mode addr rb)
  (cond
    [(= mode 0) (hash-ref memory addr 0)]
    [(= mode 1) (raise 'failed)]
    [(= mode 2) (+ (hash-ref memory addr) rb)]))

(define (step pc rb memory in out type)
  (let* [(next (hash-ref memory pc 0))
         (op-code (remainder next 100))]
    (cond [(= op-code 1) ; ADD
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (+ op1 op2))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 2) ; MUL
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (* op1 op2))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 3)                 ; INPUT
           (let* ([addr (get-addr memory (get-mode next 1) (+ pc 1) rb)])
             (hash-set! memory addr (first in))
             (step (+ 2 pc) rb memory (rest in) out type))]
          [(= op-code 4) ; OUTPUT
           (let* ([value (get-arg memory (get-mode next 1) (+ pc 1) rb)])
             (step (+ 2 pc) rb memory in (cons value out) type))]
          [(= op-code 5)                 ; J-True
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ pc 2) rb)])
             (if (zero? value)
                 (step (+ 3 pc) rb memory in out type)
                 (step addr rb memory in out type)))]
          [(= op-code 6)                 ; J-False
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ 2 pc) rb)])
             (if (zero? value)
                 (step addr rb memory in out type)
                 (step (+ 3 pc) rb memory in out type)))]
          [(= (remainder next 100) 7)                 ; less-than
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (if (< op1 op2) 1 0))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 8)                 ; equal
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (if (= op1 op2) 1 0))
             (step (+ 4 pc) rb memory in out type))]
          [(= op-code 9)
           (let ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)])
             (step (+ 2 pc) (+ op1 rb) memory in out type))]
          [else (if (equal? type 'debug) memory (reverse out))])))

(define (list->pic lst line others)
  (cond
    [(empty? lst) (reverse (cons (apply string (reverse line)) others))]
    [(= (first lst) 46) (list->pic (rest lst) (cons #\. line) others)]
    [(= (first lst) 35) (list->pic (rest lst) (cons #\# line) others)]
    [(= (first lst) 94) (list->pic (rest lst) (cons #\^ line) others)]
    [(= (first lst) 10) (list->pic (rest lst) '()  (cons (apply string (reverse line)) others))]))

(define (list->pairs lst row col pairs)
  (cond
    [(empty? lst) pairs]
    [(= (first lst) 46) (list->pairs (rest lst) row (add1 col) pairs)]
    [(= (first lst) 35) (list->pairs (rest lst) row (add1 col) (cons (cons col row) pairs))]
    [(= (first lst) 94) (printf "~a ~a\n" col row)
                        (list->pairs (rest lst) row (add1 col) pairs)]
    [(= (first lst) 10) (list->pairs (rest lst) (add1 row) 0 pairs)]))

(define (neighbours pair my-map)
  (let ([x (car pair)][y (cdr pair)])
    (and (= (hash-ref my-map (cons (sub1 x) y) 9) 1)
         (= (hash-ref my-map (cons (add1 x) y) 9) 1)
         (= (hash-ref my-map (cons x (sub1 y)) 9) 1)
         (= (hash-ref my-map (cons x (add1 y)) 9) 1))))

(define (puzzle file-name start)
  (let* ([output (step 0 0 (my-hash (numbers file-name)) (list start) '() 'run)]
         [pairs (list->pairs output 0 0 '())]
         [my-map (for/hash ([p (in-list pairs)]) (values p 1))]
         [sections (for/list ([(k v) (in-hash my-map)] #:when (neighbours k my-map)) k)])
    (apply + (map (λ (p) (* (car p) (cdr p))) sections))))

;(puzzle "d17.txt" 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dx '(0 1 0 -1))
(define dy '(-1 0 1 0))

(define (next x y dir)
  (let ([nx (list-ref dx dir)] [ny (list-ref dy dir)]) (cons (+ x nx) (+ y ny))))

(define (search-path x y dir last other my-map)
  ;(printf "~a:~a/~a\n" x y dir)
  (let ([nxy (next x y dir)]
        [lxy (next x y (modulo (sub1 dir) 4))]
        [rxy (next x y (modulo (add1 dir) 4))])
    (cond
      [(= (hash-ref my-map nxy 0) 1) (search-path (car nxy) (cdr nxy) dir (add1 last) other my-map)]
      [(= (hash-ref my-map lxy 0) 1) (search-path (car lxy) (cdr lxy) (modulo (sub1 dir) 4) 1 (cons "L" (cons last other)) my-map)]
      [(= (hash-ref my-map rxy 0) 1) (search-path (car rxy) (cdr rxy) (modulo (add1 dir) 4) 1 (cons "R" (cons last other)) my-map)]
      [else (cons last other)])))

(define (search file-name start)
  (let* ([output (step 0 0 (my-hash (numbers file-name)) (list start) '() 'run)]
         [pairs (list->pairs output 0 0 '())]
         [my-map (for/hash ([p (in-list pairs)]) (values p 1))])
    (print my-map)
    (reverse (search-path 0 10 0 1 '() my-map))))

;(search "d17.txt" 1)
     
(define (draw-puzzle file-name start)
  (let* ([output (step 0 0 (my-hash (numbers file-name)) (list start) '() 'run)]
         [pic (list->pic output '() '())])
    pic))

;movement program:
;A,B,A,C,A,B,C,C,A,B
;R,8,L,10,R,8
;R,12,R,8,L,8,L,12
;L,12,L,10,L,8
;n

(define (puzzle2 file-name)
  (step 0 0 (my-hash (cons 2 (rest (numbers file-name)))) '(65 44 66 44 65 44 67 44 65 44 66 44 67 44 67 44 65 44 66 10 82 44 56 44 76 44 49 48 44 82 44 56 10 82 44 49 50 44 82 44 56 44 76 44 56 44 76 44 49 50 10 76 44 49 50 44 76 44 49 48 44 76 44 56 10 110 10)  '() 'run))
;(puzzle2 "d17.txt") ; -> 833429