#lang racket

; 4 directions X N S W E
(define dx '(0 0  0 -1 1))
(define dy '(0 -1 1  0 0))

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

(define (backward path)
  (let* ([prev-dir (caddar path)])
                (cons (list 0 0 (list-ref '(0 -2 -1 -4 -3) prev-dir)) (rest path))))

(define (next path mind)
  (filter (λ (t) (= (hash-ref mind (take t 2) 3) 3))
             (for/list ([i (in-range 1 5)]) (list (+ (caar path) (list-ref dx i)) (+ (cadar path) (list-ref dy i)) i))))

(define (find-next path mind value)
  (cond
    [(or (= value 1) (= value 2))
     (let ([h (next path mind)])
       (cond
         [(and (empty? h) (empty? (cdr path))) '()]
         [(empty? h) (backward path)]
         [else  (cons (first h) path)]))]
    [(zero? value) ; go back
     (let ([h (next (rest path) mind)])
       (cond
         [(and (empty? h) (empty? (cddr path))) '()]
         [(empty? h) (backward (rest path))]
         [else  (cons (first h) (rest path))]))]))
               
; one step of the machine
; pc - program counter
; rb - relative base
; memory - memory as a hash
; mind - as a hash
; x y - actual position
; path - thread of ariadne

(define (step pc rb memory mind  path)
  (let* [(next (hash-ref memory pc 0))
         (op-code (remainder next 100))]
    (cond [(= op-code 1) ; ADD
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (+ op1 op2))
             (step (+ 4 pc) rb memory mind path))]
          [(= op-code 2) ; MUL
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (* op1 op2))
             (step (+ 4 pc) rb memory mind path))]
          [(= op-code 3)                 ; INPUT - dir
           (let* ([addr (get-addr memory (get-mode next 1) (+ pc 1) rb)]
                  [dir (caddar path)]) ;at 1/3
             (if (< dir 0)
                 (begin ; backward
                   (hash-set! memory addr (abs dir))
                   (step (+ 2 pc) rb memory mind (rest path)))
                 (begin ; forward
                   (hash-set! memory addr dir)
                   (step (+ 2 pc) rb memory mind path))))]
          [(= op-code 4) ; OUTPUT
           (let ([value (get-arg memory (get-mode next 1) (+ pc 1) rb)]
                 [stored (hash-ref mind (take (first path) 2) 99)])
             (hash-set! mind (take (first path) 2) value) ; store 
             (when (and (< stored 99) (not (= value stored))) (printf "ERROR: ~a ~a ~a\n" value stored path))
             (let ([new-path (find-next path mind value)])
                   (if (empty? new-path) mind
                       (step (+ 2 pc) rb memory mind new-path))))]
          [(= op-code 5)                 ; J-True
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ pc 2) rb)])
             (if (zero? value)
                 (step (+ 3 pc) rb memory mind path)
                 (step addr rb memory mind path)))]
          [(= op-code 6)                 ; J-False
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [addr  (get-arg memory (get-mode next 2) (+ 2 pc) rb)])
             (if (zero? value)
                 (step addr rb memory mind path)
                 (step (+ 3 pc) rb memory mind path)))]
          [(= (remainder next 100) 7)                 ; less-than
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (if (< op1 op2) 1 0))
             (step (+ 4 pc) rb memory mind path))]
          [(= op-code 8)                 ; equal
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc) rb)]
                  [addr (get-addr memory (get-mode next 3) (+ pc 3) rb)])
             (hash-set! memory addr (if (= op1 op2) 1 0))
             (step (+ 4 pc) rb memory mind path))]
          [(= op-code 9)
           (let ([op1 (get-arg memory (get-mode next 1) (+ 1 pc) rb)])
             (step (+ 2 pc) (+ op1 rb) memory mind path))]
          [else  mind])))

(define (connections mind)
  (for/list ([(key value) (in-hash mind)] #:when (= value 1)) key))

(define (goal mind)
  (for/list ([(key value) (in-hash mind)] #:when (= value 2)) key))

(define (less-list? xs ys)
  (cond
    [(empty? xs) #t]
    [(< (car xs) (car ys) ) #t]
    [else #f]))

(define (astar open mind goalx goaly)
  (let ([x (cadar open)] [y (caddar open)] [g (fourth (first open))])
    (if (and (= x goalx) (= y goaly)) g
        (begin
          (hash-set! mind (list x y) 4)
          (let* ([neigbours (for/list ([i (in-range 1 5)]) (list (+ x (list-ref dx i)) (+ y (list-ref dy i))))]
                 [valid-neigbours (filter (λ (t) (< 0 (hash-ref mind (take t 2) 4) 3)) neigbours)]
                 [children (map (λ (p) (list (+ (abs (- (first p) goalx)) (abs (- (second p) goaly)) g)
                                             (first p) (second p) (add1 g))) valid-neigbours)]
                 [new-open (sort (append children (rest open)) less-list?)])
         (astar new-open mind goalx goaly))))))

(define (bfs open mind oxygen n)
  (if (empty? open)
      (apply max (hash-values oxygen))
      (let* ([neigbours (for*/list ([i (in-range 1 5)] [p (in-list open)])
                          (list (+ (first p) (list-ref dx i)) (+ (second p) (list-ref dy i))))]
             [new-neigbours (filter (λ (t) (and (= (hash-ref oxygen t 9999) 9999)
                                                (= 1 (hash-ref mind t 3)))) neigbours)])   
      (bfs (for/list ([t (in-list new-neigbours)])
        (hash-set! oxygen t n)
        t) mind oxygen (add1 n)))))
            
(define (print-map mind)
  (let* ([hk (hash-keys mind)] [hkx (map first hk)] [hky (map second hk)]
     [minx (apply min hkx)] [maxx (apply max hkx)]
     [miny (apply min hky)] [maxy (apply max hky)])
  (for ([y (in-range miny (add1 maxy))])
    (for ([x (in-range minx (add1 maxx))])
      (let ([block (hash-ref mind (list x y) 4)])
        (cond
          [(= block 0) (printf "#")] [(= block 1) (printf ".")]
          [(= block 2) (printf "*")] [(= block 4) (printf "?")])))
    (printf "\n"))))

(define (puzzle1 file-name)
  ;    pc rb memory                       mind.map        x y dir 
  (let* ([mind (step 0 0 (my-hash (numbers file-name)) (make-hash) (list (list 0 -1 1) (list 0 0 1)))]
         [gxy (first (goal mind))]
         [gx (first gxy)]
         [gy (second gxy)])
    (astar (list (list (+ (abs gx) (abs gy)) 0 0 0)) mind gx gy)))
    ;(connections mind)))   
    ;(print-map mind)))

(define (puzzle2 file-name)
  ;    pc rb memory                       mind.map        x y dir 
  (let* ([mind (step 0 0 (my-hash (numbers file-name)) (make-hash) (list (list 0 -1 1) (list 0 0 1)))]
         [gxy (first (goal mind))]
         [gx (first gxy)]
         [gy (second gxy)])
    (bfs (list (list gx gy)) mind (make-hash (list (cons (list gx gy) 0))) 1)))

;(puzzle1 "d15.txt")
     
  
