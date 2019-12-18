#lang racket
; input beolvasása
(define (numbers file-path) (map string->number (string-split (first (file->lines file-path)) ",")))

; ha a regiszter tartalma mondja meg, hogy mit kell használnunk
(define (indirect n memory) (list-ref memory (list-ref memory n)))

; op-code feletti rész értelmezése
(define (get-mode code id)
  (cond
    [(= id 1) (remainder (quotient code 100) 10)]
    [(= id 2) (remainder (quotient code 1000) 10)]
    [else (quotient code 10000)]))

; a két címzés egységes kezelése
(define (get-arg memory mode addr)
  (if (zero? mode) (indirect addr memory) (list-ref memory addr)))

; egy utasítás végrehajtásával járó állapotváltozás
(define (step args4)
  ; (printf "@ ~a execute ~a in ~a \n"  pc (list-ref memory pc) memory)
  (let* ([pc     (first args4)]                ; program counter
         [memory (second args4)]               ; a memória aktuális állapota
         [in     (third args4)]                ; input puffer
         [out    (fourth args4)]               ; output puffer?
         [next (list-ref memory pc)]           ; aktuális memóriacím tartalma
         [op-code (remainder next 100)])       ; a művelet redukált kódja
    ;(printf " ~a:  " op-code)
    (cond [(= op-code 1) ; ADD
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2))]
                  [addr (if (zero? (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (+ op1 op2))])
             ;(printf "~a + ~a -> ~a" op1 op2 addr)
             (list (+ 4 pc) new-memory in out))]
          [(= op-code 2) ; MUL
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ pc 2))]
                  [addr (if (zero? (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (* op1 op2))])
             ;(printf "~a * ~a -> ~a" op1 op2 addr)
             (list (+ 4 pc) new-memory in out))]
          [(= op-code 3)                 ; INPUT
           (if (empty? in)
               (list pc memory in out)
               (let* ([addr (list-ref memory (+ pc 1))]
                      [new-memory (list-set memory addr (first in))])
                 ;(printf "IN ~a -> ~a" (first in) addr)
                 (list (+ 2 pc) new-memory (rest in) out)))]
          [(= op-code 4) ; OUTPUT
           (let* ([value (get-arg memory (get-mode next 1) (+ pc 1))])
             ;(printf "Output: ~a\n" value)
                 ;(printf "Out ~a" value)
             (list (+ 2 pc) memory in (cons value out)))]
          [(= op-code 5)                 ; J-True
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [addr  (get-arg memory (get-mode next 2) (+ pc 2))])
             ;(if (zero? value) (printf "Continue") (printf "Jump to ~a" addr)) 
             (if (zero? value) (list (+ 3 pc) memory in out) (list addr memory in out)))]
          [(= op-code 6)                 ; J-False
           (let* ([value (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [addr  (get-arg memory (get-mode next 2) (+ 2 pc))])
             ;(if (zero? value) (printf "Jump to ~a" addr) (printf "Continue") ) 
             (if (zero? value) (list addr memory in out) (list (+ 3 pc) memory in out)))]
          [(= (remainder next 100) 7)                 ; less-than
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc))]
                  [addr (if (= 0 (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (if (< op1 op2) 1 0))])
             ;(printf "Compare ~a < ~a" op1 op2)
             (list (+ 4 pc) new-memory in out))]
          [(= op-code 8)                 ; equal
           (let* ([op1 (get-arg memory (get-mode next 1) (+ 1 pc))]
                  [op2 (get-arg memory (get-mode next 2) (+ 2 pc))]
                  [addr (if (= 0 (get-mode next 3)) (list-ref memory (+ pc 3)) (raise 'failed))]
                  [new-memory (list-set memory addr (if (= op1 op2) 1 0))])
             ;(printf "Compare ~a < ~a" op1 op2)
             (list (+ 4 pc) new-memory in out))]
          [else
           ;(printf "STOP\n")
           (list pc memory in out)])))      ; STOP

; adott lista összes permutációjának elkészítése
(define (permute lst)
  (cond
    [(= (length lst) 1) (list lst)]
    [else (apply append (map 
                          (lambda (i) (map (lambda (j) (cons i j)) 
                                           (permute (remove i lst)))) lst))]))

; fusson addig, amíg STOP utasítást nem kap - egy processzor
(define (run lst)   ; returns back the memory
  (let ([lst2 (step lst)])
    (if (= 99 (list-ref (second lst2) (first lst2)))
        (second lst2) (run lst2))))

(define (step5 lst)
  ;(printf "\n#~a " (remainder (first lst) 5))
  (if (not (equal? '(99 99 99 99 99) (map (lambda (x) (list-ref (second x) (first x))) (rest lst))))
    ;(printf "all: ~a\n" lst)
    (let* ([new (step (second lst))] ; aktuális gép egy lépést megtett
           [next (third lst)]       ; soron következő gép állapota
           [last (list-ref (second new) (first new))]) ; 
      ;(when (and (= (remainder (first lst) 5) 4)
      ;           (not (empty? (fourth new))))
      ;  (printf "~a\n~a\n~a\n\n" (first lst) new next))
      ;(printf "step:~a\nnew: ~a\nnext ~a\n" (first lst) new next)
      ;(printf "   ~a" (drop (second new) 26))
      (step5 (cons (add1 (first lst))
                   (cons (list (first next) (second next) (append (third next) (fourth new)) '())
                         (append (cdddr lst) (list (list (first new) (second new) (third new) '())))))))
    (first (third (second lst)))))
             
  
(define (run5 seq memory)
  (step5 (list 0
          (list 0 memory (list (first seq)  0) '())
          (list 0 memory (list (second seq) ) '())
          (list 0 memory (list (third seq)  ) '())
          (list 0 memory (list (fourth seq) ) '())
          (list 0 memory (list (fifth seq)  ) '()))))

(define (find-max memory seq)
  (apply max (map (lambda (lst) (run5 lst memory)) (permute seq))))

(module+ test
  (require rackunit)
  (check-equal? (run5 '(9 8 7 6 5) (list 3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5)) 139629729)
  (check-equal? (run5 '(9 7 8 5 6) (list 3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10))  18216)
  (check-equal? (find-max (list 3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5) '(5 6 7 8 9)) 139629729)
  (check-equal? (find-max (list 3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10) '(5 6 7 8 9)) 18216))

;puzzle 1
;(find-max (numbers "d07.txt") '(5 6 7 8 9))

