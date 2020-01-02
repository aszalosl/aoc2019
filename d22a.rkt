#lang racket

; deal into new stack -> reverse

(define (cut deck n)
  (if (positive? n)
    (append (drop deck n) (take deck n))
    (let ([size (length deck)])
      (cut deck (modulo n size)))))

(define (increment deck n)
  (define (inner deck size n c hs)
    (cond
      [(empty? deck) (for/list ([k (in-range size)]) (hash-ref hs k))]
      [else (let ([next (modulo (+ c n) size)])
              (hash-set! hs c (first deck))
              (inner (rest deck) size n next hs))]))
  (inner deck (length deck) n 0 (make-hash)))

(define (execute orders deck)
  (if (empty? orders)
    deck
    (let ([order (first orders)])
      (cond 
        [(equal? order "deal into new stack") 
         (execute (rest orders) (reverse deck))]
        [(equal? (substring order 0 3) "cut")
         (let ([param (string->number (substring order 4))])
           (execute (rest orders) (cut deck param)))]
        [(equal? (substring order 0 19) "deal with increment")
         (let ([param (string->number (substring order 20))])
           (execute (rest orders) (increment deck param)))]
        [else (printf "Error: ~a" order)]))))

(define (find key list n)
  (cond
    [(= key (first list)) n]
    [else (find key (rest list) (add1 n))]))

(define (puzzle1 file-name size)
  (let* ([orders (file->lines file-name)])
    (execute orders (build-list size values))))

;(find 2019 (puzzle1 "d22.txt" 10007) 0)

;(module+ test
;  (require rackunit)
;  (check-equal? (puzzle1 "d22a.txt" 10) '(0 3 6 9 2 5 8 1 4 7))
;  (check-equal? (puzzle1 "d22b.txt" 10) '(3 0 7 4 1 8 5 2 9 6))
;  (check-equal? (puzzle1 "d22c.txt" 10) '(6 3 0 7 4 1 8 5 2 9))
;  (check-equal? (puzzle1 "d22d.txt" 10) '(9 2 5 8 1 4 7 0 3 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (extended a b)
  (define (inner a b c d e f)
    (cond
      [(zero? b) (list c e)]
      [(let ([r (modulo a b)] [q (quotient a b)])
         (inner b r d (- c (* q d)) f (- e (* q f))))]))
  (inner a b 1 0 0 1))

(define (backwards reverse-orders position size)
  (if (empty? reverse-orders)
      position
      (let ([order (first reverse-orders)])
        (cond 
          [(equal? order "deal into new stack") 
           (backwards (rest reverse-orders) (- size position 1) size)]
          [(equal? (substring order 0 3) "cut")
           (let ([param (string->number (substring order 4))])
             (backwards (rest reverse-orders) (modulo (+ position param) size) size))]
          [(equal? (substring order 0 19)  "deal with increment")
           (let* ([param (string->number (substring order 20))]
                  [s (first (extended param size))]
                  [new-pos (modulo (* position s) size)])
             (backwards (rest reverse-orders) new-pos size))]
          [else (printf "Error: ~a" order)]))))

(define (pow-mod a n mod)
  (define (prod-mod x xs m)
    (cond
      [(empty? xs) x]
      [else (prod-mod (modulo (* x (first xs)) m) (rest xs) m)]))
  (define (inner y n ys m)
    (let ([y2 (modulo (* y y) m)])
    (cond
      [(zero? n) ys]
      [(odd? n) (inner y2 (quotient n 2) (cons y ys) m)]
      [(even? n) (inner y2 (quotient n 2) ys m)])))
  (prod-mod 1 (inner a n '() mod) mod))

(define (puzzle2 file-name position size repetition)
  (let* ([orders (file->lines file-name)]
         [b (backwards (reverse orders) 0 size)]
         [a (- (backwards (reverse orders) 1 size) b)]
         [an (pow-mod a repetition size)]
         [inv-a-1 (pow-mod (sub1 a) (- size 2) size)]
         [bn (modulo (* b (sub1 an) inv-a-1) size)])
    (modulo (+ (* an position) bn) size)))

(puzzle2 "d22.txt" 2020 119315717514047 101741582076661)