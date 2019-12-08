#lang racket
(define size (* 25 6))
(define (layer n code) (substring code (* n size) (* (add1 n) size)))
(define (count str n c0 c1 c2)
  ;(printf "~a ~a ~a ~a\n" n c0 c1 c2)
  (cond
    [(< n 0) (list c0 (* c1 c2))]
    [(equal? (string-ref str n) #\0) (count str (sub1 n) (add1 c0) c1 c2)]
    [(equal? (string-ref str n) #\1) (count str (sub1 n) c0 (add1 c1) c2)]
    [(equal? (string-ref str n) #\2) (count str (sub1 n) c0 c1 (add1 c2))]))

(define (puzzle1 filename)
  (let* ([all-code (first (file->lines filename))]
         [counts (for/list ([i (in-range (/ (string-length all-code) size))])
                   (count (layer i all-code) (sub1 size) 0 0 0))]
         [mini (apply min (map first counts))])
    (cdar (filter (lambda (x) (= 5 (first x))) counts))))

;(puzzle1 "d08.txt")

(define (puzzle2 filename)
 (let ([code (first (file->lines filename))]
       [my-hash (make-hash)])
   (for ([i (in-range (string-length code))])
     (let* ([ch (string-ref code i)]
            [y (quotient (remainder i size) 25)]
            [x (remainder i 25)])
       (unless (equal? ch #\2) (hash-ref! my-hash (cons x y) ch))))
   (for ([y (in-range 6)])
     (begin
       (for ([x (in-range 25)])
         (let ([ch (hash-ref my-hash (cons x y) "-")])
           (if (equal? ch #\1) (printf "1") (printf "."))))
       (printf "\n")))))

;(puzzle2 "d08.txt")
