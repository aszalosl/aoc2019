#lang racket
(define (data filename)
  (make-immutable-hash 
   (map (lambda (line) (reverse (string-split line ")"))) (file->lines filename))))

(define (distance x n my-hash)
  ;(printf "hash: ~a ~a\n" x n) 
  (if (equal? x "COM") n (distance (first (hash-ref my-hash x)) (+ 1 n) my-hash)))

(define (path x list my-hash)
  (if (equal? x "COM") list (path (first (hash-ref my-hash x)) (cons x list) my-hash)))

(define (diff p1 p2)
  (if (equal? (first p1) (first p2))
      (diff (rest p1) (rest p2))
      (- (+ (length p1) (length p2)) 2)))

(define (puzzle1 filename)
  (let ([my-hash (data filename)])
    ;(printf "~a \n" my-hash)
    (apply + (map (lambda (x) (distance x 0 my-hash)) (hash-keys my-hash)))))

(define (puzzle2 filename)
  (let* ([my-hash (data filename)]
         [ps (path "SAN" '() my-hash)]
         [py (path "YOU" '() my-hash)])
    ;(printf "~a ~a\n" ps py)
    (diff ps py)))

(module+ test
  (require rackunit)
  (check-equal? (puzzle1 "d06a.txt") 42)
  (check-equal? (puzzle2 "d06b.txt") 4)
  )


; (puzzle1 "d06.txt")
