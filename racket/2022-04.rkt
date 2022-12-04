#lang racket

(require rackunit)

(define sample "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(struct range (start end)
  #:transparent)

(define (string->range str)
  (match-let ([(list l r)(string-split str "-")])
    (range (string->number l) (string->number r))))

(define (parse-pair str)
  (match-let([(list no1 no2) (string-split str ",")])
    (cons (string->range no1) (string->range no2))))

(define (range-contains r1 r2)
  (or (and (>= (range-start r1) (range-start r2))
           (>= (range-end r2) (range-end r1)))
      (and (>= (range-start r2) (range-start r1))
           (>= (range-end r1) (range-end r2)))))

(define (run-task matchfn input)
  (length (filter (lambda (line)
                    (match-let ([(cons r1 r2) (parse-pair line)])
                      (matchfn r1 r2)))
                  (string-split input "\n"))))

(check-equal? (string->range "112-457") (range 112 457))
(check-equal? (parse-pair "2-4,12-14") (cons (range 2 4) (range 12 14)))
(check-equal? (range-contains (range 1 4) (range 3 5)) #f)
(check-equal? (range-contains (range 1 4) (range 1 4)) #t)
(check-equal? (range-contains (range 1 4) (range 2 3)) #t)
(check-equal? (range-contains (range 1 4) (range 0 5)) #t)
(check-equal? (range-contains (range 1 4) (range 0 1)) #f)

(check-equal? (run-task range-contains sample) 2)

(define (range-overlaps r1 r2)
  (or (and (<= (range-start r1) (range-end r2)) (>= (range-end r1) (range-start r2)))
      (and (<= (range-start r2) (range-end r1)) (>= (range-end r2) (range-start r1)))))

(check-equal? (range-overlaps (range 1 4) (range 3 5)) #t)
(check-equal? (range-overlaps (range 1 4) (range 1 4)) #t)
(check-equal? (range-overlaps (range 1 4) (range 2 3)) #t)
(check-equal? (range-overlaps (range 1 4) (range 0 5)) #t)
(check-equal? (range-overlaps (range 1 4) (range 0 1)) #t)
(check-equal? (range-overlaps (range 1 4) (range 4 5)) #t)
(check-equal? (range-overlaps (range 1 4) (range 5 6)) #f)
(check-equal? (range-overlaps (range 7 8) (range 5 6)) #f)

(run-task range-contains (file->string "input-04"))

(run-task range-overlaps (file->string "input-04"))
