#lang racket

(require racket/stream)
(require rackunit)

(define (sequences str unique-len)
  (for/stream ([i (range 0 (+ 1 (- (string-length str) unique-len)))])
    (substring str i (+ i unique-len))))

(check-equal? (stream->list (sequences "abcdefg" 4))
              '("abcd" "bcde" "cdef" "defg"))

(define (check-signal str)
  (eq? (string-length str)
       (length (remove-duplicates (string->list str)))))

(check-equal? (check-signal "abcd") #t)
(check-equal? (check-signal "abca") #f)

(define (stream-index-of p s)
  (define (iter s n)
    (if (eq? p (stream-first s))
        n
        (iter (stream-rest s) (+ 1 n))))
  (iter s 0))

(check-equal? (stream-index-of 10 (for/stream ([i (in-naturals)]) i)) 10)

(define (run-task unique-len str )
  (+ unique-len
     (stream-index-of #t (stream-map check-signal (sequences str unique-len)))))

(check-equal? (run-task 4 "bvwbjplbgvbhsrlpgdmjqwftvncz") 5)
(check-equal? (run-task 4 "nppdvjthqldpwncqszvftbrmjlhg") 6)
(check-equal? (run-task 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 10)
(check-equal? (run-task 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 11)

(check-equal? (run-task 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb") 19)
(check-equal? (run-task 14 "bvwbjplbgvbhsrlpgdmjqwftvncz") 23)
(check-equal? (run-task 14 "nppdvjthqldpwncqszvftbrmjlhg") 23)
(check-equal? (run-task 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 29)
(check-equal? (run-task 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 26)

(run-task 4 (file->string "input-06"))
(run-task 14 (file->string "input-06"))
