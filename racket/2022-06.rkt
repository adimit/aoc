#lang racket

(require rackunit)

(define (sequences str unique-len)
  (define (iter s lst)
    (if ((string-length s) . <= . unique-len)
        (cons s lst)
        (iter (substring s 1) (cons (substring s 0 unique-len) lst))))
  (reverse (iter str '())))

(check-equal? (sequences "abcdefg" 4)
              '("abcd" "bcde" "cdef" "defg"))

(define (check-signal str)
  (eq? (string-length str)
       (length (remove-duplicates (string->list str)))))

(check-equal? (check-signal "abcd") #t)
(check-equal? (check-signal "abca") #f)

(define (run-task unique-len str )
  (+ unique-len
     (index-of (map check-signal (sequences str unique-len))
               #t)))

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
