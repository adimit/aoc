#lang racket

(define sample
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(define file-input (file->string "input-03"))

(require rackunit)

(define (priority item)
  (if (char-upper-case? item)
      (+ 26 (- (char->integer item) (- (char->integer #\A) 1)))
      (- (char->integer item) (- (char->integer #\a) 1))))

(struct backpack (left right)
  #:transparent)

(define (parse-input str)
  (map (lambda (line)
         (let-values
             ([(l r) (split-at (string->list line) (/ (string-length line) 2))])
           (backpack l r)))
       (string-split str "\n")))


(define (find-common bp)
  (set-intersect (backpack-left bp) (backpack-right bp)))

(define (run-task input)
  (apply + (map (compose priority first find-common)
                (parse-input input))))

(define (sum-to-n n) (/ (+ n (* n n)) 2))
(check-equal?
 (apply + (map
           priority
           (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
 (sum-to-n 52)
 "Priority over alphabet should sum up to (n+n²)/2 (1 + … + n)")
(check-equal? (run-task sample) 157 "Check against the provided sample")

(run-task file-input)

(define (run-task-2 input)
  (define (iter lst sigma)
    (if (empty? lst) sigma
        (let-values ([(group rest) (split-at lst 3)])
          (iter rest (+ sigma (priority (first (apply set-intersect (map string->list group)))))))))
  (iter (string-split input "\n") 0))

(check-equal? (run-task-2 sample) 70)

(run-task-2 file-input)
