#lang racket

(define sample "noop
addx 3
addx -5")

(define large-sample
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(define (execute-instructions str)
  (reverse
   (flatten
    (foldl
     (lambda (s r)
       (cond
         [(regexp-match? "noop" s) (cons (car r) r)]
         [(regexp-match "addx ([0-9-]+)" s)
          => (lambda (ms)
               (cons (+ (car r) (string->number (second ms))) (cons (car r) r)))]))
     '(1)
     (string-split str "\n" )))))

(define (signal-strength l pos)
  (* pos (list-ref l (- pos 1))))

(define (run-task str)
  (let ([l (execute-instructions str)])
    (+ (signal-strength l 20)
       (signal-strength l 60)
       (signal-strength l 100)
       (signal-strength l 140)
       (signal-strength l 180)
       (signal-strength l 220))))

(require rackunit)
(check-equal? (run-task large-sample) 13140)

(define (compute-pixels str)
  (let ([l (execute-instructions str)])
         (map (lambda (x i)
                (if ((abs (- (modulo i 40) x)) . <= . 1)
                    #\#
                    #\.))
              l
              (range 0 (length l)))))

(define (run-task-2 str)
  (define (render-screen l result)
    (if (>= (length l) 40)
        (let-values ([(line r) (split-at l 40)])
          (render-screen r (cons (list->string line) result)))
        (reverse result)))
  (render-screen (compute-pixels str) '()))
