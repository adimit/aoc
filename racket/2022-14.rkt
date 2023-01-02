#lang typed/racket

(require typed/pict)
(require typed/racket)
(require/typed data/bit-vector
  [#:opaque BitVector bit-vector?]
  [make-bit-vector (->* (Integer) (Boolean) BitVector)]
  [bit-vector (-> Boolean * BitVector)]
  [bit-vector-ref (->* (BitVector Integer) (Boolean) Boolean)]
  [bit-vector-set! (-> BitVector Integer Boolean Void)]
  [string->bit-vector (-> String BitVector)]
  [bit-vector->string (-> BitVector String)]
  [bit-vector-copy (->* (BitVector) (Integer Integer) BitVector)]
  [in-bit-vector (-> BitVector (Sequenceof Boolean))]
  [bit-vector-length (-> BitVector Exact-Nonnegative-Integer)]
  [bit-vector-popcount (-> BitVector Exact-Nonnegative-Integer)]
  [list->bit-vector (-> (Listof Boolean) BitVector)]
  [bit-vector->list (-> BitVector (Listof Boolean))])

(: sample String)
(define sample "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(struct extents ([xmin : Integer]
                 [xmax : Integer]
                 [ymin : Integer]
                 [ymax : Integer]) #:transparent)

(struct board ([pixels : BitVector] [extents : extents]) #:transparent)
(struct coords ([x : Integer] [y : Integer]) #:transparent)

(define origin (coords 500 0))

(: string->coords (→ String coords))
(define (string->coords str)
  (let ([ls (map (λ ([s : String])
                  (let ([r (string->number s)])
                    (if (exact-integer? r) r 0))) (string-split str ","))])
    (coords (first ls) (second ls))))

(: sign (→ Integer Integer))
(define (sign x)
  "Returns 1 or -1."
  (if (eq? x 0) 1
      (floor (/ x (abs x)))))

(: rrange (→ Integer Integer (Listof Integer)))
(define (rrange a b)
  (let ([o (sign (- b a))])
    (range (+ a o) b o)))

(: draw-line (→ coords (Listof coords) (Listof coords)))
(define (draw-line c l)
  (cons c (append
           (if (eq? (coords-y c) (coords-y (first l)))
               (map (λ ([new-x : Integer]) (coords new-x (coords-y c))) (rrange (coords-x c) (coords-x (first l))))
               (map (λ ([new-y : Integer]) (coords (coords-x c) new-y)) (rrange (coords-y c) (coords-y (first l)))))
           l)))

(: parse-lines (→ String (Listof coords)))
(define (parse-lines str)
  (append*
   (map (λ ([line-str : String])
          (let ([line (map string->coords (string-split line-str " -> "))])
            (foldl
             draw-line
             (list (first line))
             (rest line))))
        (string-split str "\n"))))

(: compute-extents (→ (Listof coords) extents))
(define (compute-extents l)
  (let ([xs (map coords-x l)]
        [ys (map coords-y l)])
    (extents (apply min xs) (apply max xs) (apply min ys) (apply max ys))))

; If the coords are out of the extents, that's tough cookies, aka undefined behaviour
; Perhaps this could be made into a contract, but that would slow performance!
(: translate-coords (→ coords extents Integer))
(define (translate-coords c e)
  (+ (- (coords-x c) (extents-xmin e))
     (* (coords-y c) (+ 1 (- (extents-xmax e) (extents-xmin e))))))

(: board-set! (→ board coords Void))
(define (board-set! b c)
  (bit-vector-set! (board-pixels b) (translate-coords c (board-extents b)) #t))

(: parse-walls (→ String board))
(define (parse-walls str)
  (let* ([wall-coordinates (parse-lines str)]
         [ex (compute-extents wall-coordinates)]
         [bvl (* (+ 1 (extents-ymax ex))
                 (+ 1 (- (extents-xmax ex) (extents-xmin ex))))]
         [brd (board (make-bit-vector bvl) ex)])
    (map (curry board-set! brd) wall-coordinates)
    brd))

(: wall-at? (-> board coords Boolean))
(define (wall-at? b c)
  (bit-vector-ref (board-pixels b) (translate-coords c (board-extents b))))

(: apl (∀ (E R) (→ R (→ E E * R) (Listof E) R)))
(define (apl def f ls)
  (cond
    [(null? ls) def]
    [else (apply f ls)]))

(: paint-board  (→ board board pict))
(define (paint-board b₀ b₁)
  (let* ([pixel-factor 3]
         [square (filled-rectangle pixel-factor pixel-factor)]
         [wall (colorize square "brown")]
         [space (colorize square "white")]
         [sand (colorize square "orange")]
         [origin-square (colorize square "green")]
         [padding 1])
    (apl
     (blank)
     (curry vc-append padding)
     (for/list : (Listof pict)
               ([y (range 0 (+ 1 (extents-ymax (board-extents b₀ ))))])
       (apl
        (blank)
        (curry hc-append padding)
        (for/list : (Listof pict)
                  ([x (range (extents-xmin (board-extents b₀))
                             (+ 1 (extents-xmax (board-extents b₀))))])
          (cond
            [(wall-at? b₀ (coords x y)) wall]
            [(wall-at? b₁ (coords x y)) sand]
            [(equal? origin (coords x y)) origin-square]
            [else space])))))))

(: board-copy (→ board board))
(define (board-copy b0)
  (board (bit-vector-copy (board-pixels b0)) (board-extents b0)))

(: gravity (→ board coords (U coords 'at-rest 'oob)))
(define (gravity b c)
  (let ([y₁ (+ 1 (coords-y c))]
        [x_l (- (coords-x c) 1)]
        [x_r (+ (coords-x c) 1)])
    (cond
      [(y₁ . > . (extents-ymax (board-extents b))) 'oob]
      [(not (wall-at? b (coords (coords-x c) y₁))) (coords (coords-x c) y₁)]
      [(x_l . < . (extents-xmin (board-extents b))) 'oob]
      [(not (wall-at? b (coords x_l y₁))) (coords x_l y₁)]
      [(x_r . > . (extents-xmax (board-extents b))) 'oob]
      [(not (wall-at? b (coords x_r y₁))) (coords x_r y₁)]
      [else 'at-rest])))

(: sand-fall (→ board (U coords 'oob)))
(define (sand-fall b)
  (: iter (→ coords (U coords 'oob)))
  (define (iter c)
    (let ([step (gravity b c)])
      (cond
        [(equal? step 'oob) step]
        [(equal? step 'at-rest) c]
        [(coords? step) (iter step)])))
  (iter origin))

(: run-task (→ String pict))
(define (run-task str)
  (let* ([b₀ (parse-walls str)]
         [b₁ (board-copy b₀)])
    (: iter (→ Integer Void))
    (define (iter n)
      (let ([s (sand-fall b₁)])
        (unless (equal? s 'oob)
          (board-set! b₁ s)
          (println (format "iteration ~s" n))
          (iter (+ n 1)))))
    (iter 1)
    (paint-board b₀ b₁)))

(: parse-walls-2 (→ String board))
(define (parse-walls-2 str)
  (let* ([wall-coordinates (parse-lines str)]
         [ymax (+ 2 (apply max (map coords-y wall-coordinates)))]
         [wall-coordinates-2 (append wall-coordinates (draw-line (coords (- (coords-x origin) ymax) ymax)
                                                                 (list (coords (+ (coords-x origin) ymax) ymax))))]
         [ex (compute-extents wall-coordinates-2)]
         [bvl (* (+ 1 (extents-ymax ex))
                 (+ 1 (- (extents-xmax ex) (extents-xmin ex))))]
         [brd (board (make-bit-vector bvl) ex)])
    (map (curry board-set! brd) wall-coordinates-2)
    brd))

(: run-task-2 (→ String Integer))
(define (run-task-2 str)
  (let* ([b₀ (parse-walls-2 str)]
         [b₁ (board-copy b₀)])
    (: iter (→ Integer Integer))
    (define (iter n)
      (let ([s (sand-fall b₁)])
        (if (or (equal? s 'oob) (equal? s origin))
            n
            (begin (board-set! b₁ s)
                   (iter (+ n 1))))))
    (iter 1)))

(module+ test
  (require typed/rackunit)
  (let ([e (board-extents (parse-walls sample))])
    (check-equal? (translate-coords (coords 494 0) e) 0 "Start of first row")
    (check-equal? (translate-coords (coords 500 0) e) 6 "Sand origin")
    (check-equal? (translate-coords (coords 503 0) e) 9 "End of first row")
    (check-equal? (translate-coords (coords 494 1) e) 10 "Second row")
    (check-equal? (translate-coords (coords 494 9) e) 90 "Last row, first position")
    (check-equal? (translate-coords (coords 503 9) e) 99 "Last row, last position"))
  (let ([s (parse-walls sample)])
    (check-equal? (gravity s (coords 503 9)) 'oob "Falls off the bottom of the board")
    (check-equal? (gravity s (coords 494 8)) 'oob "Falls off to the left")
    (check-equal? (gravity s (coords 503 3)) 'oob "Falls off to the right")
    (check-equal? (gravity s (coords 495 8)) 'at-rest "Rests on the bottom")
    (check-equal? (gravity s (coords 500 0)) (coords 500 1) "Drops straight down")
    (check-equal? (gravity s (coords 496 5)) (coords 495 6) "Drops diagonally left")
    (check-equal? (gravity (let ([c (board-copy s)])
                             (board-set! c (coords 499 4))
                             c)
                           (coords 499 3))
                  (coords 500 4) "Drops diagonally right"))
  (let ([s (parse-walls sample)])
    (check-equal? (wall-at? s (coords 498 3)) #f)
    (check-equal? (wall-at? s (coords 498 4)) #t)
    (check-equal? (wall-at? s (coords 498 5)) #t)
    (check-equal? (wall-at? s (coords 498 6)) #t)
    (check-equal? (wall-at? s (coords 498 7)) #f)
    (check-equal? (wall-at? s (coords 499 6)) #f)
    (check-equal? (wall-at? s (coords 497 6)) #t)
    (check-equal? (wall-at? s (coords 496 6)) #t)
    (check-equal? (wall-at? s (coords 495 6)) #f)))
