#lang racket/base
(require (prefix-in r/b: racket/base)
         racket/draw
         (only-in racket/gui/base normal-control-font canvas%)
         racket/class
         mred/private/panel-wob)
(module+ test (require rackunit))

;(define (white-on-black-panel-scheme?) #f)

(define tab-panel-control%
  (class canvas%
    (inherit refresh get-dc get-client-size min-height min-width
             stretchable-height)
    (init choices [callback void] [style '()] [font normal-control-font])
    (super-new [style (r/b:append
                       (if (member 'deleted style)
                           '(style)
                           '())
                       '(no-focus transparent))])

    ;; ----------
    ;; internal state variables
    
    ;; (or/c #f (integer-in 0 (hash-count items)))
    ;; the currently selected tab
    (define selection #f)
    
    ;; hash[natural -o> string]
    ;; indicates the strings on each of the tab items
    (define items (make-hash (for/list ([i (in-naturals)]
                                        [s (in-list choices)])
                               (cons i s))))

    (define/private (number-of-items) (hash-count items))
    (define/private (get-item i) (hash-ref items i))
    (define/private (set-item i v)
      (unless (equal? (hash-ref items i #f) v)
        (hash-set! items i v)
        (update-min-width)
        (refresh)))
    (define/private (set-items is)
      (define new-items
        (for/hash ([i (in-naturals)]
                   [c (in-list is)])
          (values i c)))
      (unless (equal? new-items items)
        (set! items new-items)
        (update-min-width)
        (refresh)))
    (define/private (delete-item n)
      (delete-item/hash items n)
      (update-min-width)
      (refresh))

    ;; #t if we are between mouse enter and leave events, #f otherwise
    (define mouse-entered? #f)
    (define/private (set-mouse-entered? nv)
      (unless (equal? mouse-entered? nv)
        (set! mouse-entered? nv)
        (refresh)))
    
    ;; (or/c #f (integer-in 0 (number-of-items)))
    ;; indicates which of the tabs the mouse is currently over
    (define mouse-over #f)
    
    ;; boolean
    ;; when `mouse-over` isn't #f, if this is #t then
    ;; the mouse isn't just over the tab, it is also inside
    ;; the close `x` for that one
    (define mouse-over-close? #f)

    (define/private (set-mouse-over new-mouse-over new-mouse-over-close?)
      (unless (and (equal? mouse-over new-mouse-over)
                   (equal? mouse-over-close? new-mouse-over-close?))
        (set! mouse-over new-mouse-over)
        (set! mouse-over-close? new-mouse-over-close?)
        (refresh)))

    ;; (or/c #f (integer-in 0 (number-of-items)))
    ;; indicates which item was clicked in
    ;; (either becuase it is being dragged or for the close button)
    (define clicked-in #f)

    ;; (or/c #f natural?)
    ;; indicates the offset from the start where the
    ;; clicked-in tab was first clicked in or that
    ;; the close button was clicked on.
    ;; this is meaningful only if clicked-in is not #f
    (define clicked-in-offset #f)

    (define/private (set-clicked-in new-clicked-in new-clicked-in-offset)
      (unless (and (equal? clicked-in new-clicked-in)
                   (equal? clicked-in-offset new-clicked-in-offset))
        (set! clicked-in new-clicked-in)
        (set! clicked-in-offset new-clicked-in-offset)
        (refresh)))
    
    ;; the current coordinates of the mouse (but only
    ;; if the button is down and only when are between
    ;; enter and leave events)
    (define mouse-down-x #f)
    (define mouse-down-y #f)
    (define/private (set-mouse-xy x y)
      (unless (and (equal? mouse-down-x x)
                   (equal? mouse-down-y y))
        (set! mouse-down-x x)
        (set! mouse-down-y y)
        (refresh)))

    ;; ----------
    ;; public api
    
    (define/public (append choice)
      (set-item (number-of-items) choice)
      (refresh))
    (define/public (delete n)
      (unless (< n (number-of-items))
        (raise-argument-error 'delete `(integer-in 0 ,(number-of-items)) n))
      (delete-item n)
      (refresh))
      
    (define/public (get-item-label n) (void))
    (define/public (get-number) (number-of-items))
    (define/public (get-selection) selection)
    (define/public (set new-choices)
      (set-items new-choices)
      (refresh))
    (define/public (set-item-label n label) (void))
    (define/public (set-selection n) (void))
        
    ;; ----------
    ;; drawing
    
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-text-foreground
            (if (white-on-black-panel-scheme?)
                "white"
                "black"))

      ;; 1. draw the items that aren't being dragged
      (for ([i (in-range (number-of-items))])
        (unless (equal? i clicked-in)
          (define mouse-over?
            (and (not clicked-in)
                 (equal? i mouse-over)))
          (define ith-offset (find-ith-offset i))
          (draw-ith-item i
                         (natural-left-position (+ i ith-offset))
                         mouse-over?
                         (and mouse-over? mouse-over-close?)
                         #f)))

      ;; 2.
      (draw-lines-between-items)

      ;; 3. draw the one that is being dragged (so it shows up on top)
      (when clicked-in
        (cond
          [clicked-in-offset
           (draw-ith-item clicked-in
                          (get-left-edge-of-moving-tab)
                          #t
                          #f
                          #f)]
          [else
           ;; here we need to do button-like things
           (void)])))

    (define/private (draw-lines-between-items)
      (define dc (get-dc))
      (send dc set-pen (get-text-and-close-foreground-color #f #f) 1 'solid)
      (define-values (cw ch) (get-client-size))
      (for ([i (in-range 1 (number-of-items))])
        (define x (natural-left-position i))
        (send dc draw-line x vertical-item-margin x (- ch vertical-item-margin))))

    (define/private (draw-ith-item i x-start mouse-over? mouse-over-close? mouse-down?)
      (define dc (get-dc))
      (define lab (get-item i))
      (define lab-space (- (width-of-tab)
                           horizontal-item-margin
                           horizontal-item-margin
                           size-of-close-icon-circle))
      (define-values (cw ch) (get-client-size))

      (send dc set-brush (get-tab-background-color mouse-over? mouse-down?) 'solid)
      (send dc set-pen "black" 1 'transparent)
      (send dc draw-rectangle x-start 0 (width-of-tab) ch)
      
      (send dc set-clipping-rect
            (+ x-start horizontal-item-margin)
            0
            lab-space
            ch)
      (send dc set-text-foreground (get-text-and-close-foreground-color mouse-over? mouse-down?))
      (send dc draw-text lab (+ x-start horizontal-item-margin) vertical-item-margin)
      (send dc set-clipping-region #f)
      (maybe-draw-fade-at-edge lab lab-space x-start mouse-over? mouse-down?)
      (draw-close-icon x-start mouse-over? mouse-over-close? mouse-down?))
    
    (define/private (maybe-draw-fade-at-edge lab lab-space x-start mouse-over? mouse-down?)
      (define dc (get-dc))
      (define-values (cw ch) (get-client-size))
      (define-values (tw th td ta) (send dc get-text-extent lab))
      (when (tw . >= . lab-space)
        (when (lab-space . > . end-of-label-horizontal-gradient-amount)
          (define right-edge-of-label
            (+ x-start horizontal-item-margin lab-space))
          (define old-brush (send dc get-brush))
          (define old-pen (send dc get-pen))
          (define gradient-stops
            (list (list 0 (make-transparent (get-tab-background-color mouse-over? mouse-down?)))
                  (list 1 (get-tab-background-color mouse-over? mouse-down?))))
          (send dc set-brush
                (new brush%
                     [gradient
                      (new linear-gradient%
                           [x0 (- right-edge-of-label end-of-label-horizontal-gradient-amount)]
                           [y0 0]
                           [x1 right-edge-of-label]
                           [y1 0]
                           [stops gradient-stops])]))
          (send dc set-pen "black" 1 'transparent)
          (send dc draw-rectangle
                (- right-edge-of-label end-of-label-horizontal-gradient-amount)
                0
                end-of-label-horizontal-gradient-amount
                ch)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush))))

    (define/private (draw-close-icon x-start mouse-over? mouse-over-close? mouse-down?)
      (define dc (get-dc))
      (define close-icon-start (+ x-start (get-start-of-cross-x-offset)))
      (define-values (cw ch) (get-client-size))
      (define cx (+ close-icon-start (/ size-of-close-icon-circle 2)))
      (define cy (/ ch 2))
      (when mouse-over-close?
        (send dc set-brush (get-mouse-over-close-circle-color) 'solid)
        (send dc set-pen "black" 1 'transparent)
        (send dc draw-ellipse
              (- cx (/ size-of-close-icon-circle 2))
              (- cy (/ size-of-close-icon-circle 2))
              size-of-close-icon-circle
              size-of-close-icon-circle))
      (send dc set-pen (get-text-and-close-foreground-color mouse-over? mouse-down?) 1 'solid)
      (send dc draw-line
            (- cx (/ size-of-close-icon-x 2))
            (- cy (/ size-of-close-icon-x 2))
            (+ cx (/ size-of-close-icon-x 2))
            (+ cy (/ size-of-close-icon-x 2)))
      (send dc draw-line
            (- cx (/ size-of-close-icon-x 2))
            (+ cy (/ size-of-close-icon-x 2))
            (+ cx (/ size-of-close-icon-x 2))
            (- cy (/ size-of-close-icon-x 2)))
      (void))

    ;; -------
    ;; mouse movement

    (define/override (on-event evt)
      (cond
        [(send evt leaving?)
         (set-mouse-over #f #f)
         (set-mouse-xy #f #f)
         (set-mouse-entered? #f)
         (set-clicked-in #f #f)]
        [(send evt entering?)
         (set-mouse-entered? #t)])

      (when mouse-entered?
        (define-values (mouse-over mx-offset-in-tab mouse-over-close?)
          (mouse->tab (send evt get-x) (send evt get-y)))
        (cond
          [(send evt button-down?)
           (set-mouse-xy (send evt get-x) (send evt get-y))
           (set-clicked-in mouse-over (and (not mouse-over-close?) mx-offset-in-tab))]
          [(send evt dragging?)
           (set-mouse-xy (send evt get-x) (send evt get-y))]
          [(send evt button-up?)
           (set-mouse-xy #f #f)
           (set-clicked-in #f #f)]
          [else
           (set-mouse-over mouse-over mouse-over-close?)])))
    
    ;; -------
    ;; colors
    
    (define/private (get-text-and-close-foreground-color mouse-over? mouse-down?)
      (cond
        [(white-on-black-panel-scheme?)
         (cond
           [mouse-down?
            wob-text-and-close-icon-down-color]
           [else
            wob-text-and-close-icon-color])]
        [else
         (cond
           [mouse-down?
            tab-text-and-close-icon-down-color]
           [else
            tab-text-and-close-icon-color])]))

    (define/private (get-tab-background-color mouse-over? mouse-down?)
      (cond
        [(white-on-black-panel-scheme?)
         (cond
           [(and mouse-over? mouse-down?) selected-wob-color]
           [mouse-over? mouse-over-wob-color]
           [else natural-wob-color])]
        [else
         (cond
           [(and mouse-over? mouse-down?) selected-tab-color]
           [mouse-over? mouse-over-tab-color]
           [else natural-tab-color])]))

    (define/private (get-mouse-over-close-circle-color)
      (cond
        [(white-on-black-panel-scheme?)
         wob-mouse-over-close-circle-color]
        [else
         tab-mouse-over-close-circle-color]))

    ;; -----
    ;; sizes and positions
    
    (define/private (natural-left-position i)
      (* i (width-of-tab)))

    ;; determines the delta (0, -1, +1) for the `ith` tab
    ;; due to some other tab being dragged around
    ;; pre: i ≠ clicked-in
    (define/private (find-ith-offset i)
      (cond
        [(and clicked-in clicked-in-offset)
         (define i-left (natural-left-position i))
         (define i-right (+ i-left (width-of-tab)))
         (define i-middle (/ (+ i-left i-right) 2))
         (define left-edge-of-moving-tab (get-left-edge-of-moving-tab))
         (define right-edge-of-moving-tab (+ left-edge-of-moving-tab (width-of-tab)))
         (cond
           [(< i clicked-in)
            (if (left-edge-of-moving-tab . < . i-middle)
                +1
                0)]
           [(< clicked-in i)
            (if (right-edge-of-moving-tab . > . i-middle)
                -1
                0)]
           [else 0])]
        [else 0]))

    (define/private (get-left-edge-of-moving-tab)
      (ensure-in-bounds (natural-left-position 0)
                        (- mouse-down-x clicked-in-offset)
                        (natural-left-position (- (number-of-items) 1))))
    
    (define/private (width-of-tab)
      (define-values (cw ch) (get-client-size))
      (define dc (get-dc))
      (define shrinking-required-size (/ cw (number-of-items)))

      ;; this is the maximum size that a tab will ever be
      (define unconstrained-tab-size (* (send (send dc get-font) get-point-size) 12))
      (cond
        [(< shrinking-required-size unconstrained-tab-size)
         shrinking-required-size]
        [else
         unconstrained-tab-size]))

    (define/private (update-min-width)
      (define items (number-of-items))
      (min-width
       (* items (+ horizontal-item-margin
                   horizontal-item-margin
                   size-of-close-icon-circle))))

    ;; returns the position where the close x starts, relative
    ;; to the position of the start of the tab itself
    (define/private (get-start-of-cross-x-offset)
      (- (width-of-tab)
         horizontal-item-margin
         size-of-close-icon-circle))

    (define/private (mouse->tab mx my)
      (define-values (cw ch) (get-client-size))
      (define tab-candidate-i (floor (/ mx (width-of-tab))))
      (cond
        [(<= 0 tab-candidate-i (- (number-of-items) 1))
         (define mx-offset-in-tab (- mx (natural-left-position tab-candidate-i)))
         (define start-of-cross (get-start-of-cross-x-offset))
         (define-values (cw ch) (get-client-size))
         (define in-close-x
           (<= start-of-cross mx-offset-in-tab (+ start-of-cross size-of-close-icon-circle)))
         (define in-close-y
           (<= (- (/ ch 2) size-of-close-icon-circle)
               my
               (+ (/ ch 2) size-of-close-icon-circle)))
         (values tab-candidate-i mx-offset-in-tab (and in-close-x in-close-y))]
        [else
         (values #f #f #f)]))
    
    (let ()
      (define dc (get-dc))
      (send dc set-smoothing 'smoothed)
      (send dc set-font font)
      (define-values (tw th td ta) (send dc get-text-extent "Xy"))
      (min-height (max (+ vertical-item-margin
                          (ceiling (inexact->exact th))
                          vertical-item-margin)
                       size-of-close-icon-circle))
      (update-min-width)
      (stretchable-height #f))
    ))

;; -----
;; size constants

;; space around text in each item horizontally
(define horizontal-item-margin 10)
(define vertical-item-margin 4)

(define end-of-label-horizontal-gradient-amount 16)

(define size-of-close-icon-x 6)
(define size-of-close-icon-circle 12)

;; ------
;; color constants
(define shade-delta 16)
(define shade-start 20)
(define (make-color shade-count dark?)
  (define offset (+ shade-start (* shade-delta shade-count)))
  (define c (if dark? offset (- 255 offset)))
  (make-object color% c c c))

(define natural-tab-color (make-color 1 #f))
(define natural-wob-color (make-color 1 #t))
(define mouse-over-tab-color (make-color 2 #f))
(define mouse-over-wob-color (make-color 2 #t))
(define selected-tab-color (make-color 3 #f))
(define selected-wob-color (make-color 3 #t))

(define wob-text-and-close-icon-down-color (make-color 1 #f))
(define wob-text-and-close-icon-color (make-color 2 #f))
(define tab-text-and-close-icon-down-color (make-color 1 #t))
(define tab-text-and-close-icon-color (make-color 2 #t))

(define tab-mouse-over-close-circle-color (make-color 6 #f))
(define wob-mouse-over-close-circle-color (make-color 6 #t))

(define (make-transparent color)
  (make-object color%
    (send color red)
    (send color green)
    (send color blue)
    0))

(define (ensure-in-bounds low x high)
  (max (min x high) low))
(module+ test
  (check-equal? (ensure-in-bounds 0 1 10) 1)
  (check-equal? (ensure-in-bounds 0 8 10) 8)
  (check-equal? (ensure-in-bounds 0 -1 10) 0)
  (check-equal? (ensure-in-bounds 0 11 10) 10))

(define (delete-item/hash items n)
  (for ([i (in-range (+ n 1) (hash-count items))])
    (hash-set! items (- i 1) (hash-ref items i)))
  (hash-remove! items (- (hash-count items) 1)))

(module+ test
  (let ()
    (define ht (make-hash (list (cons 0 "a"))))
    (delete-item/hash ht 0)
    (check-equal? ht (make-hash)))

  (let ()
    (define ht (make-hash (list (cons 0 "a")
                                (cons 1 "b")
                                (cons 2 "c")
                                (cons 3 "d"))))
    (delete-item/hash ht 0)
    (check-equal? ht (make-hash (list (cons 0 "b")
                                      (cons 1 "c")
                                      (cons 2 "d")))))

  (let ()
    (define ht (make-hash (list (cons 0 "a")
                                (cons 1 "b")
                                (cons 2 "c")
                                (cons 3 "d"))))
    (delete-item/hash ht 2)
    (check-equal? ht (make-hash (list (cons 0 "a")
                                      (cons 1 "b")
                                      (cons 2 "d")))))

  (let ()
    (define ht (make-hash (list (cons 0 "a")
                                (cons 1 "b")
                                (cons 2 "c")
                                (cons 3 "d"))))
    (delete-item/hash ht 3)
    (check-equal? ht (make-hash (list (cons 0 "a")
                                      (cons 1 "b")
                                      (cons 2 "c"))))))

(module+ main
  (require (only-in racket/gui/base frame% canvas%))
  (define f (new frame% [label ""] [width 300] [height 80]))
  (define tpc (new tab-panel-control% [parent f]
                   [choices '("a" "b" "abcdefghijklmnopqrstuvwxyz" "d" "e")]))
  (send f show #t))
