#lang racket
(require racket/unsafe/ops)


;;Original Myers stack implementation

;;TODO
;myers-stack-index
;myers-stack-drop
;Test
;Visualization

#;(provide myers-stack myers-stack-car 
         myers-stack-cdr myers-stack-cons
         myers-stack-null? myers-stack-pair?
         myers-stack->list list->myers-stack
         myers-stack-drop
         gen:ms)


;;;;;;;;;;;;;;;;;;;;;;;;  
;;Myers Stack Datatype;;
;;;;;;;;;;;;;;;;;;;;;;;;

(struct myers-stack (data 
                     length
                     next
                     jump)
  #:mutable)

(define (myers-stack-car stack)
 ; (assert (< 0 (myers-stack-length stack)))
  (myers-stack-data stack))

(define myers-stack-car-set! set-myers-stack-data!)

(define (myers-stack-cdr stack)
 ;; (assert (< 0 (myers-stack-length stack)))
  (myers-stack-next stack))

;;;;;;;;;;;;;;;;;;;;;;
;;Exported Functions;;
;;;;;;;;;;;;;;;;;;;;;;

(define myers-stack-null
         (let* ([jumps-jump (myers-stack #f -1 #f #f)]
                [jump (myers-stack #f 0 #f jumps-jump)])
           (myers-stack #f 0 #f jump)))
   
(define (myers-stack-cons car cdr)
   (let* ([jump (myers-stack-jump cdr)]
            [jumps-jump (myers-stack-jump jump)])
      (let ([c-length (myers-stack-length cdr)]
             [jump-length (myers-stack-length jump)]
             [jumps-jump-length (myers-stack-length jumps-jump)])
         (let ([length (+ 1 c-length)])
            (if (eq? (- c-length jump-length)
                     (- jump-length jumps-jump-length))
                (myers-stack car length cdr jumps-jump)
                (myers-stack car length cdr cdr))))))


(define (myers-stack-null? stack)
  (and (myers-stack? stack) (zero? (myers-stack-length stack))))

(define (myers-stack-pair? stack)
  (and (myers-stack? stack) (not (zero? (myers-stack-length stack)))))

(define (myers-stack->list stack)
  (cond
    [(myers-stack-null? stack) '()]
    [(myers-stack-pair? stack)
     (cons (myers-stack-car stack)
           (myers-stack->list (myers-stack-cdr stack)))]))

(define (list->myers-stack list)
  (foldr myers-stack-cons myers-stack-null list))

#;(define (make-myers-stack ...)
  (list->myers-stack (list ...)))


(define (drop stack count)
  (cond
    [(eq? count 1) (myers-stack-cdr stack)]
    [(> count 1)
      (let* ([jump-length (+ (myers-stack-length stack) (myers-stack-length (myers-stack-jump stack)))])
             (cond
               [(<= jump-length count) (drop (myers-stack-jump stack)(- count jump-length))]
               [else (drop (myers-stack-next stack) (- count 1))]))]
    [else stack]))

;;;;;;;;;
;;Tests;;
;;;;;;;;;

(define x (list->myers-stack (list 1 2 3 4 5 6 7)))
(pretty-print x)
(newline)
(print "drop: ")
(print (myers-stack-data (drop x 4)))
(newline)
;(print (list->myers-stack (list 1 2 3 4 5 6 7)))
(print (myers-stack-length (myers-stack-jump x)))
(newline)
(print (myers-stack-length x))
(newline)
(print (myers-stack-car x))
(newline)
(print (myers-stack->list x))
(newline)
(print (myers-stack-length (myers-stack-jump x)))
;(define x (myers-stack-cons 2 (myers-stack-cons 1 myers-stack-null)))
;(print (myers-stack-data (myers-stack-next x)))

;(print (myers-stack-data x))
;(define x myers-stack-null)
;(define y (myers-stack-length x))
;(define z (myers-stack-next x))
;(print (list x y z))
;(print (myers-stack-data (myers-stack-next x)))
;(print (myers-stack-length x))

