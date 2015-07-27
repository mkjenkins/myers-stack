#lang racket
(require racket/unsafe/ops)


;;Original Myers stack implementation

;;TODO
;myers-stack-index
;myers-stack-drop
;Test
;Visualization

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

   
(define (myers-stack-cons car cdr )
   (let* ([ jump (myers-stack-jump cdr )]
            [jumps-jump (myers-stack-jump jump)])
      (let ([cdr-length (myers-stack-length cdr)]
             [jump-length (myers-stack-length jump)]
             [jumps-jump-length (myers-stack-length jumps-jump)])
         (let ([length (unsafe-fx+ 1 cdr-length )])
            (if (eq? (unsafe-fx- cdr-length jump-length )
                     (unsafe-fx- jump-length jumps-jump-length ))
                (myers-stack car cdr length jumps-jump)
                (myers-stack car cdr length cdr ))))))

(define myers-stack-null
         (let* ([jumps-jump (myers-stack #f -1 #f #f)]
                [jump (myers-stack #f 0 #f jumps-jump)])
           (myers-stack #f 0 #f jump)))

(define (myers-stack-null? stack)
  (and (myers-stack? stack) (zero? (myers-stack-length stack))))

(define (myers-stack-pair? stack)
  (and (myers-stack? stack) (not (zero? myers-stack-length stack))))

(define (myers-stack->list stack)
  (cond
    [(myers-stack-null? stack) '()]
    [(myers-stack-pair? stack)
     (cons (myers-stack-car stack)
           (myers-stack->list (myers-stack-cdr stack)))]))

(define (list->myers-stack list)
  (foldr myers-stack-cons myers-stack-null list))


;;;;;;;;;
;;Tests;;
;;;;;;;;;

(define x (list->myers-stack (list 1 2 3 4 5 6 7)))
(print (myers-stack-data x))
(print (myers-stack-data (myers-stack-next x)))
(print (myers-stack-length x))

