#lang racket
(require racket/unsafe/ops)
(require rackunit)	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Original Myers stack implementation;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide myers-stack myers-stack-car 
         myers-stack-cdr myers-stack-cons
         myers-stack-null? myers-stack-pair?
         myers-stack->list list->myers-stack
         myers-stack-drop)


;;;;;;;;;;;;;;;;;;;;;;;;  
;;Myers Stack Datatype;;
;;;;;;;;;;;;;;;;;;;;;;;;

(struct myers-stack (data 
                     length
                     next
                     jump)
  #:mutable)



;;;;;;;;;;;;;;;;;;;;;;
;;Exported Functions;;
;;;;;;;;;;;;;;;;;;;;;;

(define (myers-stack-car stack)
   (myers-stack-data stack))

(define myers-stack-car-set! set-myers-stack-data!)

(define (myers-stack-cdr stack)
  (myers-stack-next stack))

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
         (let ([length (unsafe-fx+ 1 c-length)])
            (if (eq? (unsafe-fx- c-length jump-length)
                     (unsafe-fx- jump-length jumps-jump-length))
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

(define (myers-stack-drop stack count)
  (cond
    [(eq? count 1) (myers-stack-cdr stack)]
    [(> count 1)
      (let* ([jump-length (unsafe-fx+ (myers-stack-length stack) (myers-stack-length (myers-stack-jump stack)))])
             (cond
               [(<= jump-length count) (myers-stack-drop (myers-stack-jump stack)(unsafe-fx- count jump-length))]
               [else (myers-stack-drop (myers-stack-next stack) (unsafe-fx- count 1))]))]
    [else stack]))

;;;;;;;;;
;;Tests;;
;;;;;;;;;

(define x (list->myers-stack (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
(check-eq? 0 (myers-stack-length myers-stack-null))
(check-eq? 5 (myers-stack-data (myers-stack-drop x 4)))
(print (myers-stack->list (list->myers-stack (list 1 2 3 4 5 6 7))))
(newline)
(check-eq?  16 (myers-stack-length x))
(check-eq? 14 (myers-stack-length (myers-stack-cdr (myers-stack-cdr x))))
(check-true (myers-stack-pair? x) "uh oh")
(check-false (myers-stack-null? x) "uh oh")
(check-true (myers-stack-null? myers-stack-null) "uh oh")
(check-eq? 1 (myers-stack-car (myers-stack-drop x 0)))
(define crap (myers-stack-cons 2 (myers-stack-cons 1 myers-stack-null)))
(check-eq? 2 (myers-stack-car crap))

