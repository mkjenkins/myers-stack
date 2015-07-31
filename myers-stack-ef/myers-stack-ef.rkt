#lang racket
(require racket/unsafe/ops)
(require rackunit)

#;(provide myers-stack myers-stack-ef-car 
         myers-stack-cdr myers-stack-cons
         myers-stack-null? myers-stack-pair?
         myers-stack->list list->myers-stack
         myers-stack-drop)

(struct myers-stack-ef (head
                        mid)
  #:mutable)

(struct node (data
              length
              next 
              jump)
  #:mutable)


(define (myers-stack-ef-car stack)
  (node-data (myers-stack-ef-head stack)))

(define myers-stack-car-set! set-myers-stack-ef-head!)

(define (myers-stack-ef-cdr stack)
  (node-next (myers-stack-ef-head stack)))


;;;;;;;;;;;;;;;;;;;;;;
;;Exported Functions;;
;;;;;;;;;;;;;;;;;;;;;;

 (define (myers-stack-ef-cons car cdr)
   (let* ([head-length (node-length (myers-stack-ef-head cdr))]
            [head-jump-length  (node-length (myers-stack-ef-head (node-jump (myers-stack-ef-head cdr))))])
      (let ([c-length (node-length (myers-stack-ef-head cdr))]
             [mid-next-length (if (eq? #f (node-next (myers-stack-ef-mid cdr)))
                                  0
                                 (node-length (myers-stack-ef-mid (node-next (myers-stack-ef-mid cdr)))))]
             [mid-next-jump-length (if (eq? #f (node-next (myers-stack-ef-mid cdr)))
                                       0
                                       (node-length (myers-stack-ef-mid (node-jump (myers-stack-ef-mid (node-next (myers-stack-ef-mid cdr)))))))])
         (let ([length (unsafe-fx+ 1 c-length)])
            (if (eq? (unsafe-fx- head-length head-jump-length )
                     (unsafe-fx- mid-next-length mid-next-jump-length))
                (myers-stack-ef (node car length cdr (node-next (myers-stack-ef-mid cdr))) (node car length cdr (node-jump (myers-stack-ef-mid cdr))))
                (myers-stack-ef (node car length cdr (node-jump (myers-stack-ef-mid cdr))) (node car length cdr (node-jump (myers-stack-ef-mid cdr)))))))))

(define myers-stack-ef-null
         (let* ([jumps-jump (myers-stack-ef (node #f -1 #f #f) (node #f -1 #f #f))]
                [jump (myers-stack-ef (node #f 0 #f jumps-jump) (node #f 0 #f jumps-jump))])
            (myers-stack-ef (node #f 0 jump jump) (node #f 0 jump jump))))

(define (myers-stack-ef-null? stack)
  (and (myers-stack-ef? stack) (zero? (node-length (myers-stack-ef-head stack)))))

(define (myers-stack-ef-pair? stack)
  (and (myers-stack-ef? stack) (not (zero? (node-length (myers-stack-ef-head stack))))))

(define (myers-stack-ef->list stack)
  (cond
    [(myers-stack-ef-null? stack) '()]
    [(myers-stack-ef-pair? stack)
     (cons (myers-stack-ef-car stack)
           (myers-stack-ef->list (node-next (myers-stack-ef-head stack))))]))

(define (list->myers-stack-ef list)
  (foldr myers-stack-ef-cons  myers-stack-ef-null list))

(define (myers list)
  (myers-stack-ef-cons list myers-stack-ef-null))

(define (myers-stack-ef-drop stack count)
  (cond
    [(eq? count 1) (myers-stack-ef-cdr stack)]
    [(> count 1)
      (let* ([jump-length (unsafe-fx+ (node-length (myers-stack-ef-head stack)) (node-length (myers-stack-ef-head (node-jump (myers-stack-ef-head stack)))))])
             (cond
               [(<= jump-length count) (myers-stack-ef-drop (node-jump (myers-stack-ef-head stack))(unsafe-fx- count jump-length))]
               [else (myers-stack-ef-drop (node-next (myers-stack-ef-head stack)) (unsafe-fx- count 1))]))]
    [else stack]))


;;;;;;;;;
;;Tests;;
;;;;;;;;;
(define x (list->myers-stack-ef (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
(check-eq? 0 (node-length (myers-stack-ef-head myers-stack-ef-null)))
(check-eq? 5 (myers-stack-ef-car (myers-stack-ef-drop x 4)))
(print (myers-stack-ef->list (list->myers-stack-ef (list 1 2 3 4 5 6 7))))
(newline)
(check-eq?  16 (node-length (myers-stack-ef-head x)))
(check-eq? 2 (myers-stack-ef-car (myers-stack-ef-cdr x)))
(check-true (myers-stack-ef-pair? x) "uh oh")
(check-false (myers-stack-ef-null? x) "uh oh")
(check-true (myers-stack-ef-null? myers-stack-ef-null) "uh oh")
(check-eq? 1 (myers-stack-ef-car (myers-stack-ef-drop x 0)))
(define crap (myers-stack-ef-cons 2 (myers-stack-ef-cons 1 myers-stack-ef-null)))
(check-eq? 2 (myers-stack-ef-car crap))
