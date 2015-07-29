#lang racket
(require racket/unsafe/ops)

;head and mid will be nodes
(struct myers-stack-ef (head
                        mid))
(struct node (data
              length
              next 
              jump))


(define (myers-stack-ef-car stack)
 ; (assert (< 0 (myers-stack-length stack)))
  (myers-stack-ef-head stack))

;(define myers-stack-car-set! (myers-stack-ef-data! set-node-data!))

(define (myers-stack-ef-cdr stack)
 ;; (assert (< 0 (myers-stack-length stack)))
  (node-next (myers-stack-ef-head stack)))


;;;;;;;;;;;;;;;;;;;;;;
;;Exported Functions;;
;;;;;;;;;;;;;;;;;;;;;;

;head->length - head->jump->length == mid->next->length - mid->next->jump->length
 (define (myers-stack-ef-cons car cdr)
   (let* ([head-length (node-length (myers-stack-ef-head cdr))]
            [head-jump-length  (node-length (myers-stack-ef-head (node-jump (myers-stack-ef-head cdr))))])
      (let ([cdr-length (node-length (myers-stack-ef-head cdr))]
             [mid-next-length (if (eq? #f (node-next (myers-stack-ef-mid cdr)))
                                  0
                                 (node-next (myers-stack-ef-mid cdr)))]
             [mid-next-jump-length (if (eq? #f (node-next (myers-stack-ef-mid cdr)))
                                       0
                                       (node-next (myers-stack-ef-mid cdr)))])
         (let ([length (+ 1 cdr-length)])
            (if (eq? (- head-length head-jump-length )
                     (- mid-next-length mid-next-jump-length))
                (myers-stack-ef (node car length cdr  (node-jump (myers-stack-ef-mid cdr))) (node-next (myers-stack-ef-head cdr)))
                (myers-stack-ef (node car length cdr (node-jump (myers-stack-ef-mid cdr))) (node-next (myers-stack-ef-head cdr))))))))

(define myers-stack-ef-null
         (let* ([jumps-jump (myers-stack-ef (node #f -1 #f #f) (node #f -1 #f #f))]
                [jump (myers-stack-ef (node #f 0 #f jumps-jump) (node #f 0 #f jumps-jump))])
            (myers-stack-ef (node #f 0 #f jump) (node #f 0 #f jump))))

(define (myers-stack-ef-null? stack)
  (and (myers-stack-ef? stack) (zero? (myers-stack-ef-head (node-length stack)))))

(define (myers-stack-ef-pair? stack)
  (and (myers-stack-ef? stack) (not (zero? myers-stack-ef-head (node-length stack)))))

(define (myers-stack-ef->list stack)
  (cond
    [(myers-stack-ef-null? stack) '()]
    [(myers-stack-ef-pair? stack)
     (cons (myers-stack-ef-car (node-data stack))
           (myers-stack-ef->list (myers-stack-ef-head (node-next stack))))]))

(define (list->myers-stack-ef list)
  (foldr myers-stack-ef-cons  myers-stack-ef-null list))

(define (myers list)
  (myers-stack-ef-cons list myers-stack-ef-null))

;;;;;;;;;
;;Tests;;
;;;;;;;;;
;(define x (list->myers-stack-ef (list 1 2 3)))
(define x (myers-stack-ef-cons 1 myers-stack-ef-null))
(display (node-next (myers-stack-ef-head x)))
(display (node-jump (myers-stack-ef-head x)))
(define y (myers-stack-ef-cons 2 x))
;(display (node-next (myers-stack-ef-head y)))
;(print (node-next (myers-stack-ef-mid myers-stack-ef-null)))