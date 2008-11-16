; Support for <link> tags.
#lang scheme
(require "xexpr.ss")


(define link-table (make-hash))

(provide make-link-table)
(define (make-link-table x)
    (gmap-q void
           (λ (x)
             (cond
               [(and (element? x) (symbol=? 'a (element-name x)) (get-attribute 'name x))
                (hash-set! link-table (get-attribute 'name x) true)]
               [(and (element? x) (get-attribute 'title x))
                (begin
                  (hash-set! link-table (get-attribute 'title x) true))]
               [else (void)]))
           x)
    link-table)

(define (make-link-transformer links)
  (make-element-transformer
   'link
   (λ (x)
     (cond
       [(empty? (element-children x))
        (begin
          (printf "<link>: empty link~n")
          x)]
       [(not (empty? (rest (element-children x))))
        (begin
          (printf "<link>: multiple children in ~a" (xexpr->string x))
          x)]
       [(not (string? (first (element-children x))))
        (begin
          (printf "<link>: expected text in ~a" (xexpr->string x))
          x)]
       [else
        (let ([link (first (element-children x))])
          (if (hash-ref links link false)
              `(a ([href ,(format "#~a" link)] [class "link"]) ,link)
              (error (format "<link>: target not found in ~a" (xexpr->string x)))))]))))

(provide/contract (links->html (xexpr? . -> . xexpr?)))
(define (links->html x)
  (gmap (make-link-transformer link-table) x))
  
  