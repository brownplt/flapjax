#lang scheme
(require
 (only-in xml xexpr? xml->xexpr document-element read-xml xexpr->string))

(provide xml->xexpr xexpr? xexpr->string)
; Not the usual contract for a predicate.  Handle with care.
(provide element?)
(define element? (λ (v) (and (cons? v))))

(provide/contract (element-children (element? . -> . (listof any/c))))
(define (element-children x)
  (if (and (not (empty? (rest x))) ; we have a tail
           (or (empty? (second x)) ; (tag () ...)
               (cons? (first (second x))))) ; (tag ([...] ...) ...)
      (rest (rest x))
      (rest x)))

(provide/contract (element-attributes (element? . -> . (listof any/c))))
(define (element-attributes x)
  (if (and (not (empty? (rest x))) ; we have a tail
           (or (empty? (second x)) ; (tag () ...)
               (cons? (first (second x))))) ; (tag ([...] ...) ...)
      (second x)
      empty))

(provide element-name)
(define (element-name x)
  (first x))
  

; gmap/acc : a (xexpr a -> xexpr a) -> xexpr
; Applies the transformer outside in, passing the accumulator down to its children.  Each path gets a new
; accumulator.  The accumulator returned at leaves is discarded
(provide gmap/acc)
(define (gmap/acc a f x)
  (let-values ([(x a) (f x a)])
    (cond
      [(element? x) `(,(element-name x) ,(element-attributes x)
                                        ,@(map (λ (x) (gmap/acc a f x)) (element-children x)))]
      [else x])))

; gmap/acc/inverted [a] (xexpr [a] -> xexpr a) -> xexpr a
(define (gmap/acc/inverted a f x)
  (cond
    [(element? x)
     (let* ([rs (map (λ (x) (let-values ([(x a) (f x a)]) (cons x a))) (element-children x))]
            [xs (map car rs)]
            [as (map cdr rs)])
       (f `(,(element-name x) ,(element-attributes x) ,@xs) as))]
    [else (f x a)]))

(provide gmap)
(define (gmap f x)
  (let ([x (f x)])
    (cond
      [(element? x) `(,(element-name x) ,(element-attributes x)
                                        ,@(map  (λ (x) (gmap f x)) (element-children x)))]
      [else x])))

(provide gmap-q)
(define (gmap-q combine f x)
  (letrec ([func (λ (x)
                   (let ([a (f x)])
                     (cond
                       [(element? x)
                        (foldl (λ (x a) (combine (func x) a)) a (element-children x))]
                       [else a])))])
    (func x)))
                         

(provide/contract (path->xexpr ((or/c string? path?) . -> . xexpr?)))
(define (path->xexpr path)
  (let* ([in (open-input-file path)]
         [x (xml->xexpr (document-element (read-xml in)))])
    (close-input-port in)
    x))

(provide write-xexpr-to-file)
(define (write-xexpr-to-file path x [dtd false])
  (with-output-to-file 
      path
    (λ ()
      (when dtd
        (display dtd))
      (display
       (xexpr->string x)))
    #:exists 'replace))


(provide/contract (make-element-transformer
                   (symbol? (element? . -> . xexpr?) . -> . (any/c . -> . any))))
(define (make-element-transformer elt f)
  (λ (x)
    (cond
      [(and (element? x) (symbol=? (element-name x) elt)) (f x)]
      [else x])))

(provide compose-transformers)
(define (compose-transformers . ts)
  (λ (x)
    (let loop ([ts ts])
      (if (empty? ts)
          x
          (let ([r ((first ts) x)])
            (if (eq? r x)
                (loop (rest ts))
                r))))))

(provide/contract (get-attribute (symbol? element? . -> . (or/c false? string?))))
(define (get-attribute id elt)
  (let ([r (assoc id (element-attributes elt))])
    (and r (second r))))

(provide transform-attribute)
(define (transform-attribute id attrs f)
  (if (empty? attrs)
      empty
      (if (symbol=? (first (first attrs)) id)
          `([,(first (first attrs)) ,(f (second (first attrs)))] ,@(rest attrs))
          (cons (first attrs) (transform-attribute id (rest attrs) f)))))

(provide define-element-transformer)
(define-syntax (define-element-transformer stx)
  (syntax-case stx ()
    [(_ (id x) body ...)
     #`(define id
         (make-element-transformer
          'id
          (lambda (x)
            body ...)))]))