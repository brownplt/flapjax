#!/usr/bin/env mzscheme
#lang scheme
(require "xexpr.ss" 
         "links.ss"
         "vim.ss")

(define (article? x)
  (and (element? x) (eq? 'article (element-name x))))

(define (chapter? x)
  (and (element? x) (eq? 'chapter (element-name x))))

(define (document? x)
  (and (element? x) (eq? 'document (element-name x))))

(define raw-doc (path->xexpr "../doc.html"))

(define chapters
  (filter (λ (x) (and (element? x) (eq? 'chapter (element-name x)))) (element-children raw-doc)))

(define articles
  (filter (λ (x) (and (element? x) (eq? 'article (element-name x)))) (append-map element-children chapters)))

(define programlisting
  (make-element-transformer
   'programlisting
   (λ (x)
     `(pre () ,@(element-children x)))))

(define example
  (make-element-transformer
   'example
   (λ (x)
     `(div ([class "example"])
           (div ([class "title"]) "Example")
           ,@(element-children x)))))

(define code
  (make-element-transformer
   'code
   (λ (x)
     `(span ([class "inlineCode"]) ,@(element-children x)))))


(define article
  (make-element-transformer 
   'article
   (λ (x)
     `(div ([class "article"])
           (div ([class "title"]) 
                (a ([name ,(title->url (element-title x))]) ,(element-title x))
                nbsp
                (a ([class "diminutive"] [href "#toc"]) "[top]"))
           ,@(element-children x)))))

(define fxdoc-transformer
  (compose-transformers vim programlisting example code article))

(define (make-synonyms title xs)
  (let* ([s (apply string-append xs)]
         [synonyms 
          (map (λ (s) `(span ([class "inlineCode"]) ,s))
               (filter (λ (s) (> (string-length s) 0)) (regexp-split #rx"( |\n)+" s)))])
    `(div ([class "footnote"])
          ,@(cond
              [(empty? synonyms) empty]
              [(empty? (rest synonyms))
               `(,(first synonyms) " is a synonym for " (span ([class "inlineCode"]) ,title) ".")]
              [else
               `(
                 ,@(add-between (take synonyms (sub1 (length synonyms))) ", ")
                 " and " ,(list-ref synonyms (sub1 (length synonyms)))
                 " are synonyms for " (span ([class "inlineCode"]) ,title) ".")]))))

(define (synonyms->html x)
  (gmap/acc false
            (λ (x a)
              (cond
                [(article? x) (values x (element-title x))]
                [(and (element? x) (symbol=? (element-name x) 'synonyms))
                 (values (make-synonyms a (element-children x)) false)]
                [else (values x a)]))
            x))
              
(define (to-html x)
  (gmap fxdoc-transformer (synonyms->html (links->html x))))

(define (title->url title)
  (string-append (regexp-replace* #rx" " title "")))

(define (element-title x)
  (let ([title-pair (assoc 'title (element-attributes x))])
    (if title-pair
        (second title-pair)
        (error 'element-title "no title in ~a" x))))


(define-struct toc:contents (title items))
(define-struct toc:section (title children) #:transparent)
(define-struct toc:article (title url) #:transparent)

(define (extract-toc doc)
  (cond
    [(document? doc) 
     (make-toc:contents (element-title doc) (append-map extract-toc (element-children doc)))]
    [(chapter? doc) `(,(make-toc:section (element-title doc)
                                         (append-map extract-toc (element-children doc))))]
    [(article? doc) (list (make-toc:article (element-title doc) (title->url (element-title doc))))]
    [else empty]))

(define (flatten-articles doc)
  (cond
    [(document? doc) (append-map flatten-articles (element-children doc))]
    [(article? doc) (list doc)]
    [(chapter? doc) (append-map flatten-articles (element-children doc))]
    [(not (element? doc)) (list)]
    [else (error 'flatten-articles "unknown form: ~a" doc)]))

(define (group-by n lst)
  (if (< (length lst) n)
      (list lst)
      (cons (take lst n) (group-by n (drop lst n)))))

(define (toc->html/single toc)
  (local ([define (->html toc)
            (match toc
              [(struct toc:contents (title items))
               `(div ([class "toc"])
                     ; Jumping to #toc doesn't scroll all the way to the top
                     (div ([class "title"]) (a ([name "toc"]),title))
                     ,@(map ->html items))]
              [(struct toc:section (title children))
               `(div ([class "tocSection"])
                     (div ([class "tocTitle"]) ,title)
                     ,@(map (λ (x) `(div ([class "tocLine"]) ,@x)) (group-by 5 (map ->html children))))]
              [(struct toc:article (title url))
               `(span ([class "tocOuterLink"])
                      (span ([class "tocInnerLink"]) 
                            (a ([href ,(string-append "#" url)]) ,title)))]
              [else (error 'toc->html/single "unknown form: ~a" toc)])])
    (->html toc)))

(define (source->html doc)
  (local ([define articles (map to-html (flatten-articles doc))]
          [define toc (extract-toc doc)])
    `(html ()
           (head ()
                 (title ,(element-title doc))
                 (link ([rel "stylesheet"] [href "single-page.css"])))
           
           (body 
            ,(toc->html/single toc)
            ,@articles))))

(void (make-link-table raw-doc))

(define xhtml-dtd #<<END
<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
        "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
END
  )

(write-xexpr-to-file "../build/index.html" 
                     (source->html raw-doc)
                     xhtml-dtd)