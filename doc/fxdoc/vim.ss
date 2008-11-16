#!/usr/bin/env mzscheme
#lang scheme
(require 
 scheme/system
 (only-in html read-html-as-xml)
 "xexpr.ss")

(define jedit-command "vim") ; fuck

;;; Derived from http://www.jukie.net/~bart/blog/generating-colourized-source-html
(define format-command
  (string-append 
   "~a -u /dev/null" ; placeholder for vim-command, don't use a .vimrc file
   " -c \"set bg=light\""
   " -c \"syntax on\""
   " -c \"run! syntax/2html.vim\""
   " -c \"w! ~a\"" ; placeholder for the temporary file--destination
   " -c \"q\" -c \"q\"" ; close both buffers; this quits vim too
   " \"~a\"" ; placeholder for the source file
   ))

(define (exec-vim in-file out-file)
  (let ([cmd (format format-command jedit-command out-file in-file)])
    (display cmd) (newline)
    (system cmd)))

; make-temp-name/suffix :: path? -> path?
; Constructs a "temporary" path name, preserving the suffix of the given path.
; The name may already exist and this function may not work on Windows.
(define (make-temp-name/suffix path)
  (let-values ([(base name dir?) (split-path path)])
    (build-path base (string->path-element (format "vim-format-~a" (path->string name))))))

(define (unyellow attrs)
  (transform-attribute 'color attrs
                       (λ (v) 
                         (match v
                           ["#00ffff" "red"]
                           ["#ffff00" "blue"]
                           [else v]))))

(define (recolor x)
  (gmap 
   (make-element-transformer 
         'font (λ (x) `(font ,(unyellow (element-attributes x)) ,@(element-children x))))
   x))
                                  

(define (remove-html-wrapper vim-xexpr)
  `(div () ,@(map recolor (drop (list-ref vim-xexpr 3) 5))))

; vim-format : path? -> xexpr? or false?
(define (vim-format-file file)
  (let ([temp-path (make-temp-name/suffix file)])
    (if (exec-vim (path->string file) (path->string temp-path))
        (let* ([in-port (open-input-file temp-path)]
               [result
               (remove-html-wrapper (xml->xexpr 
                                     (first (read-html-as-xml in-port))))])
          (close-input-port in-port)
          (delete-file temp-path)
          result)
        (begin
          (delete-file temp-path)
          false))))

; vim-format-string : string? string? -> xexpr? or false?
(provide/contract
 (vim-format-string (string? string? . -> . (or/c xexpr? false?))))
(define (vim-format-string str ext)
  (let ([temp-path (make-temporary-file 
                    (string-append "~a" ext)
                    false
                    ".")])
    (with-output-to-file temp-path (λ () (display str)) #:exists 'truncate)
    (let ([result (vim-format-file temp-path)])
      (delete-file temp-path)
      result)))

(provide vim)
(define-element-transformer (vim x)
  (or (vim-format-string (apply string-append (element-children x))
       (get-attribute 'ext x))
      (error 'vim-transformer "failed on ~a" x)))

; Command-line mode!
#;(when (not (zero? (vector-length (current-command-line-arguments))))
  (display (vim-format-file (vector-ref (current-command-line-arguments) 0))))