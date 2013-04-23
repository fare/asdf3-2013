#lang at-exp racket
;;-*- Scheme -*-

(require
  scribble/core
  scribble/decode
  scribble/base
  scriblib/autobib
  scribble/manual
  scribble/bnf
  (for-syntax syntax/parse))

#|
(require
|#

(provide
  code
  backend html-only pdf-only
  multiple-sections include-asdf1-diffs include-asdf2-diffs
  cl clcode
  XXX latin de_facto CL
  ASDF ASDF1 ASDF2 ASDF3 ASDF-Install POIU XCVB DEFSYSTEM defsystem mk-defsystem Make)

(define backend (make-parameter '#:html))
(define-syntax-rule (html-only x ...) (when (eq? (backend) '#:html) (list x ...)))
(define-syntax-rule (pdf-only x ...) (when (eq? (backend) '#:pdf) (list x ...)))

(define multiple-sections (make-parameter #f))
(define include-asdf1-diffs (make-parameter #t))
(define include-asdf2-diffs (make-parameter #t))

(define-syntax (clblock stx)
  (syntax-parse stx
    [(_ #:line-numbers ln str ...)
     #'@nested[#:style "smaller"]{
        @codeblock[;;#:keep-lang-line? #f
                   #:line-numbers ln
                   #:line-number-sep 3
                   str ...]}]
    [(_ str ...)
     #'(clblock #:line-numbers 0 str ...)]))

(define-syntax (clcode stx)
  (syntax-parse stx
    [(_ str ...) #'(clblock #:line-numbers #f str ...)]))

(define-syntax-rule (cl str ...)
  @code[#|#:lang "cl"|# str ...])

(define (CL) "Common Lisp")

(define (XXX . rest) '())
(define (latin x) (emph x))
(define (de_facto) @latin{de facto})

(define (ASDF . x) @cl[(apply string-append "ASDF" (if (null? x) '() (cons " " x)))])
(define (ASDF1) (ASDF "1"))
(define (ASDF2) (ASDF "2"))
(define (ASDF3) (ASDF "3"))
(define (ASDF-Install) @cl{ASDF-Install})
(define (POIU) @cl{POIU})
(define (XCVB) @cl{XCVB})
(define (DEFSYSTEM) @cl{DEFSYSTEM})
(define (defsystem) @cl{defsystem})
(define (mk-defsystem) @cl{mk-defsystem})
(define (Make) @tt{Make})
