#lang at-exp racket
;;-*- Scheme -*-

(require
  scribble/manual
  scribble/core
  scribble/decode
  scribble/base
  scriblib/autobib
  scribble/bnf
  (for-syntax syntax/parse))

(provide (all-defined-out))

(define backend (make-parameter '#:html))
(define-syntax-rule (html-only x ...) (when (eq? (backend) '#:html) (list x ...)))
(define-syntax-rule (pdf-only x ...) (when (eq? (backend) '#:pdf) (list x ...)))

(define multiple-sections (make-parameter #f))
(define include-asdf1-diffs (make-parameter #t))
(define include-asdf2-diffs (make-parameter #t))

(define (moneyquote . x) (bold x))
(define (q . x) (list "\"" x "\""))

(define-syntax (clblock stx)
  (syntax-parse stx
    [(_ #:line-numbers ln str ...)
     #'@codeblock[;;#:keep-lang-line? #f
                   #:line-numbers ln
                   #:line-number-sep 3
                   str ...]]
    [(_ str ...)
     #'(clblock #:line-numbers 0 str ...)]))

(define-syntax (clcode stx)
  (syntax-parse stx
    [(_ str ...) #'(clblock #:line-numbers #f str ...)]))

(define (cl . str)
  (apply tt str))

(define (CL) "Common Lisp")
(define (CLOS) "Common Lisp Object System")

(define (XXX . rest) '())
(define (latin x) (emph x))
(define (de_facto) @latin{de facto})
(define (bydef . x) (emph x))

(define (software-version software (version #f))
  (if version (list (cl software) (format " ~a" version)) (cl software)))
(define (ASDF (version #f)) (software-version "ASDF" version))
(define (ASDF1) (ASDF 1))
(define (ASDF2) (ASDF 2))
(define (ASDF3) (ASDF 3))
(define (cl-launch (version #f)) (software-version "cl-launch" version))
(define (ASDF-Install) @cl{ASDF-Install})
(define (UIOP) @cl{UIOP})
(define (POIU) @cl{POIU})
(define (XCVB) @cl{XCVB})
(define (DEFSYSTEM) @cl{DEFSYSTEM})
(define (defsystem) @cl{defsystem})
(define (mk-defsystem) @cl{mk-defsystem})
(define (Make) @tt{Make})
(define (Quicklisp) "Quicklisp")

(define-cite ~cite cite-noun generate-bib)

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))

