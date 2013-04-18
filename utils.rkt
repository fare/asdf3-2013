#lang at-exp racket
;;-*- Scheme -*-

(require
  scribble/base)
#|
(require
  scribble/core
  scribble/decode
  scriblib/autobib
  scribble/manual
  scribble/bnf
  (for-syntax syntax/parse))
|#

(provide
  backend html-only pdf-only
  multiple-sections
  include-asdf1-diffs include-asdf2-diffs
  XXX cl latin de_facto
  ASDF ASDF1 ASDF2 ASDF3 ASDF-Install POIU XCVB DEFSYSTEM defsystem mk-defsystem Make)

(define backend (make-parameter '#:html))
(define-syntax-rule (html-only x ...) (when (eq? (backend) '#:html) (list x ...)))
(define-syntax-rule (pdf-only x ...) (when (eq? (backend) '#:pdf) (list x ...)))

(define multiple-sections (make-parameter #f))
(define include-asdf1-diffs (make-parameter #t))
(define include-asdf2-diffs (make-parameter #t))

(define (XXX) '())
(define (cl x) (tt x))
(define (latin x) (emph x))
(define (de_facto) @latin{de facto})

(define (ASDF) @cl{ASDF})
(define (ASDF1) (list @cl{ASDF} " 1"))
(define (ASDF2) (list @cl{ASDF} " 2"))
(define (ASDF3) (list @cl{ASDF} " 3"))
(define (ASDF-Install) @cl{ASDF-Install})
(define (POIU) @cl{POIU})
(define (XCVB) @cl{XCVB})
(define (DEFSYSTEM) @cl{DEFSYSTEM})
(define (defsystem) @cl{defsystem})
(define (mk-defsystem) @cl{mk-defsystem})
(define (Make) @tt{Make})
