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

(define extended-url "http://fare.tunes.org/files/asdf3/asdf3-2014.html")

(define variant (make-parameter '#:extended))
(define (extended?) (eq? (variant) '#:extended))
(define-syntax-rule (extended-only x ...) (when (extended?) (list x ...)))
(define-syntax-rule (short-only x ...) (unless (extended?) (list x ...)))
(define (appref tag alt)
  (if (extended?)
      (secref tag)
      (hyperlink (string-append extended-url "#" tag) alt)))

(define backend (make-parameter '#:html))
(define-syntax-rule (html-only x ...) (and (eq? (backend) '#:html) (list x ...)))
(define-syntax-rule (pdf-only x ...) (and (eq? (backend) '#:pdf) (list x ...)))

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
(define (ad_hoc) @latin{ad hoc})
(define (de_facto) @latin{de facto})
(define (bydef . x) (emph x))

(define (software-version software (version #f))
  (if version (list (cl software) (format " ~a" version)) (cl software)))
(define (ASDF (version #f)) (software-version "ASDF" version))
(define (ASDF1) (ASDF 1))
(define (ASDF2) (ASDF 2))
(define (ASDF3) (ASDF 3))
(define (ASDF3.1) (ASDF 3.1))
(define (cl-launch (version #f)) (software-version "cl-launch" version))

(define-syntax defpretty
  (lambda (stx)
    (syntax-case stx ()
      [(_ pretty name ...)
       (with-syntax ([(string ...) (map symbol->string (syntax->datum #'(name ...)))])
         #'(begin
             (define (name) (pretty string)) ...))])))

(defpretty cl
  ASDF-Install UIOP POIU XCVB DEFSYSTEM defsystem mk-defsystem
  quick-build faslpath asdf/package-inferred-system

  run-program inferior-shell run run/nil run/string run/ss

  nil error cerror warning eval
  defpackage
  pathname merge-pathnames make-pathname truename *load-pathname* *load-truename*
  parse-namestring namestring
  directory probe-file

  define-package
  merge-pathnames* subpathname truenamize
  parse-unix-namestring unix-namestring parse-native-namestring native-namestring
  getcwd

  operation load-op compile-op prepare-op test-op
  load-fasl-op fasl-op load-bundle-op compile-bundle-op concatenate-source-op
  program-op image-op build-op
  upward-operation downward-operation sideway-operation selfward-operation non-propagating-operation

  component system module file cl-source-file component-children package-inferred-system

  operate load-system

  traverse traverse-action compute-action-stamp operation-done-p needed-in-image-p

  component-depends-on output-files input-files perform perform-with-restarts
  action-depends-on)

(define (*dpd*) @cl{*default-pathname-defaults*})

(defpretty emph depends-on in-order-to do-first force)

(defpretty tt Make make blaze)
(define (Quicklisp) "Quicklisp")
(define (asd) @tt{.asd})

(define-cite ~cite cite-noun generate-bib) ;; #:style number-style)

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))

(define (shell . x) (cl x))
(define (dashdash . x) (list (tt "-") (tt "-") (apply shell x)))
