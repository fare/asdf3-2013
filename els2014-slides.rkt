#lang at-exp racket ;;-*- Scheme -*-
(require slideshow
	 slideshow/code
	 scheme/gui/base
	 (except-in "utils.rkt" system module force file eval error))

;; TODO: have multiple presentations?
;; (1) using CL as a scripting language, also, deploying executables and image life-cycle hooks.
;; (2) the story of bug that begat ASDF3, exploring subtleties in the ASDF dependency model,
;;   and a surprising conclusion. [My article's Appendix F]
;; (3) laundry list of new features in ASDF3.1 since ASDF2.
;; (4) LAMBDA, the Ultimate Culture War — Live Programming vs Cult-of-Dead programs.
;; (5) other (please specify).

(define ~ @t{ })
(define *blue* (make-object color% "blue"))
(define *red* (make-object color% "red"))
(define *grey* (make-object color% 200 200 200))
(define (url x) (colorize (tt x) *blue*))
(define (red x) (colorize x *red*))
(define (grey x) (colorize x *grey*))

(define (emph x) (red x))

(define (title x) (text x (cons 'bold 'default) 38))

(define slides
  (make-keyword-procedure
   (lambda (kws kvs repeats . lines)
     (for ([i repeats])
       (let ((m (λ (xs) (map (λ (x) (x i)) xs))))
	 (keyword-apply
	  slide kws (m kvs) (m lines)))))))

(define (always x) (lambda (i) x))
(define (repeat-fun test n iftesttrue [iftestfalse ~] [ifnorepeat ~])
  (lambda (i)
    (cond
     ((not i) ifnorepeat)
     ((test i n) iftesttrue)
     (#t iftestfalse))))
(define (if= n x [y ~] [z ~]) (repeat-fun = n x y z))
(define (if<= n x [y ~] [z ~]) (lambda (i) (if (<= i n) x y)))
(define (if>= n x [y ~] [z ~]) (lambda (i) (if (>= i n) x y)))

(define (?highlight n m object [shaded grey] [highlit red] [normal identity])
  (if n
      (if (eqv? n m)
	  (highlit object)
	  (shaded object))
      (normal object)))

(define (tASDF3 . x)
  (title (apply string-append "ASDF 3: " x)))

(define (tslide _title . body)
  (keyword-apply slide '(#:title) (list (title _title)) body))

(tslide "Another System Definition Facility, version 3"
  @bt{Why CL is now an acceptable Scripting Language}
  ~
  @t{François-René Rideau <tunes@"@"google.com>}
  (comment "\
Hi, I'm François-René Rideau, and I'm here to tell you about ASDF 3, \
THE build system for Common Lisp. \
The paper is titled \"ASDF3: Why CL is now an acceptable Scripting Language\".
"))

(tslide "An Acceptable Scripting Language"
  (para #:align 'left (tt"#!/usr/bin/cl -sp lisp-stripper -E main"))
  (para #:align 'left (tt"(defun main (argv)"))
  (para #:align 'left (tt"  (if argv"))
  (para #:align 'left (tt"      (map () 'print-loc-count argv)"))
  (para #:align 'left (tt"      (print-loc-count *standard-input*)))"))
  (comment "Indeed, in the paper, I'm boasting that thanks to ASDF 3, \
you can now write scripts like this, \
where the script \"interpreter\" is the ASDF companion program cl-launch \
that invokes your favorite Common Lisp compiler. \
As you can see, it can load a system and change the current *package* in one go, \
and invoke a main function passing it command-line arguments. \
In this case, this script counts lines of code using a library that strips \
blank lines, comments, docstrings, and extra lines in string constants."))

(tslide "An Acceptable Scripting Language (2)"
  (code
  (para #:align 'left (tt"for l in sbcl ccl clisp cmucl ecl abcl \\"))
  (para #:align 'left (tt"         scl allegro lispworks gcl xcl ; do"))
  (para #:align 'left (tt" cl -l $l -i \\"))
  (para #:align 'left (tt" '(format t \"'$l': ~S~%\" `#5(1 ,@`(2 3)))' \\"))
  (para #:align 'left (tt" 2>&1 | grep \"^$l:\" # LW, GCL are verbose"))
  (para #:align 'left (tt"done"))
  (comment "Or like this where you can invoke Common Lisp code from a script, \
and even compare how many implementations interpret some program."))

(tslide "What prevented scripting?"
  ()
  (comment "And indeed, I'm boasting that thanks to ASDF 3,
you can now write scripts like this"))

(tslide "Another System Definition Facility, version 3"
  @para[#:align 'center]{A @tt{traverse} across the build}
  ~
  ~
  (comment "Why the hell is there an ASDF 3?"))

