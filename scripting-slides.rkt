#lang at-exp racket ;;-*- Scheme -*-
(require slideshow
	 slideshow/code
	 slideshow/code-pict
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

#|
(tslide "TEST SLIDE"
  (comment "\
This is a test. Do not include in final version.
"))
|#

(tslide "Another System Definition Facility, version 3"
  @bt{Why CL is now an acceptable Scripting Language}
  ~
  @t{François-René Rideau <tunes@"@"google.com>}
  (comment "\
Hi, I'm François-René Rideau, and I'm here to tell you about ASDF 3, \
the de facto standard build system for Common Lisp.

My paper is titled \"ASDF3: Why CL is now an acceptable Scripting Language\".

Indeed, one of my the too many take-home points of my paper \
is that CL is now available to compete as a scripting language \
against Unix shells, Perl, Python, Ruby, etc.

I'll explain why the last missing piece for that was ASDF 3, \
and how you can hack your own Lisp into providing the same service."))

(tslide "An Acceptable Scripting Language"
  (comment "\
First, let's see how you now can use CL as a scripting language"))

(tslide "Writing a Unix-style script in CL"
  (code
   |#!/usr/bin/cl| -sp lisp-stripper -E main
   (defun main (argv)
     (if argv
         (map () |'print-loc-count| argv)
         (print-loc-count *standard-input*))))
  (comment "\
Here is a simple script.

Here, the script \"interpreter\" is the ASDF companion program cl-launch \
that invokes your favorite Common Lisp compiler.

As you can see, I am homesteading the path /usr/bin/cl. \
The -sp option loads a system and changes the current *package* in one go. \
The -E option specifies a main function to which to pass command-line arguments \
when running the program.

This script counts lines of CL code using a library called lisp-stripper \
that strips blank lines, comments, docstrings, and extra lines in string constants."))

(tslide "Invoking Lisp code from the shell"
  (code
   |#!/bin/sh|
   |form='`#5(1 ,@`(2 3))'|
   |for l in allegro ccl clisp sbcl ecl |\\
   |      lispworks abcl cmucl gcl scl xcl ; do|
   |  cl -l $l |\\
   |     "(format t \"$l ~S~%\" $form)" |\\
   |  2>&1 |\|| grep "^$l " # LW, GCL are verbose|
   |done|)
  (comment "\
You can also invoke Common Lisp code directly from a shell script.

This simple script compares how the many implementations evaluate a same form, \
printing on each line the name of the implementation followed by the value. \

In this case, the form involves the unspecified interaction \
between known-length vector and unquote-splicing. \
The standard says that with the the hash-number-paren notation, \
whereby the reader will repeat the last form to fill a vector of specified size; \
it also says that comma-at will be spliced at read-time; \
but what happens when you do both?

That's an interesting question, but of course, \
since CL is a scripting language far superior to the Unix shell \
you could use CL instead of /bin/sh to write the same script.
"))

(tslide "Invoking external commands from CL"
  (code
   |#!/usr/bin/cl -sp inferior-shell|
   (loop with form = "`#5(1 ,@`(2 3))"
      for l in '(allegro ccl clisp sbcl ecl
                 lispworks abcl cmucl gcl scl xcl)
      do
      (run `(pipe (cl -l ,l (>& 2 1)
                      ("(format t \"" ,l " ~S~%\" "
                         ,form ")"))
              (grep ("^" ,l " "))))))
  (comment "\
The following script is doing exactly the same thing as the previous one, \
except it is written in CL.

It uses the system inferior-shell, that supports pipes, redirections, \
and user-friendly synthesis of Unix commands and their arguments.

But the point is not just to do as well as a Unix shell, but to do better."))

(tslide "Better abstractions for Scripting"
  (code
   ||
   (loop with form = "`#5(1 ,@`(2 3))"
      for l in '(allegro ccl clisp sbcl ecl
                 lispworks abcl cmucl gcl scl xcl)
      collect
      (run `(pipe (cl -l ,l (>& 2 1)
                      ("(format t \"" ,l " ~S~%\" "
                         ,form ")"))
              (grep ("^" ,l " "))) :output :forms)))
  (comment "\
And here, since you're using CL, \
you can write an expression that returns structured data, not strings. \
Ewww, strings are so uncivilized!

Structured data is a much better paradigm \
to build composable software abstractions. \
And with its ability to WRITE and READ back arbitrary symbolic expressions, \
CL still has an edge over many other languages. \
But of course, you can still exchange XML or JSON if you like."))

(tslide "Standard-based portability"
  (code
   ((ALLEGRO |#(1 2 3 2 3 2 3 2 3)|)
    (CCL |#(1 2 3 2 3 2 3 2 3))|)
    (CLISP |#(1 2 3 2 3 2 3 2 3))|)
    (SBCL |#(1 2 3 2 3 2 3 2 3))|)
    (ECL |#(1 2 3 2 3))|)
    (LISPWORKS |#(1 2 3 2 3))|)
    (ABCL |#(1 2 3))|)
    (CMUCL |#(1 2 3))|)
    (GCL |#(1 2 3))|)
    (SCL |#(1 2 3))|)
    (XCL |#(1 2 3))|)))
  (comment "\
Incidentally, here is the break down of how various implementations \
evaluate the contentious form.

For the record, my fare-quasiquote implementation agrees with ECL and LispWorks: \
if the user specified a vector of size n, the implementation should return a vector of size n.

Allegro, Clozure CL, GNU CLISP and SBCL first read a comma-at form in a vector of size n, \
then expand it; sure you can imagine how it can make sense, but this is confusing.

As for ABCL, CMUCL, GCL, SCL and XCL, they just ignore any specified size in a quasiquote context; \
I admit I find that tasteless; but of course, when the result is unspecified, \
they are allowed to send dragons flying through your nose, \
so consider yourself lucky to be given such a nice result.

So, cl-launch, ASDF, and other libraries can abstract over \
 a lot of discrepancies between CL implementations, \
but there will still remain discrepancies in many underspecified parts of the standard, \
Even then cl-launch can help you run test and experiments on all implementations."))

(tslide "What prevented scripting?"
  (comment "\
So why was scripting in CL not possible before?

What does your programming language need to possess, \
before it can be used to write scripts?"))

(tslide "What prevented scripting?"
  (para #:align 'left (t "finding source code"))
  (para #:align 'left (t "locating output files"))
  ~
  (para #:align 'left (t "command line invocation"))
  (para #:align 'left (t "argv access"))
  ~
  (para #:align 'left (t "run-program"))
  (comment "So why was scripting in CL not possible before?

Well, it was stricto sensu possibe, but completely not portable. \
Every user would have to modify every script to match his particular situation. \
There were several aspects requiring modification, that involved various amounts of pain.

Some of these aspects were universally inflicted to every common lisp user: \
finding source code and choosing output file location were basic unfulfilled needs \
before ASDF, and still painful with ASDF1. \
Without this, scripts couldn't reliably use any library or scale to large programs. \
This was not just for scripts: \
Even applications with no ambition of being distributed for execution without modification \
was difficult to configure right. \
Also, by saving the compiled output files next to the source code, \
ASDF1 and its predecessors made it impossible to share a same program \
between multiple users (at least not without security issues), \
or between multiple compilers, or multiple machines, etc. \
This might have been a mere inconvenience for writing and testing complete heavy-weight applications, \
and various kluges existed to allow to move output files aside, \
but this how a show-stopper for writing scripts.

Then there were more obvious but still quite annoying issues for writing scripts.

Invoking CL code from other a Unix shell or other program, \
and accessing arguments passed to your code, \
were non-trivial tasks, that varied wildly with your implementation.

Finally, calling an external program and extrating results was extremely difficult, \
and there again varied wildly with your implementation.

All these factors together conspired to make CL wholly unsuitable to write scripts. \
Write once, run anywhere was an unreachable dream, \
despite the large compatibility of all implementations with each other \
thanks to the CL standard."))

(tslide "What made scripting possible?"
  (para #:align 'left (t "finding source code → asdf2 (source-registry)"))
  (para #:align 'left (t "locating output files → asdf2 (output-translations)"))
  ~
  (para #:align 'left (t "command line invocation → cl-launch"))
  (para #:align 'left (t "argv access → cl-launch, asdf3 (uiop)"))
  ~
  (para #:align 'left (t "run-program → asdf3 (uiop), inferior-shell"))
  (comment "\
These issues have now been addressed.

The most pressing issues with CL, not specific to scripting, were solved by ASDF 2. \
With its source-registry, locating source became modular, \
and you didn't need per-program, per-library or even per-user configuration \
of where to find code. \
With the output-translations layer, you could the share source code tree \
between multiple users, multiple implementations, multiple machines, \
and there would be no clash or additional security issues.

Invoking CL programs in a uniform way was made possible by cl-launch. \
cl-launch was actually written before ASDF2, but it was made simpler and more powerful \
with ASDF2's output-translations and with ASDF3's portability layer UIOP.

As for invoking external programs from CL and capturing their output nicely, \
this was initially made possible by XCVB and its xcvb-driver, \
and moved into ASDF3's portability layer UIOP and further developed there. \
A more usable layer is available in the system inferior-shell,
that I demonstrated just before."))

(tslide "Finding source code (before)"
  @para[#:align 'left]{Q: where is system @tt{foo} ?}
  @para[#:align 'left]{The hard way: modify every client}
  @para[#:align 'left]{logical-pathname: system and client must agree}
  @para[#:align 'left]{ASDF: user maintains a link farm to .asd files}
  @para[#:align 'left]{but how to configure? @tt{~/.sbclrc}, etc.}
  (comment "\
First to locate the source code of the various systems, \
each user had to specially configure his Lisp implementation \
in a non-portable way. \
Back in the dark ages, every program that used libraries had to be modified to load them, \
or had to rely on a load script that would be modified to load them first from where they are. \
Then came logical pathnames, and if your system was modified once used them, \
and you could assume that they were properly configured, \
load scripts could just use them; \
except that turned every user into a system administrator, who needed to configure them. \
ASDF1 made clever use of *load-truename* and allowed you to configure a *central-registry* \
to locate .asd system definition files; \
then, source code itself never needed to be modified again, \
but every user had to become system administrators and manage a link farm of .asd files. \
And still, there was a problem, because you needed to configure things early, \
so ideally in an initialization file;
even more so since ASDF itself needed to be loaded before it could be configured, \
and that itself necessitated special steps \
that depended on the implementation and/or the path at which you had installed ASDF. \
All that meant you couldn't assume code was present, \
each user had to be his own system administrator of sorts. \
But not every implementation has an initialization file and those that do each have a different one! \
Moreover, initialization files can interfere in subtle ways \
with assumptions a script may legitimately make. \
Shell Scripts tend to purposefully ignore initialization files for this reason — \
but then Lisp scripts wouldn't be able to do that, and/or \
users can't put all the customization they want. \
That was a big mess"))

(tslide "Finding source code (after)"
  @para[#:align 'left]{ASDF 2: source-registry}
  (comment "\
ASDF 2 solved that by introducing the source-registry;
previous central-registry is still supported for backward compatibility.")
  @para[#:align 'left]{Implementation-independent}
  (comment "\
It's implementation-independent; \
it does not rely on an implementation-dependent configuration file that might not exist.")
  @para[#:align 'left]{Nice DSL}
  (comment "\
It has a nice flexible DSL to specify paths, so you can refer to the home directory, \
to a string that identifies the implementation, including its version, \
its salient configuration features, \
the operating system and hardware architecture, etc.")
  @para[#:align 'left]{Can recurse into subtrees}
  (comment "\
Unlike the ASDF1 central-registry, the ASDF2 source-registry can recurse into subtrees; \
no more having to manually scan directories and manually update link farms when the libraries \
are removed, added or modified.")
  @para[#:align 'left]{Prog > Env > User > Sys > Defaults}
  (comment "\
The ASDF2 source-registry have a nice way to get configuration from various sources \
and merge them so that the program can override the environment that can override \
user configuration files that can override system configuration files that can override defaults.")
  @para[#:align 'left]{Sensible defaults}
  (comment "\
The ASDF2 source-registry provides sensible defaults that will work with your implentation, \
with systems provided by your Linux distribution (e.g. Debian), etc.")
  @para[#:align 'left]{ASDF 3.1: @tt{~/common-lisp/}}
  (comment "\
ASDF3 introduces a universal pre-configured location, ~/common-lisp/
in which to put your code"))

(tslide "Finding source code (results)"
  @para[#:align 'left]{@it{Who knows specifies, who doesn't needn't}}
  (comment "\
Important principle of design.")
  @para[#:align 'left]{It @it{just works} by default}
  (comment "\
Important principle of design.")
  @para[#:align 'left]{Modular configuration}
  (comment "\
You can always override what other people have broken or what you want to improve.")
  @para[#:align 'left]{Better than in C!}
  (comment "\
If you develop a programming language and its build system, you may want a similar mechanism. \
The result compares very favorably with LD_LIBRARY_PATH, pkg-config, etc.")
  @para[#:align 'left]{Reusable DSL for pathname designators}
  (comment "\
Can be used by other configuration DSLs."))

#|
  (comment "\
Then, to share source code available on a system-wide basis between multiple users, \
or to use the same source code it with different implementations, you needed to somehow \
segregate output files per user, per implementation, and that was
not generally possible before ASDF, and quite hard to configure with ASDF 1. \
Or you could have loaded every file from source every time, \
but that would be very slow and would not scale to large files and big libraries.")
  (comment "\
The way you invoke Lisp depends on the compiler you use, \
and so does the way you get to command-line arguments. \
Even to invoke a script

And to spawn external programs, you would need to invoke very different \
variants of run-program on every implementation, and \
capturing the output would be a huge pain."))

program-op
image-op

image-dump-hook
image-restart-hook

|#

#|
Different talk!
(tslide "Another System Definition Facility, version 3"
  @para[#:align 'center]{A @tt{traverse} across the build}
  ~
  ~
  (comment "Why the hell is there an ASDF 3?"))

|#
