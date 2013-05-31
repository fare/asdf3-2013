#lang scribble/sigplan @nocopyright

@(require scribble/base
          scribble/manual
          "utils.rkt")

@;TODO: Parameterize whether or not to include notes about ASDF1 and ASDF2
@;TODO: Have CSS output hide it or not, a la Google 

@(define-syntax L (syntax-rules () ((_ x ...) (smaller (code x ...)))))
@(define (P . x)
  (cond
    ((null? x) '())
    ((null? (cdr x)) (car x))
    (#t (list (car x) (linebreak) (apply P (cdr x))))))

@title{@(ASDF) Tutorial}

ASDF (Another System Definition Facility) has been the de facto standard
Common Lisp build system for over ten years.
I recently rewrote it completely, several times,
all along mostly preserving backwards compatibility.
The latest incarnation, ASDF 3, in addition
to fixing deep design bugs older than ASDF itself,
also includes extensive portability library, UIOP,
not to be confused with an existing parallelizing extension, POIU.
I will show how to use ASDF, explain the recent improvements, and
discuss the challenges of writing portable Common Lisp programs
and what that means for the past and future of Lisp.

<Xach> I would like more scenario-driven examples,
e.g. "If you have a piece that has to be compiled via a separate process,
here's how the system file can help you do it."
Not ASDF3-specific, I'd like to see more things like that in general.
"If you want your FASL files for project/ to be stored in project/fasls/ rather than globally,
here's how to do it."
I suppose that's more manual than tutorial

So, how do we use @(ASDF) in practice?

Let's start with the easiest examples...

@section{Using ASDF}

@subsection{Doing without ASDF}

In the simplest case, you could do without @(ASDF):
just write Lisp code in a file, then @cl{(load ...)} it from your REPL,
or otherwise @L{C-c C-k} compile and load using
@hyperlink["http://common-lisp.net/project/slime/"]{SLIME}:

@L{(defun average (list) (/ (reduce '+ list :initial-value 0) (length list)))}

You could also make you code callable from the shell command-line;
call the following with @code{sh avg.lisp 1 2 3 4}:

@P[@L{":" ; exec sbcl --noinform --load "$0" --eval '(quit)' -- "$@"@"" # avg.lisp}
@L{(defun average (list) (/ (reduce '+ list :initial-value 0) (length list)))}
@L{(format t "~G~%" (average (mapcar 'parse-integer (cddr sb-ext:*posix-argv*))))}]

Load scripts. Initialization and setup.

This is all good and well, but comes with limitations:
without using @(ASDF),
you can't use the many free software libraries that rely on it;
there are a few libraries that don't but you'll have to roll your own way
of tracking which library to load and where to locate their code.
Also, to have any interaction with the computer
beyond the little that is standardized in the @(CL) spec,
you have to learn and use whatever few
idiosyncratic extensions to the @(CL) standard
are provided by your particular implementation.

@subsection{Simple file calling ASDF}

@section{More complex build file}

@section{Using CL-Launch}

@section{Configuring ASDF}

@subsection{Default Installation Paths}

@subsection{Source Registry}

@subsection{Output Translations}

@subsection{Using Quicklisp and clbuild}

@subsection{Finding Libraries}

How do I find a library that does what I want?

Where do I download it?

@subsection{Downloading Libraries}

@section{Creating Basic ASDF Systems}

@subsection{Simple ASDF File}

@cl{:depends-on} between systems.

@subsection{Serial Dependencies}

Scope of :serial t is the current module or system,
not its submodules or systems.

Serial parallel by nesting modules.
:pathname ""

@subsection{Explicit Dependencies}

foo/bar

../sibling/baz

@subsection{Modules}

@subsection{Other files}

README

LICENSE

TODO

.git

@subsection{Packages}

traditional one-per-system (or per subsystem, however defined)

interface package versus implementation package (possibly two systems, too)

one-package-per-file quickbuild style

uiop:define-package vs defpackage

@subsection{Using Quickprojects}

@section{Advanced ASDF Systems}

@subsection{Character Encodings}

@subsection{Finalizers}

@subsection{Using Extensions: CFFI Grovel}

@subsection{Load-only class}

Beware: defeats executable creation!

Maybe instead you want run-time evaluation
(foo '(some data))
or even
(eval '(some expression))

@section{The ASDF object model}

@subsection{Components, Operations, Actions}

Components describe how your source code is organized.

Operations describe processes or stages of processing of a component.

Actions are pairs of an operation and a component on which to effect the operation.

@emph{The dependency graph is a graph of actions, not of components.}

@subsection{Lisp Components}

system, module

cl-source-file
(also cl-source-file.lsp, cl-source-file.cl)

@subsection{Lisp Operations}

load-op, compile-op. Propagate downward.

prepare-op. Propagate upward; Also sideway load-op of dependencies.

@subsection{Static Plan then Act}

Traverse.

sequential-plan.

POIU.

@section{Troubleshooting}

@subsection{Backtrace}

Size: what is the biggest solution? the smallest?

@subsection{Trace}

What should I trace?
input

@subsection{Error}

