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

@section{Creating Basic ASDF Systems}

@subsection{Simple ASDF File}

@cl{:depends-on} between systems.

@subsection{Serial Dependencies}

@subsection{Explicit Dependencies}

@subsection{Modules}

@section{Advanced ASDF Systems}

