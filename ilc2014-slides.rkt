#lang at-exp racket ;;-*- Scheme -*-
(require slideshow
	 slideshow/code
	 slideshow/code-pict
	 scheme/gui/base
	 (except-in "utils.rkt" system module force file eval error))

(define ~ @t{ })
(define *blue* (make-object color% "blue"))
(define *red* (make-object color% "red"))
(define *grey* (make-object color% 200 200 200))
(define (url x) (colorize (tt x) *blue*))
(define (red x) (colorize x *red*))
(define (grey x) (colorize x *grey*))

(define (emph x) (red x))

(define (title x) (text x (cons 'bold 'default) 38))

(define (P . x) (para #:align 'left x))
;;(define (B x) (text x (cons 'bold 'default) 28))
(define (B x) (bt x))

(define (tslide _title . body)
  (keyword-apply slide '(#:title) (list (title _title)) body))

(tslide "Scripting with Lisp"
  @bt{Using Common Lisp as a Scripting Language}
  ~
  ~
  @t{François-René Rideau <tunes@"@"google.com>}
  ~
  ~
  @t{International Lisp Conference, 2014-08-17}
  (comment "\
Hi, I'm François-René Rideau.

Today, I will demonstrate how to use Lisp as a Scripting Language.
"))

(tslide "Ways that CL beats scripting languages"
@t{
  BOTH lexical variables AND dynamic variables
}
@t{
  Recursive data structures, not mere string manipulation.
}
@t{
  Native read-write invariance, not manual serialization.
}
@t{
  Higher-order functions, not just top-level functions.
}
@t{
  Pattern-matching, not just regular expressions.
}
@t{
  Interactive debugging, not just error (back)traces.
}
@t{
  Condition handling, not just global traps
}
@t{
  An advanced Object System, not just multiple inheritance.
}
@t{
  Native code compilation, not just script interpreters.
}
(comment "And I haven't even talked about macros or proper GC..."))

(tslide "Ways scripting languages used to beat CL"
@P{"Scripting" (whatever that means)}
~
@P{Popularity (seems to follow from "scripting")}
~
@P{Plenty of libraries (follows from "popularity")}
~
@P{Familiarity (follows from "popularity")}
(comment "So the key feature seems to be scripting. But what is it?"))


(tslide "What is Scripting?"
 @P{Low-overhead, frictionless programming}
 ~
 @P{Can reuse other people's code}
 ~
 @P{Can write code reusable by others}
 ~
 @P{Division of Labor}
 (comment "\
Each shall be able to contribute his increment of code \
without having to first write boilerplate or frameworks \
or reinvent the wheel.
"))

(tslide "CL is NOW acceptable at scripting"
 @B{Software Modularity}
 @t{mk-defsystem (1991); ASDF 1 (2002), 2 (2010), 3 (2013), 3.1 (2014)}

 @B{Software Distribution}
 @t{asdf-install (2003); clbuild 1 (2007), 2 (2010); quicklisp (2010)}

 @B{Run CL code from Unix}
 @t{cl-launch 1 (2005), 2 (2006), 3 (2012), 4 (2014)}

 @B{Run Unix code from CL}
 @t{xcvb-driver (2009), uiop (2013); inferior-shell 1 (2012), 2 (2014)}
 (comment "\
I have written before Why Common Lisp is Now an Acceptable Scripting Language.

It used not to be. Several pieces were missing \
that only became barely acceptable recently; \
some only became actually good this very year.

First, you need a usable mechanism to combine software modules. \
In CL, it's called defsystem, and it's only done \
in a fully portable and satisfactory manner since this year; \
ASDF 3 was basically there, but ASDF 3.1 completes it and fixes many issues.

Then, you need an effective place from which to get modules and their dependencies, \
and to which to publish them. \
In CL, there used to be asdf-install, but its weak coordination model \
couldn't scale to a large number of evolving, mutually-dependent libraries. \
Quicklisp now does it without requiring the amount of administration.

You need to be able to run CL programs from any other language, \
and cl-launch has made it portably possible for developers for many years, \
and the latest version is usable by end-users thanks to ASDF3.

You need CL programs to be able to run programs in other languages; \
and xcvb-driver has made it portably possible recently, \
and thanks to UIOP, part of ASDF3, it's now improved and available everywhere; \
inferior-shell has been offering a nicer interface on top of that
for splicing strings in arguments and building pipes and redirections. \
There are precious few reasons to write a shell script when you could write \
a CL script instead.
"))

(tslide "Low overhead"
 @P{One-line script invocation.}
 (code |#!/usr/bin/cl|)
 ~
 @P{Write Once, Run Most-anywhere (WORM)}
 @P{Any OS, Implementation combination}
 ~
 @P{No sysadmining required}
 @P{No editing scripts, configuration, etc.}

 (comment "OK, so on Windows, you need cygwin or some other hack"))

(tslide "Writing a Unix-style script in Lisp"
  (code
   |#!/usr/bin/cl| -sp lisp-stripper -E main
   (defun main (argv)
     (if argv
         (map () |'print-loc-count| argv)
         (print-loc-count *standard-input*)))
   ||
   ||
   ||
   |_lispwc *.lisp|)
  (comment "\
Here is a simple script.

Here, the script \"interpreter\" is the ASDF companion program cl-launch. \
It invokes your favorite Common Lisp compiler to run the script.

As you can see, I am homesteading the path /usr/bin/cl. \
The -sp option loads a system and changes the current *package* in one go. \
The -E option specifies a main function to which to pass command-line arguments \
when running the program.

This script counts lines of CL code using a library called lisp-stripper \
that strips blank lines, comments, docstrings, and extra lines in string constants."))

(tslide "Invoking CL code from the shell"
  (code
   |#!/bin/sh|
   ||
   |form='`#5(1 ,@`(2 3))'|
   ||
   |for impl in allegro ccl clisp sbcl ecl |\\
   |            lispworks abcl cmucl gcl scl xcl ; |\\
   |do |
   |_  cl -l $impl |\\
   |      "(format t \"$impl ~S~%\" $form)" |\\
   |  2>&1 |\|| grep "^$impl " # LW, GCL are verbose|
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

(tslide "Invoking external commands from Lisp"
  (code
   |#!/usr/bin/cl -sp inferior-shell|
   ||
   (loop with form = "`#5(1 ,@`(2 3))"
      ||
      for impl in '(allegro ccl clisp sbcl ecl
                    lispworks abcl cmucl gcl scl xcl)
      do
      (run `(pipe (cl -l ,impl (>& 2 1)
                      ("(format t \"" ,impl " ~S~%\" "
                         ,form ")"))
              (grep ("^" ,impl " "))))))
  (comment "\
The following script is doing exactly the same thing as the previous one, \
except it is written in CL.

It uses the system inferior-shell, that supports pipes, redirections, \
and user-friendly synthesis of Unix commands and their arguments.

But the point is not just to do as well as a Unix shell, but to do better."))

(tslide "Better abstractions for scripting"
  (code
   ||
   ||
   (loop with form = "`#5(1 ,@`(2 3))"
      ||
      for impl in '(allegro ccl clisp sbcl ecl
                    lispworks abcl cmucl gcl scl xcl)
      collect
      (run `(pipe (cl -l ,impl (>& 2 1)
                      ("(format t \"" ,impl " ~S~%\" "
                         ,form ")"))
              (grep ("^" ,impl " "))) :output :forms)))
  (comment "\
And here, since you're using CL, \
you can write an expression that returns structured data, not strings. \
Ewww, strings are so uncivilized!

Structured data is a much better paradigm \
to build composable software abstractions. \
And with its ability to WRITE and READ back arbitrary symbolic expressions, \
CL still has an edge over many other languages. \
But of course, you can still exchange XML or JSON if you like."))

(tslide "Standards-based portability"
  (code
   |_`#5(1 ,@`(2 3))|
   ||
   ||
   ((ALLEGRO |#(1 2 3 2 3 2 3 2 3)|)
    (CCL |#(1 2 3 2 3 2 3 2 3))|)
    (CLISP |#(1 2 3 2 3 2 3 2 3))|)
    (SBCL |#(1 2 3 2 3 2 3 2 3))|)
    (ECL |#(1 2 3 3 3))|)
    (LISPWORKS |#(1 2 3 3 3))|)
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

(tslide "Now Portable"
  @P{invocation: main, argv}
  ~
  @P{modularity: image dump and restore hooks}
  ~
  @P{access: run-program}
  ~
  @P{runtime: pathnames, getenv, temp files...})

(tslide "Users need not be developers"
  @P{No editing paths in source files}
  ~
  @P{No ./configure ; make ; make install}
  ~
  @P{No object files in your or each other's way}
  ~
  @P{Just invoke the script, click, etc.})

(tslide "Users need not be sysadmins"
  @P{No editing paths in configuration files}
  ~
  @P{No deploy step before use}
  ~
  @P{Share source code, not object code}
  ~
  @P{Sysadmining left to sysadmin and easy.})

(tslide "Modularity"
  @P{defsystem}
  ~
  @P{Just download source into a registered tree}
  ~
  @P{Refer to code by name, not path}
  ~
  @P{Cost: scanning tree})

(tslide "Lisp is the Virtual Machine"
  @P{Bytecode 40: begin program}
  @P{Bytecode 41: end program}
  ~
  @P{fasl cache: per-user persistent file-grained JIT}
  ~
  @P{Same (non) config for compile-time vs runtime}
  ~
  @P{NOW much better than C, as good as Python})

(tslide "Easier delivery with bundle operations"
  @P{Deliver an executable: @tt{cl-launch}}
  ~
  @P{Deliver a library: @tt{asdf:compile-bundle-op}}
  ~
  @P{Deliver code as only one or two files!}
  (comment "\
On implementations that don't support standalone executables, \
the delivery will have to be in two files: \
an image, and a launch shell script; \
that plus the Lisp implementation, if the image isn't executable.

From Lisp, you can use asdf:program-op and asdf:image-op, \
but beware that on some implementations, this causes Lisp to quit. \
Often, the solution would be to fork before you dump an image, \
but forking is not available on all those implementations!
"))

(tslide "Image Life-cycle support"
  @P{Need to use environment variables?}
  ~
  (code
   (uiop:register-image-dump-hook 'clear-env-vars)
   ||
   (uiop:register-image-restore-hook 'init-env-vars))
  (comment "\
You don't want to leak build environment information \
into your executable binaries. \
It's not just an issue that makes your build harder to reproduce and bugs harder to track. \
It's not just a potential source of production bugs that are not detected during testing. \
It's also a potential security threat that you need to take seriously.

ASDF 3's portability layer UIOP provides a portable way to register hook functions \
that will clean up your environment before you dump an image. \
You can also register other functions, that will for instance \
extract from source control an accurate identifier for the current build, \
finalize some data structures and dictionaries based on the complete code, \
generate some code based on various data schemas, \
precompile the above as well as various CLOS methods, \
etc.

UIOP also allows you to register hook functions that will initialize your environment \
when you restart a new process from the Lisp image, \
including right now during the build for the current image.")
  'next
  ~
  @P{Many other uses}
  ~
  @P{A standard interface @it{matters}}
  (comment "\
Being able to do all that in a standard portable way means that \
you can write libraries that rely on these services being present, \
and on the libraries they themselves depend on being initialized. \
Users can use these libraries and not have to be aware \
of magic hooks they need to call to finalize or initialize each of them, \
either as a special step in their build script, \
or by using some arcane hook in their implementation at some point. \
There is no more trouble with libraries either initializing their dependencies \
and then finding that there are bugs when two libraries both try to initialize a same dependency; \
or not initializing their dependencies and then finding that there are subtle bugs \
because the user failed to initialize all the libraries in the correct order. \
Hooks are run in the correct order, depending on the order they are registered, \
which itself is compatible with the order of declared dependencies between libraries.

Remember, that as said Jeff Atwood:
\"Any time you're asking the user to make a choice they don't care about,
you have failed the user\""))

(tslide "Scripting Language?"
  (comment "\
So. I claim that with all these improvements,
CL is now an acceptable scripting language, which it wasn't before.
This begs the question: what is an acceptable scripting language?")
  'next
  @P{Low-overhead programming}
  @P{No boilerplate}
  @P{Write Once, Run Most-anywhere @it{unmodified}}
  @P{No setup needed}
  @P{Spawn or be spawned by other programs}
  @P{call or be called by functions in other languages}
  (comment "\
To me, the general criterion to a scripting language is low-overhead programming. \
This means little or no boilerplate \
between the programmer and a runnable program: \
one short line max as in #!/usr/bin/cl is OK; \
ten lines to include plenty of header files, class definitions, \
or a main(argc, argv) function prototype, is NOT OK. \
Having to write your own portability layer is NOT OK. \
cl-launch and ASDF 3 solved that for CL.

This also means little or no boilerplate between the user and running the program. \
Having to install the program and its dependencies is OK, \
though it should be mostly automated. \
Requiring a special setup and/or system administration skills is NOT OK. \
Having to configure variables specific to the task at hand is OK. \
The need to modify the script itself so it runs at all on your machine is NOT OK. \
cl-launch and ASDF 2 mainly solved the configuration issue, \
but many small improvements have been made since.

Finally, this means easy interoperation with other software on the system. \
Since the shell command line is the standard way for multiple programs to interoperate, \
it should be supported, both ways. \
cl-launch and ASDF 3 solve that. \
And since C libraries is the standard way to provide new services \
— respectively JVM libraries, .NET libraries, etc., depending on your platform — \
the scripting language should provide an easy to interface to that, both ways. \
CFFI provides that for CL.
"))

(tslide "What is it all about?"
  (comment "\
Why do we need scripting languages, or a build system, to begin with?
")
  'next
  @P{ASDF 3 does nothing that cannot be done without it}
  (comment "\
In the end, detractors will deride, ASDF 3 does nothing that cannot be done without it. \
Any program you write that uses ASDF 3 or cl-launch could be written without either. \
At the very worst, it would include relevant snippets of ASDF 3 or cl-launch to do the same thing, \
just lighter weight for not having to support cases irrelevant to the program at hand.")
  'next
  @P{Neither does any piece of software}
  (comment "\
But the same can be said of any and all software, beside the end applications: \
no computable function can ever extend the set of things that can theoretically be computed. \
No library can do anything that couldn't be done by duplicating relevant parts of its code \
in all client code. etc.")
  'next
  @P{Division of labor}
  (comment "\
The point of any and every library is division of labor: \
human creativity is a scarce resource, and \
by cooperating with each other, we can achieve more than we could separately, \
avoiding to each have to redundantly solve the same problems, \
when we could each be solving new problems that we can specialize on.")
  'next
  @P{@it{Enabling} the division of labor}
  (comment "\
The point of a build system is to enable the division of labor between other programmers. \
It achieves that by making it easy to divide software into many components that complement each other, \
that each may somehow fit into some programmer's brain, \
while reducing friction in combining these components into a complete program."))

(tslide "Beyond ASDF 3"
  (comment "\
So what is the next step for ASDF?
")
  'next
  @P{less overhead:}
  @P{ASDF 3.1: @tt{asdf:package-inferred-system}}
  ~
  @P{more modularity:}
  @P{ASDF 3.1: @tt{*readtable*} protection}
  ~
  @P{more access:}
  @P{Integration with other languages?}
  (comment "\
ASDF 3.1 has two innovations that further improve the language.

First, it sports an alternative lower-overhead way to declare dependencies, \
using the one-package-per-file style previously promoted by faslpath and quick-build. \
Since we have files and packages anyway, we might as well reuse package declarations, \
deduce dependencies from them, and match package names to file names to system names. \
This unsurprisingly makes component management more like Java or Python. \
The implementation about a hundred lines of code only, \
and for less than two hundred lines, you could have the equivalent of ASDF, \
except without all the bells and whistles, in one 1/50th to 1/100th of the size.

Second, ASDF 3.1 increases modularity by protecting the syntax of modules being compiled \
as determined by the *readtable* used while compiling, from the syntax of the toplevel, \
as determined by the *readtable* at the REPL. \
Common Lisp has too many special or global parameters, \
and by better isolating the parameters used during the build, \
we can make the build more modular. \
*Update*: these changes never made it to ASDF 3.1, and the new maintainer is in no hurry \
to merge the branch where it's happening.

Third, in ASDF 2 the dependency model was so specialized it could only be used to compile Lisp code; \
with ASDF 3, it is fully general and can be used to compile anything in any language, \
or manage any dependency-based build.
"))

(tslide "Lessons for other languages"
  @P{less overhead}
  ~
  ~
  @P{more modularity}
  ~
  ~
  @P{more access}
  ~
  (comment "\
If you're developing a language other than CL, \
consider these axes for improvement.

Can you reduce the overhead to writing useful programs?

Can you remove shared state?
Minimize configuration?
If any configuration is needed, can you let the user or programs override the defaults?

Can you access the rest of the system? Be accessed from it?
"))

(tslide "Extended version of my ELS 2014 article..."
  @P{The basic design of ASDF}
  @P{Why it rocks / sucks compared with C build tools}
  @P{Innovations in ASDF 1 2 2.26 3 3.1}
  @P{The Problem with Pathnames}
  @P{Lessons in Software Design including Pitfalls}
  @P{A great bug chase story}
  ~
  @para[#:align 'center]{@tt{http://github.com/fare/asdf3-2013}}
  (comment "\
The extended version of the article I published for ELS 2014
also contains many other themes, which explains why it's 26 pages long.

Many among you might enjoy reading all or part of it.
"))

(tslide "Share and Enjoy!"
  @P{@tt{http://common-lisp.net/project/asdf/}}
  @P{@tt{http://cliki.net/cl-launch}}
  @P{@tt{http://cliki.net/inferior-shell}}
  @P{@tt{http://www.quicklisp.org/beta/}}
  ~
  @P{@tt{http://github.com/fare/asdf3-2013}}
  ~
  @para[#:align 'center]{Any Questions?}
  (comment "\
All the software I've described is published as free software. \
You can find them at the following addresses."))
