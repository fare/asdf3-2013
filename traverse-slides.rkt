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

(tslide "Another System Definition Facility version 3.1"
  @para[#:align 'center]{A @tt{traverse} across the build}
  ~
  @para[#:align 'center]{A monster hunt story}
  ~
  ~
  @t{François-René Rideau <tunes@"@"google.com>}
  (comment "\
Hi, I'm François-René Rideau, and I'm here to tell you about ASDF 3, \
the de facto standard build system for Common Lisp.

My paper at the European Lisp Symposium 2014 is titled \
\"ASDF3: Why CL is now an acceptable Scripting Language\".

But today, I am going to tell you a story. \
It's a monster hunt story."))

(tslide "Based on a true story!"
  (comment "\
It's a classic monster hunt story that I'm going to tell you.")
  'next
  @para[#:align 'left]{Setting: a seemingly peaceful little village}
  (comment "\
The setting is a seemingly peaceful and orderly little village on the programming frontier. \
It is a familiar old place, not a big one, but a good, comfortable one; \
though it is not perfect, and monsters roam at night, it looks fundamentally healthy.

 (That village, is ASDF.)")
  'next
  @para[#:align 'left]{Protagonist: young and naïve new hunter}
  (comment "\
The protagonist is the hunter. In the beginning he is young and naïve — but capable.

  (Later, I will tell the story in the first person, but for now, third person will do.)

When he comes into town, our protagonist kicks out \
a few baddies that were victimizing the population. \
Soon enough he replaces the ailing sheriff.

  (That would be me becoming ASDF maintainer in 2009 when Gary King steps down, \
after fixing some pathname related bugs.)")
  'next
  @para[#:align 'left]{Order: New sheriff in town}
  (comment "\
Under the new sheriff, monsters big and small are hunted down. \
The inhabitants are not afraid anymore, though some of them remain grumpy.

  (That's me fixing bugs with the help of many other programmers.)

The protagonist builds fortifications, and finds he has to extend the city limits \
to make it easier to defend, adding new buildings along the way.

  (That would be improving the ASDF design to be more robust, and adding features.)

Often he has to hunt monsters that he himself let in, \
sometimes after they hurt citizens.

  (That's when I introduce bugs myself, and sometimes fail to fix them before release.)

The protagonist feels guilty about it and learns to be a better sheriff.

  (That's when I get to deeply respect the regression test suite.)

But by and large, his endeavor is a success. \
At long last, he thinks the place is now safe, \
and that he knows everything about the town and its now former monsters.

My, how wrong he is!

  (That's me at the time of ASDF 2.26)")
  'next
  @para[#:align 'left]{Nemesis: a monster that terrorizes innocent people}
  (comment "\
Now, a monster has been terrorizing innocent citizens for years. \
No one has seen the monster, but he leaves a characteristic mark on his victims.

  (That's the old bug whereby changes in dependencies are not propagated correctly across modules.)

The protagonist's best buddy has found a good way to protect homes against the monster, \
but it still roams in the streets at night.

  (That's when Robert Goldman fixes the bug and \
  gets dependency changes to trigger rebuild across modules, \
  but dependency changes still fail to trigger rebuild across systems)

Our sheriff, having finally vanquished all other monsters, \
  and having no other foe left in town, \
  sets off to catch this last monster.

And so, he has to enter hitherto unexplored caverns deep below the village, \
  a place abandoned long ago, where the creature lurks.

  (That would be the ASDF traverse algorithm.)

And of course that's when the story turns ugly.")
  'next
  @para[#:align 'left]{Fight: not easy as thought, but long and hard}
  (comment "\
Our protagonist thinks the monster will be an easy catch, \
what with his all experience and technology.

But it's actually a long, hard fight to the death. \
It's the toughest enemy ever.

  (And that's the story of writing ASDF 2.27, that becomes ASDF 3 after months of struggle.)")
  'next
  @para[#:align 'left]{Tension: climaxes! twists! magic help!}
  (comment "\
Along the way, \
many times, the protagonist thinks he has almost won, but not at all; \
many times, he thinks he is lost, but he keeps at it.

Quickly though, he realizes that the monster he was chasing \
is but a henchman of a bigger monster \
that has been ruling over the village all along. \
The apparent orderliness of the village was but a lie, \
all that he thought he knew was fake!

Happily, a mysterious wise man left cryptic instructions on how to defeat the monster.

  (And that's the great hunt story I'm going to tell you.)")
  'next
  @para[#:align 'left]{Conclusion: respect acquired and lessons learned}
  (comment "\
In the end, the sheriff vanquishes his foes and defeats the great monster for good, \
but not until he has learned to respect his enemy.

And his real prize is in the lessons he learned and the final illumination he reaches.

  (And I hope you too can enjoy this illumination.)"))

(tslide "The setting: ASDF"
  @item{build system}
  ~
  @item{components}
  @item{operations}
  ~
  @item{action graph}
  @item{plan}
  ~
  @item{in-memory}
  (comment "\
So let's examine the setting.

@(ASDF) is a build system for CL: \
it helps developers divide software into a hierarchy of @bydef{component}s \
and automatically generates a working program from all the source code.

Top components are called @bydef{system}s in an age-old Lisp tradition, 
while the bottom ones are source @bydef{file}s, typically written in CL.
In between, there may be a recursive hierarchy of @bydef{module}s@extended-only{
that may contain files or other modules and may or may not map to subdirectories}.

Users may then @(operate) on these components with various build @bydef{operation}s,
most prominently compiling the source code (operation @(compile-op)) and
loading the output into the current Lisp image (operation @(load-op)).

Several related systems may be developed together
in the same source code @bydef{project}.
Each system may depend on code from other systems,
either from the same project or from a different project.
@(ASDF) itself has no notion of projects,
but other tools on top of @(ASDF) do:
@(Quicklisp) @~cite[quicklisp] packages together
systems from a project into a @bydef{release},
and provides hundreds of releases as a @bydef{distribution},
automatically downloading on demand
required systems and all their transitive dependencies.

Further, each component may explicitly declare
a @bydef{dependency} on other components:
whenever a component relies on declarations or definitions
of packages, macros, variables, classes, functions, etc.,
present in another component, the programmer must
declare that the former component @(depends-on) the latter.

"))

(tslide "Defining components"
  (code
   (defsystem "fare-quasiquote" ...
     :depends-on ("fare-utils")
     :components
     ((:file "packages")
      (:file "quasiquote"
             :depends-on ("packages"))
      (:file "pp-quasiquote"
             :depends-on ("quasiquote")))))
  (comment "\
"))

(tslide "Modules"
  (code
   (defsystem "fare-utils" ... :components
    ((:file "package")
     (:module "base" :depends-on ("package")
      :components
      ((:file "utils")
       (:file "strings" :depends-on ("utils"))
       ...))
     (:module "filesystem" :depends-on ("base")
      :components ...) ...)))
  (comment "\
"))

(tslide "ASDF: summary"
  @item{@tt{component} = @tt{system} | @tt{module} | @tt{file} $ …}
  @item{a component @it{depends-on} other components}
  @item{@tt{operation} = @tt{load-op} | @tt{compile-op} | …}
  @item{operations @it{propagate} along the component DAG}
  @item{@tt{action} = @tt{operation} x @tt{component}}
  @item{an action @it{depends-on} other actions}
  @item{@tt{action} DAG ≠ @tt{component} DAG}
  @item{@tt{plan} = topologically sorted list of actions}
  (comment "\
"))

#|
@subsubsection{Example System Definition@extended-only{s}}

Below is how the @cl{fare-quasiquote} system is defined (with elisions)
in a file @tt{fare-quasiquote.asd}.
It contains three files, @tt{packages},
@tt{quasiquote} and @tt{pp-quasiquote}
(the @tt{.lisp} suffix is automatically added based on the component class;
see @appref["pathnames"]{Appendix C}).
The latter files each depend on the first file,
because this former file defines the CL packages@note{
  Packages are namespaces that contain symbols;
  they need to be created before the symbols they contain
  may even be read as valid syntax.@;
  @extended-only{
  Each CL process has a global flat two-level namespace:
  symbols, named by strings, live in packages, also named by strings;
  symbols are read in the current @cl{*package*},
  but the package may be overridden with colon-separated prefix, as in
  @cl{other-package:some-symbol}.
  However, this namespace isn't global across images:
  packages can import symbols from other packages,
  but a symbol keeps the name in all packages and knows its "home" package.
  Different CL processes running different code bases
  may thus have a different set of packages,
  where symbols have different home packages;
  printing symbols on one system and reading them on another may fail
  or may lead to subtle bugs.
}}:

Among the elided elements were metadata such as @cl{:license "MIT"},
and extra dependency information
@cl{:in-order-to ((test-op (test-op "fare-quasiquote-test")))},
that delegates testing the current system
to running tests on another system.
Notice how the system itself @(depends-on) another system, @cl{fare-utils},
a collection of utility functions and macros from another project,
whereas testing is specified to be done by @cl{fare-quasiquote-test},
a system defined in a different file, @cl{fare-quasiquote-test.asd},
within the same project.

@extended-only{
The @tt{fare-utils.asd} file, in its own project,
looks like this (with a lot of elisions):



This example illustrates the use of modules:
The first component is a file @tt{package.lisp},
that all other components depend on.
Then, there is a module @cl{base};
in absence of contrary declaration,
it corresponds to directory @tt{base/};
and it itself contains files
@tt{utils.lisp}, @tt{strings.lisp}, etc.
As you can see, dependencies name @bydef{sibling} components
under the same @bydef{parent} system or module,
that can themselves be files or modules.
}

@subsubsection{Action Graph}
@; TODO: add a graph!

Building software is modeled as a Directed Acyclic Graph (DAG) of @bydef{action}s,
where @emph{each action is a pair of an operation and a component.}
The DAG defines a partial order, whereby each action must be @bydef{perform}ed,
but only after all the actions it (transitively) @(depends-on) have already been performed.

For instance, in @cl{fare-quasiquote} above,
the @emph{loading} of (the compilation output of) @tt{quasiquote}
@(depends-on) the @emph{compiling} of @tt{quasiquote},
which itself depends-on
the @emph{loading} of (the compilation output of) @tt{package}, etc.

Importantly, though, this graph is distinct
from the preceding graph of components:
the graph of actions isn't a mere refinement of the graph of components
but a transformation of it that also incorporates
crucial information about the structure of operations.

@short-only{
@(ASDF) extracts from this DAG a @bydef{plan},
which by default is a topologically sorted list of actions,
that it then @bydef{perform}s in order,
in a design inspired by Pitman@~cite[Pitman-Large-Systems]
}
@extended-only{
Unlike its immediate predecessor @(mk-defsystem),
@(ASDF) makes a @bydef{plan} of all actions needed
to obtain an up-to-date version of the build output
before it @bydef{performs} these actions.
In @(ASDF) itself, this plan is a topologically sorted list of actions to be performed sequentially:
a total order that is a linear extension of the partial order of dependencies;
performing the actions in that order ensures that
the actions are always performed after the actions they depend on.

It's of course possible to reify the complete DAG of actions
rather than just one valid sequence.
Andreas Fuchs did in 2006, in a small but quite brilliant @(ASDF) extension
called @(POIU), the "Parallel Operator on Independent Units".
@(POIU) compiles files in parallel on Unix multiprocessors using @tt{fork},
while still loading them sequentially into a main image, minimizing latency.
We later rewrote @(POIU), making it
both more portable and simpler by co-developing it with @(ASDF).
Understanding the many clever tricks
by which Andreas Fuchs overcame the issues with the @(ASDF1) model
to compute such a complete DAG led to many aha moments,
instrumental when writing @(ASDF3) (see @appref["traverse"]{Appendix F}).
}

Users can extend @(ASDF) by defining
new subclasses of @(operation) and/or @(component)
and the methods that use them,
or by using global, per-system or per-component hooks.

@subsubsection{In-image}

@moneyquote{@(ASDF) is an @q{in-image} build system},
in the Lisp @(defsystem) tradition:
it compiles (if necessary) and loads software into the current CL image,
and can later update the current image by recompiling and reloading the components that have changed.
For better or worse, this notably differs from common practice in most other languages,
where the build system is a completely different piece of software running in a separate process.@note{
  Of course, a build system could compile CL code in separate processes,
  for the sake determinism and parallelism:
  our XCVB did @~cite[XCVB-2009]; so does the Google build system.
  @;@extended-only{As for the wide variety of Lisp dialects beside CL,
  @;they have as many different build systems, often integrated with a module system.}
}
On the one hand, it minimizes overhead to writing build system extensions.
On the other hand, it puts great pressure on @(ASDF) to remain minimal.

Qualitatively, @(ASDF) must be delivered as a single source file
and cannot use any external library,
since it itself defines the code that may load other files and libraries.
Quantitatively, @(ASDF) must minimize its memory footprint,
since it's present in all programs that are built,
and any resource spent is paid by each program.@extended-only{@note{
  This arguably mattered more in 2002 when @(ASDF) was first released
  and was about a thousand lines long:
  By 2014, it has grown over ten times in size,
  but memory sizes have increased even faster.
}}

For all these reasons, @(ASDF) follows the minimalist principle that
@moneyquote{anything that can be provided as an extension
            should be provided as an extension and left out of the core}.
Thus it cannot afford to support a persistence cache
indexed by the cryptographic digest of build expressions,
or a distributed network of workers, etc.
However, these could conceivably be implemented as @(ASDF) extensions.

|#


(tslide "Also in the extended article..."
  @para[#:align 'left]{CL is Now an Acceptable Scripting Language}
  @para[#:align 'left]{Why it rocks / sucks compared with C build tools}
  @para[#:align 'left]{Innovations in ASDF 1 2 2.26 3 3.1}
  @para[#:align 'left]{The Problem with Pathnames}
  @para[#:align 'left]{Lessons in Software Design including Pitfalls}
  ~
  @para[#:align 'left]{@tt{http://github.com/fare/asdf3-2013}}
  (comment "\
"))

(tslide "Use it!"
  @para[#:align 'left]{@tt{http://common-lisp.net/project/asdf/}}
  ~
  @para[#:align 'left]{@tt{http://github.com/fare/asdf3-2013}}
  ~
  @para[#:align 'center]{Any Questions?}
  (comment "\
"))
