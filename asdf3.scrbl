#lang scribble/sigplan @nocopyright
@;-*- Scheme -*-

@(require scribble/base
          #|scribble/manual
          scriblib/autobib scriblib/footnote
          (only-in scribble/core style)|#
          "utils.rkt"
          #|"bibliography.scrbl"|#)

@;TODO: Parameterize whether to load the sections as standalone or sections,
@;TODO: With a different introduction message depending.
@(multiple-sections #t)

@title{Making sense of ASDF}

@abstract{
This essay describes the key tensions that underlie the design of @(ASDF),
the @(de_facto) standard Common Lisp build system.
As often, the nature of these tensions is revealed by
the cracks in the resulting structure and the dynamic of its evolution.
The Lisp approach is contrasted with that of Unix,
with @(ASDF)'s design being notably compared to that of @(Make).
Some historical elements are given that may be skipped by those
less interested in Common Lisp itself.
}

@section{Introduction}

I'm pleased to announce that as of February 2013,
Common Lisp is now on par with 1977 technology for building software!
Indeed since release 2.27,
@hyperlink["http://common-lisp.net/project/asdf/" (ASDF)]
can now properly process @emph{and propagate} timestamps
to rebuild dependencies as required;
it thereby achieves what
@hyperlink["http://en.wikipedia.org/wiki/Make_(software)" (Make)]
has been doing for 36 years.

To exercise the bug, define a system with two files,
whereby the second file depends on the first file;
compile your system; modify the first file; recompile.
See whether the second file too gets recompiled as well as the first one.
@(ASDF3) correctly recompiles the second file. @(ASDF2) fails.
But more importantly, all its predecessors fail:
Not just Dan Barlow's original @(ASDF),
the algorithm of which @(ASDF2) reproduced albeit with refactored internals,
but also its immediate predecessor @(mk-defsystem)
(the source code of which I inspected),
all the way to the original @(DEFSYSTEM).

Now, how come such a bug could survive so long
without anyone noticing it, much less fixing it?
What does that tell us about Lisp and Lisp programmers
as compared to other programming languages and their practitioners?
Part of the answer of course is that
due to the way they write software,
most Lisp programmers don't see the bug,
or see it as a transient failure among many others,
that disappears all by itself as they keep programming.
Still it bears exploring the converging reasons why a bug
that would have been damning in the context @(Make)
ended up being a minor inconvenience in the context of @(DEFSYSTEM).

We will use this and other past bugs or lack of features in @(ASDF)
and the way that Lisp programmers have coped with them
as a guide to what were or weren't the driving forces behind its design.
Along the way, we will contrast with the approach that emerged
in the larger community of Unix C hackers.

We will therefore examine these reasons in detail,
then make some digressions about the history of @(ASDF),
and conclude with a discussion of the strong points and limitations of @(ASDF),
with an eye on a future replacement.
@XXX{}

@(include-section "live-programming.scrbl")

@(include-section "old-bug.scrbl")

@(include-section "history.scrbl")

@(include-section "tutorial.scrbl")

