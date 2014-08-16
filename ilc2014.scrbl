#lang scribble/sigplan
@; @nocopyright @preprint
@;-*- Scheme -*-

@(require scribble/base
          scriblib/autobib scriblib/footnote
          scribble/decode scribble/core scribble/manual-struct scribble/decode-struct
          scribble/html-properties scribble/tag
          (only-in scribble/core style)
          "utils.rkt" "bibliography.scrbl")

@authorinfo["François-René Rideau" "Google" "tunes@google.com"]

@conferenceinfo["ILC 2014" "August 15--17, Montréal, Québec, Canada."]
@copyrightyear{2014}

@title{LAMBDA the Ultimate Scripting Language
              @(linebreak) @smaller{or Goodbye Shell Scripts, Hello Common Lisp!}}

@XXX{Keywords:
Programming Languages
Lisp
Common Lisp
Shell
Modularity
Deployment
Division of labor
}

@abstract{
   We will demonstrate how to use Common Lisp for all your shell scripting needs,
   by leveraging recent progress with asdf 3, cl-launch, inferior-shell, optima.ppcre, etc.
   Not only can Lisp handle all the text processing and subprocess management
   that you can do in scripting languages,
   its data structures, higher-order functions, conditions and object system
   provide for a much nicer experience than in the usual scripting languages
   — not to tell about the nice interactive debugger.
   No more @tt{#!/bin/sh} — use a real programming language: @tt{#!/usr/bin/cl}.
}

@section{Description}

Common Lisp being a universal programming language,
it was therefore always @emph{possible} to use it
for the tasks traditionally associated with @q{scripting languages};
yet it only recently acquired some of the key features
that make a programming language attractive for these "scripting" tasks:
(1) low overhead in defining and invoking programs,
(2) a modular deployment story for @q{Write Once, Run Most-anywhere},
and (3) easy access to system functionality including portable management of subprocesses.

We have previously explained @~cite[ASDF3-2014 Lisp-Acceptable-Scripting-Language]
why these recently implemented features were essential to enable
the kind of division of labor that underlies @q{scripting}.
Now we'll explain how to leverage this new ability
to use Common Lisp for writing @q{scripts};
we will show not only how it can compete not just with Shell scripts,
but also with writing scripts in Perl, Python, Ruby, and other @q{scripting} languages.
The features that we will demonstrate include:
@itemlist[
@item{
  One-line script invocation.
}
@item{
  One-line overhead to script definition.
}
@item{
  Easy extraction of data from subprocesses.
}
@item{
  Recursive data structures, not mere string manipulation.
}
@item{
  Native read-write invariance, not manual serialization.
}
@item{
  Higher-order functions, not just top-level functions.
}
@item{
  Pattern-matching, not just regular expressions.
}
@item{
  Interactive debugging, not just error (back)traces.
}
@item{
  Condition handling, not just global traps
}
@item{
  An advanced Object System, not just multiple inheritance.
}
@item{
  Native code compilation, not just script interpreters.
}]

@(generate-bib)
