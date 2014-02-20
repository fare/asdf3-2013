#lang scribble/sigplan @nocopyright
@;-*- Scheme -*-

@(require scribble/base
          scriblib/autobib scriblib/footnote
          (only-in scribble/core style)
          "utils.rkt" "bibliography.scrbl")

@;TODO: Parameterize whether to load the sections as standalone or sections,
@;TODO: With a different introduction message depending.
@(multiple-sections #t)

@authorinfo["François-René Rideau" "Google" "tunes@google.com"]

@conferenceinfo["ELS 2014" "May 5--6, Paris, France."]
@copyrightyear{2014}
@;@copyrightdata{123-4-5678-9012-3/45/67}

@title{ASDF 3: Why Lisp is an Acceptable Scripting Language}

@abstract{
@(ASDF), the @(de_facto) standard build system for @(CL),
has been vastly improved between 2010 and 2014.
These and other improvements finally bring @(CL) up to par
with "scripting languages"
in terms of ease of writing and deploying portable code
that may access and "glue" together
functionality from the underlying system or external programs —
except this time in a language with reasonable semantics,
efficient implementations and extensible syntax.
We describe the most salient improvements in @(ASDF),
and how they enable previously difficult and portably impossible
uses of the programming language.
We discuss past and future challenges
in improving this key piece of software infrastructure,
and what approaches did or didn't work
in bringing change to the @(CL) community.
}

@section{Introduction}

@(ASDF) has been the @(de_facto) standard build system
for portable @(CL) software since shortly after its release
by Dan Barlow in 2002 @~cite[ASDF-Manual].@note{
@(CL) is a language defined in the ANSI standard X3.226-1994.
  It is a multi-paradigm dynamically-typed high-level language:
  though it is still known for its decent support for functional programming,
  its support for Object-Oriented Programming is what remains unsurpassed in many ways,
  and sadly few languages even attempt to match its syntactic extensibility
  or support for interactive development.
  It was explicitly designed to allow for high-performance implementations;
  some of them, depending on the application,
  may rival with compiled "C" programs in terms of speed,
  or at least be in the same ball park for performance,
  usually far ahead of "scripting" languages and their implementations.

  Over a dozen actively maintained implementations
  purport to conform to the ANSI @(CL) standard, plus a few unmaintained ones.
  No single implementation is the best, shiniest, smallest and fastest,
  cheapest, and ported to most platforms.
  While it is possible to write useful programs
  using only the standardized parts of the language,
  serious implementations each incompatibly provide
  plenty of additional practical features not included in the standard,
  such as threads, unicode support, or a "foreign function interface" to libraries written in C.
  However software can be made to work on all the interesting combinations
  of implementation, operating system and hardware architecture
  by following reasonable portability constraints
  and using existing libraries to abstract over discrepancies between implementations.
}
As a build system, it enables division of labor in software development:
software is organized in separately developed @bydef{systems},
on which users may perform various build @bydef{operations},
most prominently compiling the source code and
loading the result in the current Lisp image.

With the recent release of @(ASDF) 3 in May 2013,
it has become possible to write @emph{portable} software in @(CL),
that can do anything you can in any so-called "scripting" language:
notably invoke other programs and capture their output,
or be invoked by other programs and process command-line arguments;
also being able to deliver the result as either small portably executable scripts
or large, non-portable but self-contained binaries with no startup delay.
Previously, key parts of a program had to be programmed or configured to match
one's specific @(CL) implementation and specific software installation paths.
Now, all of one's usual Unix scripting needs can be entirely fulfilled by @(CL),
while benefitting from its efficient implementations, hundreds of software libraries, etc.
The utility @tt{cl-launch} can help get Lisp programs be portably invoked.

In a first part, we will describe the major changes in ASDF
since the original version 1 series.
To make things easier to @(CL) programmers who have used the tool at some point,
we break down improvements by major milestones.
For each improvement, we provide a rationale for the change,
and a simple example of how it enables better programming practices
or more powerful programs that were not possible or not portable before.

In a second part, we will discuss the challenges of making acceptable changes
to a piece of community software that is not only used by hundreds of developers,
but also extended in various ways by tens of them.
What it means to be backward compatible is not
keeping all the old bugs that people have come to rely on;
it is providing a smooth upgrade path to better software
despite weak synchronization.

In our conclusion, we explore how the improvements we had to make,
and the way we could make them,
illustrate what was wrong, and was right, about @(CL),
so that growers of other programming languages may learn from the experience.

@section{ASDF, Before and Now}

@subsection{ASDF: Basic Concepts}

@(ASDF) is a build system:
it helps developers divide software into a hierarchy of smaller @bydef{components},
and automatically assemble these units into a complete working programs.
In @(ASDF), toplevel components are called @bydef{systems}
while the most basic components considered are source files.

Several related systems may be developed together
in a same source code @bydef{repository}.
Each system may depend on code from other systems,
that may come from the same repository, or from a different repository.
@(ASDF) itself has no notion of repositories,
but other tools do, on top of @(ASDF):
Quicklisp will provide together systems from a same repository as a @bydef{release},
and provide hundreds of releases as a @bydef{distribution}.

allows  enables division of labor in software development:
software is organized in separately developed @bydef{systems},
on which users may perform various build @bydef{operations},
most notably compiling the source code and
loading the result in the current Lisp image.


A given source code @bydef{repository} may define multiple related systems,
that each may depend on other systems from this or other repositories.
A system is itself organized in a hierarchy of smaller @bydef{components},
down to individual @bydef{files}, themselves recursively grouped in @bydef{modules}
that may or may not directly fit the filesystem directory hierarchy.
@(ASDF) ensures that before a component is compiled or loaded,
all its declared dependencies have themselves been compiled and are up to date,
and have been loaded in the current image, and all its recursive contents.
These dependencies may include other systems in same or different repositories,
or other components in the same module.


@(ASDF) is a tool to describe how Lisp source code is organized in systems, and how to build a system in term of
actions that depend on previous actions.

Typical actions consist in compiling a Lisp source file (if not up to date) and loading the resulting
compilation output (if not both loaded and up to date).
And you must typically compile and load files that
define packages, macros, variables, before you may compile and load other files that use them.

It is roughly what @(CL) hackers use to build and load software where C hackers might use GNU Make to
build software and ld.so to load it.

asdf/defsystem is the part that people usually refer to as ASDF, with uiop being only a supporting library,
that happens to be distributed at the same time, by necessity.


portability, robustness,
usability, extensibility, configurability, internal consistency, and the ability to create standalone
executables.
It was pre-released as 2.27 on February 1st 2013, released as 3.0.0 on May 15th 2013, with further
stable releases since.


@subsection{ASDF 1: A Successful Experiment}

@subsection{ASDF 2: Productizing ASDF}

ASDF 2 (2010) @~cite[Evolving-ASDF] was a major rewrite that brought
portability, configurability, robustness and upgradability,
and later (2011) working defsystem dependencies, Windows support, selective system forcing,
and (2012) encoding support, and hooks to wrap around compilation and check user invariants.

@subsection{ASDF 3: }

ASDF 3 (2013) was completely revamped, twice over, to correctly compute timestamps
across an arbitrary action graph;
users may specify how operations do or don't propagate
along the component hierarchy;
there are several ways to deliver software in a single-file "bundle",
including a single FASL for a system and/or its dependencies, or
(on supported implementations) a standalone executable programs;
better supported are conditional compilation, versioning, and more.
ASDF 3 also brought a user-visible portability layer UIOP, abstracting over
pathnames, filesystem access, run-program, Lisp image lifecycle, and
many other everyday concerns.

@subsection{ASDF 3.1: Lifting Lisp}

ASDF 3.1 (2014) includes package-system, which support
a one-file, one-package, one-system style of programming
with maintainability benefits;
it also sports important utilities such as
a run-program that has input and error-output as well as output redirection,
or with-temporary-file or the nest macro.

running an external program and getting its output,
or even manipulating pathnames and accessing the filesystem,
were notably hard;
even more complexity was involved to
@emph{robustly} deal with countless corner cases.
ASDF 3.1 provides solutions to these issues, and many more.

@section{Evolving Code in an Immutable Community}

In a second part, we will discuss the challenges of making acceptable change
to a piece of community software that is not only used by hundreds of developers,
but also extended in various ways by tens of them:
backward compatibility is much harder in presence of extensibility.
The following features succeeded,
because they provided sufficient backward compatibility,
for interesting values of "sufficient":
source-registry configuration, output translations, output renaming,
change in default encoding, asdf-bundle,
replacement for if-component-dep-fails,
if-bind for aif,
compile-file* for *compile-file-function*,
refactoring into one-file-one-package style.
The following features failed,
because they either weren't compatible enough, or didn't bring sufficient value:
preference files, generalized around method combinations, asdf-binary-locations,
deferred-warnings, unconstrained versions,
:current-directory, *system-cache* and get-uid, asdf-utilities, asdf-utils,
d as nickname to asdf-driver, DBG macro.


@section{Conclusion: Lessons for Language Growers}







@(ASDF) maintainership and development was taken over
by François-René Rideau in November 2009.
A first major rewrite led to ASDF 2, released in May 2010;
the ASDF 2 series culminated with ASDF 2.26,
at which time all the code had been rewritten,
and the core @tt{traverse} algorithm had been refactored
into



and ASDF 3 released in May 2013
(beta releases starting with 2.27 in February 2013).
Robert Goldman assumed maintainership in July 2013,
a few months after the release of ASDF 3,
and but François-René Rideau remained main developer until
and February 2014,
who also was

with the release of @(ASDF) 2 in May 2010,
of @(ASDF) 3 beta (aka 2.27) in February 2013,
and @(ASDF) 3.1 in February 2014, with Robert Goldman taking




@section{New Features}

@subsection{From ASDF 2}

The most important features from ASDF 2
were described Robert Goldman's and my ILC 2010 article:
@hyperlink["http://common-lisp.net/project/asdf/ilc2010draft.pdf"]
{Evolving ASDF: More Cooperation, Less Coordination}.

Upgradability crucially decoupled what ASDF users could rely on
from implementations provided, enabling a virtuous circle of universal upgrades,
where previously where everyone was waiting for others to upgrade, in a deadlock.

Portability decoupled which implementation and operating system
were used to develop a system from which it could be compiled with,
where previously any non-trivial use of pathnames, filesystem access
or subprocess invocation was a portability minefield.

Configurability decoupled use and installation of libraries,
whereby multiple parties could each modularly contribute some software systems
and provide configuration for it without being required
to know configuration of other systems,
where previously, who installed software couldn't notify users,
and users had to know and specify configuration of all software installed.

Robustness decoupled the need for testing

@(generate-bib)
