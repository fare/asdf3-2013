#lang scribble/sigplan @nocopyright
@;-*- Scheme -*-

@(require scribble/base
          scriblib/autobib scriblib/footnote
          (only-in scribble/core style)
          "utils.rkt" "bibliography.scrbl")

@authorinfo["François-René Rideau" "Google" "tunes@google.com"]

@conferenceinfo["ELS 2014" "May 5--6, Paris, France."]
@copyrightyear{2014}

@title{ASDF 3, or Why Lisp is Now an Acceptable Scripting Language}

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
source code is organized in separately developed components
that can depend on other components,
and the build system transforms the transitive closure of these components
into a working program.

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

@(ASDF) is a build system for @(CL):
it helps developers divide software into a hierarchy of smaller @bydef{components},
and automatically generate a working program from all the source code.

Top components are called @bydef{systems} in an age-old Lisp tradition,
while the bottom ones are source files, typically written in @(CL).
Users may then @tt{operate} on these components with various build @bydef{operations},
most prominently compiling the source code (operation @tt{compile-op}) and
loading the result in the current Lisp image (operation @tt{load-op}).

Several related systems may be developed together
in a same source code @bydef{repository}.
Each system may depend on code from other systems,
that may come from the same repository, or from a different repository.
@(ASDF) itself has no notion of repositories,
but other tools do, on top of @(ASDF):
Quicklisp @~cite[Quicklisp] will provide together
systems from a same repository as a @bydef{release},
and provide hundreds of releases as a @bydef{distribution},
automatically downloading on demand
requested systems and all their transitive dependencies.

A system is itself organized in a hierarchy of @bydef{components}:
each component may be either an individual source @bydef{file} or some kind
(usually, a @(CL) source file),
or a @bydef{module} that recursively contains other components.
Modules may or may not directly fit the filesystem directory hierarchy.
Each component may explicitly depend on other components.
@(ASDF) ensures that before a component is compiled or loaded,
all its declared dependencies have themselves been
compiled and loaded in the current image and are up-to-date,
with all their own transitive dependencies.

@XXX{
To
@(ASDF) is a tool to describe how Lisp source code is organized in systems, and how to build a system in term of
actions that depend on previous actions.

Typical actions consist in compiling a Lisp source file (if not up to date) and loading the resulting
compilation output (if not both loaded and up to date).
And you must typically compile and load files that
define packages, macros, variables, before you may compile and load other files that use them.

It is roughly what @(CL) hackers use to build and load software where C hackers might use GNU Make to
build software and ld.so to load it.

@tt{asdf/defsystem} is the part that people usually refer to as @(ASDF),
with @tt{uiop} being only a supporting library,
that happens to be distributed at the same time, by necessity.


portability, robustness,
usability, extensibility, configurability, internal consistency, and the ability to create standalone
executables.
It was pre-released as 2.27 on February 1st 2013, released as 3.0.0 on May 15th 2013, with further
stable releases since.
}

@subsection{DEFSYSTEM before ASDF}

Ever since the late 1970s, Lisp implementations
have been each providing their variant of the original
Lisp Machine @tt{DEFSYSTEM} @~cite[CHINE-NUAL].
These build systems allowed users to define @bydef{systems},
units of software development made of many files,
themselves often grouped as @bydef{modules};
many @bydef{operations} were available to transform those systems and files,
mainly to compile the files and to load them,
but also to extract and print documentation,
to create an archive, etc.;
@tt{DEFSYSTEM} users could further declare dependency rules
between operations on those files, modules and systems,
such that files providing definitions
should be compiled and loaded before files using those definitions.

Back in 2001, the state of the art in free software @(CL) build systems
was still @tt{mk-defsystem} @~cite[MK-DEFSYSTEM], originally developed in 1990.
Like later variants of DEFSYSTEM on all Lisp systems,
it featured a declarative model to define a system in terms of
a hierarchical tree of @bydef{components} that could be modules or files,
each component being able to declare dependencies on other components.
The many subtle rules concerning the build operations
could be automatically deduced from these declarations,
instead of having to be manually specified by users.

However, @tt{mk-defsystem} suffered from several flaws,
in addition to a slightly annoying software license;
these flaws were probably justified at the time it was written,
several years before the @(CL) standard was adopted,
but were making it less than ideal in the world of universal personal computing.
First, installing a system required editing the system's definition files to configure pathnames,
and/or editing some machine configuration file to define "logical pathname translations"
allowing to map "logical pathnames" used by those files to actual physical pathnames;
back when there were a few data centers each with a full time administrator,
each configuring the system once for tens of users, this was a small issue;
but when hundreds of programmers were each installing software on their home computer,
this was less than ideal.
Second, extending the code was very hard:
Mark Kantrovitz had to restrict his code to functionality universally available in 1990,
and add a lot of boilerplate and magic to support many implementations;
adding new features needed in 2001 would have required modifying the carefully crafted file,
which would require a lot of work, yet eventually would probably still break
the support for now obsolete implementations that couldn't be tested anymore.

@subsection{ASDF 1: A Successful Experiment}

In 2001, Dan Barlow, a then prolific @(CL) hacker,
wanted a good @tt{defsystem} variant suitable for his needs;
instead of attempting to modify @tt{mk-defsystem}, at high cost for little expected benefit,
he wrote a new one, @(ASDF):
he could abandon the strictures of supporting long obsolete implementations,
and instead target modern @(CL) implementations.
In 2002, he published @(ASDF), made it part of SBCL,
and used it for his popular @(CL) software.
It was many times smaller than @tt{mk-defsystem}
(under a thousand line of code, instead of five thousand),
much more usable, actually extensible,
and easy to port to other modern @(CL) implementations,
with an uncontroversial MIT-style software license.
It was an immediate success.

@(ASDF) included many brilliant innovations.
To make configuration easy, @(ASDF) used
the @tt{*load-truename*} feature of modern Lisps:
thus, systems didn't need to be edited anymore,
since pathnames of all components could be deduced
from the pathname of the system definition file itself;
and because the @tt{truename} resolved Unix symlinks,
you could have symlinks to all your Lisp systems
in one or a few directories, and
@(ASDF) could find all of them.
Configuration was a matter of initializing @(ASDF)'s
@tt{*central-registry*} of directories in which to look for system definition files,
and maintaining the "link farms" in those directories;
much simpler and easier to automate than what was required with @tt{mk-defsystem}.

Also, following earlier suggestions by Kent Pitman @~cite[Pitman-Large-Systems],
Dan Barlow used object-oriented style to make his @tt{defsystem} extensible
without the need to modify the main source file.
Using the now standardized @(CLOS),
Dan Barlow defined his @tt{defsystem} in terms of @bydef{generic functions}
specialized on two arguments, @tt{operation} and @tt{component},
(using multiple dispatch, an essential OO feature unhappily not available
in lesser programming languages, i.e. sadly almost of them).
Extending @(ASDF) is a matter of simply by defining new subclasses
of @tt{operation} and/or @tt{component}
and a handful of new methods for the existing generic functions,
specialized on these new subclasses.
Dan Barlow then demonstrated such simple extension with his @tt{SB-GROVEL},
a system to automatically extract low-level details
of C function and data structure definitions,
so they may be used by SBCL's foreign function interface.

@(ASDF) was a great success at the time,
but after many years, it was also found to have its downsides:
Dan Barlow was experimenting with new concepts,
and his programming style was to write
the simplest code that would work in the common case;
his code had a lot of rough edges:
while @(ASDF) worked great on SBCL for the features he was using,
it often in ugly ways when using other implementations,
or exercising corner cases he had never tested;
the naive use of lists as a data structure
didn't scale to large systems with thousands of files;
the extensibility API was lacking in many ways
that required redefining @(ASDF) internals.

What more, every implementation had its own version,
with its own bug fixes and its own bugs,
and some large projects had their own patches
to redefine or work around significant chunks of ASDF.


@subsection{ASDF 2: Productizing ASDF}

@(ASDF) maintainership and development was taken over
by François-René Rideau in November 2009;
which l
A first major rewrite led to ASDF 2, released in May 2010.

ASDF 2 (2010) @~cite[Evolving-ASDF] was a major rewrite that brought
portability, configurability, robustness and upgradability,
and later (2011) working defsystem dependencies, Windows support, selective system forcing,
and (2012) encoding support, and hooks to wrap around compilation and check user invariants.
@note{
  Since we published that paper,
  a lot of the complexity we described was done away with:
  instead of trying to dynamically upgrade data,
  we "punt" and drop in-memory data if the schema has changed in incompatible ways,
  instead of trying hard to provide methods for @tt{update-instance-for-redefined-class}.
  Dropping the data in hard cases, combined with preemptive upgrading of ASDF,
  also allows us to simply use @tt{fmakunbound} everywhere,
  instead of having to @tt{unintern} some functions.
}



the ASDF 2 series culminated with ASDF 2.26,
at which time all the code had been rewritten,
and the core @tt{traverse} algorithm had been refactored
into


@subsection{ASDF 3: A Mature Build}

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

ASDF 3.1 (2014) builds on top of ASDF 3 to provide new features.

The @tt{asdf/package-system} extension supports
a one-file, one-package, one-system style of programming
with maintainability benefits.

UIOP's @tt{run-program} was generalized
to accept redirection of input and error-output as well as of output.

New macros include a debugged and extended @tt{with-temporary-file}
or the @tt{nest} macro.

Running an external program and getting its output,
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
were described Robert Goldman's and my ILC 2010 article @~cite[Evolving-ASDF]

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
