#lang scribble/sigplan @nocopyright
@;-*- Scheme -*-

@(require scribble/base
          scriblib/autobib scriblib/footnote
          scribble/decode scribble/core scribble/manual-struct scribble/decode-struct
          scribble/html-properties scribble/tag
          (only-in scribble/core style)
          "utils.rkt" "bibliography.scrbl")


@authorinfo["François-René Rideau" "Google" "tunes@google.com"]

@conferenceinfo["ELS 2014" "May 5--6, Paris, France."]
@copyrightyear{2014}

@title{@(ASDF3), or Why Lisp is Now an Acceptable Scripting Language}

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
  may rival with compiled C programs in terms of speed,
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

With the recent release of @(ASDF3) in May 2013,
it has become possible to write @emph{portable} software in @(CL),
that can do anything you can in any so-called "scripting" language:
notably invoke other programs and capture their output,
or be invoked by other programs and process command-line arguments;
also being able to deliver programs as either small portably executable scripts
or large non-portable but self-contained binaries that have no startup delay.
Previously, key parts of a program had to be configured to match
one's specific @(CL) implementation and specific software installation paths.
Now, all of one's usual Unix scripting needs can be entirely fulfilled by @(CL),
while benefitting from its efficient implementations, hundreds of software libraries, etc.
The utility @tt{cl-launch} can help get Lisp programs be portably invoked.

In a first part, we will give some context about what @(ASDF) is,
how it compares to what is done in the C world,
and what is the historical context in which it was designed.

In a second part, we will describe the major changes in @(ASDF)
since the original version 1 series.
To make things easier to @(CL) programmers who have used the tool at some point,
we break down improvements by major milestones.
For each improvement, we provide a rationale for the change,
and a simple example of how it enables better programming practices
or more powerful programs that were not possible or not portable before.

In a third part, we will discuss the challenges of making acceptable changes
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

@section{What @(ASDF) is}

@subsection{@(ASDF): Basic Concepts}

@(ASDF) is a build system for @(CL):
it helps developers divide software into a hierarchy of @bydef{components},
and automatically generate a working program from all the source code.

Top components are called @bydef{systems} in an age-old Lisp tradition,
while the bottom ones are source files, typically written in @(CL).
Users may then @tt{operate} on these components with various build @bydef{operations},
most prominently compiling the source code (operation @tt{compile-op}) and
loading the result in the current Lisp image (operation @tt{load-op}).

@XXX{here, include example of defsystem in foo.asd}

Several related systems may be developed together
in a same source code @bydef{repository}.
Each system may depend on code from other systems,
that may come from the same repository, or from a different repository.
@(ASDF) itself has no notion of repositories,
but other tools, on top of @(ASDF), do:
Quicklisp @~cite[Quicklisp] will provide together
systems from a same repository as a @bydef{release},
and provide hundreds of releases as a @bydef{distribution},
and automatically download on demand
required systems and all their transitive dependencies.

A system is itself organized in a hierarchy of components,
where each component may be either an individual @bydef{file}
(usually, a @(CL) source file),
or a @bydef{module} that may recursively contain other components.
Modules may or may not directly fit the filesystem directory hierarchy.

Each component may explicitly depend on other components.
Dependencies are typically files that contain definitions for
packages, macros, variables, classes, generic functions,
and any functions used at compile-time, notably during macro expansion.

Building software is modeled as a direct acyclic graph of @bydef{actions},
each action being a pair of an operation and a component.
Unlike its immediate predecessor,
@(ASDF) makes a @bydef{plan} of all actions needed
to obtain an up-to-date version of the build output,
before it @bydef{performs} these actions.
In @(ASDF) itself, this plan is a list of actions to be performed sequentially.
But it is possible to write an extension that makes a complete graph
of actions to be performed in parallel.
@note{
  Andreas Fuchs in 2006, wrote a very small but quite brilliant @(ASDF) extension
  called @(POIU), the Parallel Operator on Independent Units,
  that compiles files in parallel on Unix multiprocessors using @tt{fork},
  while still loading them sequentially, minimizing latency.

  François-René Rideau later rewrote @(POIU), making it
  both more portable and simpler by, co-developing it with @(ASDF).
  Understanding the sometimes bizarre and useless-looking
  but actually extremely clever and extremely necessary tricks
  by which Andreas Fuchs overcame the limitations and conceptual bugs of @(ASDF)
  led to many aha moments, instrumental when fixing @(ASDF2) into @(ASDF3).
}
In making this plan,
@(ASDF) ensures that before the action that compiles or loads component is performed,
all the actions that compile and load its declared dependencies have themselves been performed,
which also includes all their own transitive dependencies.

@subsection{Comparison to C programming practice}

Most programmers are familiar with C, but not with @(CL).
It is therefore worth constrasting @(ASDF) to the tools used by C programmers
to provide similar services.

To build build and load software, C programmers typically use
@tt{make} to build the software, and @tt{ld.so} to load it;
additionally, they use a tool like @tt{autoconf}
to locate available libraries and identify their features.
In many ways is these C solutions are vastly more engineered than @(ASDF),
that indeed is vastly more primitive.
But in other important ways, @(ASDF) demonstrates how
a lot of the complexity present in these C systems
is extrinsic make work that is drastically simplified away
by Lisp's vastly better architecture.

@itemlist[
  @item{
    Lisp makes the full power of runtime available at compile-time,
    so it's easy write domain-specific languages as extensions
    where only the new functionality needs be defined
    (such as support for defining systems)
    while the rest of the language remains available for the user.
    In C, instead, every utility needs to onerously grow
    an entire domain-specific language from scratch,
    which, since the domain expert is seldom also language expert
    with resources to do that right,
    means plenty of mutually incompatible misdesigned
    power-starved misimplemented languages that have to be combined
    through expensive and restricted interprocess communication.
  }
  @item{
    Lisp provides full introspection at runtime and compile-time alike,
    as well as a protocol to declare @bydef{features}
    and conditionally read code and data based on them,
    so you don't need dark magic at compile-time to detect features available.
    In C, instead, people resort to horribly unmaintainable configuration scripts
    in a hodge podge of shell script, m4 macros, C preprocessing and C code,
    plus often bits of python, perl or sed, etc.
  }
  @item{
    @(ASDF) possesses a standard and standardly extensible way to configure
    where to find the libraries your code depends on, further improved in @(ASDF2).
    In C, instead, then are tens of incompatible ways to do it,
    between @tt{libtool}, @tt{autoconf}, @tt{gconf}, @tt{kde-config},
    various manual @tt{./configure} scripts, and countless other protocols,
    so that each new piece of software requires
    to learn a new ad hoc configuration method,
    making it an expensive endeavour to use and/or distribute libraries.
  }
  @item{
    @(ASDF) uses the very same mechanism to configure both runtime and compile-time,
    so there is only one configuration mechanism to learn and to use,
    and no risk of discrepancy between the two.
    In C, completely different and incompatible mechanisms are used
    at runtime (@tt{ld.so}) and compile-time (an unspecified mess),
    which further makes it hard to match
    source code, compilation headers, static and dynamic libraries,
    requiring huge complex "software distribution" infrastructure
    (that admittedly also manage versioning, downloading and precompilation),
    and causing very hard to understand bugs when subtle discrepancies creep in.
  }
]

@subsection{DEFSYSTEM before @(ASDF)}

Ever since the late 1970s, Lisp implementations
have each been providing their variant of the original
Lisp Machine @tt{DEFSYSTEM} @~cite[CHINE-NUAL].
These build systems allowed users to define @bydef{systems},
units of software development made of many @bydef{files},
themselves often grouped into @bydef{modules};
many @bydef{operations} were available to transform those systems and files,
mainly to compile the files and to load them,
but also to extract and print documentation,
to create an archive, issue hot patches, etc.;
@tt{DEFSYSTEM} users could further declare dependency rules
between operations on those files, modules and systems,
such that files providing definitions
should be compiled and loaded before files using those definitions.

Since 1990, the state of the art in free software @(CL) build systems
was @tt{mk-defsystem} @~cite[MK-DEFSYSTEM].@note{
  The variants of @tt{DEFSYSTEM} available
  on each of the major proprietary @(CL) implementations
  (Allegro, LispWorks, and formerly, Genera),
  seem to have been much better than @tt{mk-defsystem}.
  But they were not portable, not mutually compatible, and not free software,
  and therefore @tt{mk-defsystem} because @emph{de facto} standard for free software.
}
Like late 1980s variants of DEFSYSTEM on all Lisp systems,
it featured a declarative model to define a system in terms of
a hierarchical tree of @bydef{components},
with each component being able to declare dependencies on other components.
The many subtle rules constraining build operations
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
but when hundreds of amateur programmers
were each installing software on their home computer,
this was less than ideal.
Second, extending the code was very hard:
Mark Kantrovitz had to restrict his code to functionality universally available in 1990,
and to add a lot of boilerplate and magic to support many implementations;
adding new features needed in 2001 would have required modifying the carefully crafted file,
which would require a lot of work, yet eventually would probably still break
the support for now obsolete implementations that couldn't be tested anymore.

@section{@(ASDF) Innovations}

@subsection{@(ASDF1): A Successful Experiment}

In 2001, Dan Barlow, a then prolific @(CL) hacker,
wanted a good @tt{defsystem} variant suitable for his needs;
instead of attempting to modify @tt{mk-defsystem}, at high cost for little expected benefit,
he wrote a new one, @(ASDF):@note{
  In a combined reverence to tradition and joke,
  @(ASDF) stands for "Another System Definition Facility",
  as well as for consecutive letters on a QWERTY keyboard.
}
thus he could abandon the strictures of supporting long obsolete implementations,
and instead target modern @(CL) implementations.
In 2002, he published @(ASDF), made it part of SBCL,
and used it for his popular @(CL) software.
It was many times smaller than @tt{mk-defsystem}
(under a thousand line of code, instead of five thousand),
much more usable, actually extensible,
and easy to port to other modern @(CL) implementations,
what more with an uncontroversial MIT-style software license.
It was an immediate success.

@(ASDF) featured many brilliant innovations in its own right.

Perhaps most importantly as far as usability goes,
@(ASDF) cleverly used the @tt{*load-truename*} feature of modern Lisps,
whereby programs (in this case, the @tt{defsystem} form)
can identify from which file they are loaded.
Thus, system definition files didn't need to be edited anymore,
as was previously required with @tt{mk-defsystem},
since pathnames of all components could now be deduced
from the pathname of the system definition file itself;
furthermore, because the @tt{truename} resolved Unix symlinks,
you could have symlinks to all your Lisp systems
in one or a handful directories that @(ASDF) knew about,
and it could trivially find all of them.
Configuration was thus a matter of configuring @(ASDF)'s
@tt{*central-registry*} with a list directories
in which to look for system definition files,
and maintaining "link farms" in those directories
— and both aspects could be automated.
(See below for how @(ASDF2) improved on that.)

Also, following earlier suggestions by Kent Pitman @~cite[Pitman-Large-Systems],
Dan Barlow used object-oriented style to make his @tt{defsystem} extensible
without the need to modify the main source file.
Using the now standardized @(CLOS),
Dan Barlow defined his @tt{defsystem} in terms of @bydef{generic functions}
specialized on two arguments, @tt{operation} and @tt{component},
(using multiple dispatch, an essential OO feature unhappily not available
in lesser programming languages, i.e. sadly almost of them —
they make do by using the "visitor pattern").
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

What more, there was a vicious circle preventing
@(ASDF) bugs from being fixed or features from being added @~cite[Software-Irresponsibility]:
Every implementation had its own version,
with its own bug fixes and its own bugs;
so developers of portable systems could not assume anything
but the lowest common denominator, which was very buggy.
On the other hand, because users were not using new features,
but that their kluges and workarounds tended to institutionalize old bugs,
there was no incentive for implementations
and strong incentive to not change anything until everyone else already did.
Finally it was both impossible
to build portable libraries into a Lisp image without having @(ASDF) loaded,
and to upgrade @(ASDF) once it was loaded.
Thus, one couldn't reliably assume bugs to ever be fixed,
unless one maintained one's own copy of @(ASDF),
and closely managed the entire build chain,
starting from a naked Lisp, then
getting this copy of @(ASDF) compiled and loaded,
before any system could be loaded.
Software distributions such as Debian couldn't really help.
Finally, since there was little demand for bug fixes,
supply followed by not being active fixing bugs.
And so @(ASDF) development stagnated for many years.

@subsection{@(ASDF2): Productizing @(ASDF)}

In November 2009, François-René Rideau
took over @(ASDF) maintainership and development.
A first set of major changes led to @(ASDF2), released in May 2010.
The versions released by Dan Barlow and the maintainers who succeeded him,
and numbered 1.x are thereafter referred to at @(ASDF1).
These changes are explained in more detail in
our ILC 2010 article @~cite[Evolving-ASDF].

@subsubsection{Upgradability}

The first bug fix was to break the vicious circle
preventing bug fixes from being relevant.
The solution found was to enable hot upgrade of @(ASDF),
so that users could always load a fixed version
on top of whatever the implementation or distribution did or didn't provide.
@note{
  Since we published that paper,
  a lot of the described complexity was done away with:
  instead of trying to dynamically upgrade data,
  we "punt" and drop in-memory data if the schema has changed in incompatible ways,
  instead of trying hard to provide methods for @tt{update-instance-for-redefined-class}.
  Dropping the data in hard cases,
  also allows us to simply use @tt{fmakunbound} everywhere,
  instead of having to @tt{unintern} some functions.
  Finally, to avoid either having data to drop
  or being caught upgrading @(ASDF) in midflight,
  @(ASDF3) will preemptive upgrading itself at the beginning of the build
  (if an upgrade is available as configured).
  The only potential impact of this reduction in upgrade capability
  would be users who upgrade code in a long-running live server;
  but considering how daunting that task is, properly upgrading @(ASDF)
  despite reduced support might be the least of their problems.
}
Soon enough, users felt confident relying on bug fixes and new features,
and all implementations started providing @(ASDF2).

These days, you can @tt{(require "asdf")} on pretty much any @(CL) implementation,
and start building systems using @(ASDF)
(most implementation already provide @(ASDF3);
one still lags with @(ASDF2), but will hopefully be updated this year).

Upgradability crucially decoupled what @(ASDF) users could rely on
from implementations provided, enabling a virtuous circle of universal upgrades,
where previously where everyone was waiting for others to upgrade, in a deadlock.
@bold{Allowing for divergence creates an incentive towards convergence}.

@subsubsection{Portability}

A lot of work was spent on portability.
@(ASDF1) officially supported 4 implementations:
@tt{allegro}, @tt{ccl}, @tt{clisp}, @tt{sbcl};
@(ASDF) variants may or may not have worked on a handful other implementations;
system definition semantics often varied subtly between implementations,
notably regarding pathnames.
@(ASDF) 2.000 supported 9 implementations, adding:
@tt{abcl}, @tt{cmucl}, @tt{ecl}, @tt{lispworks}, @tt{gcl};
system definition semantics was uniform across platforms.
@(ASDF) 2.26 supported 15, adding:
@tt{cormanlisp}, @tt{genera}, @tt{mkcl}, @tt{rmcl}, @tt{scl}, @tt{xcl}.
Since then, new implementations are released with @(ASDF) support:
@tt{mocl}, and hopefully soon @tt{clasp}.

@(ASDF) as originally designed would only reliably work on Unix variants
(Linux, BSD, etc., maybe cygwin, and now also MacOS X, Android, iOS).
It can now deal with very different operating system families:
most importantly Windows, but also the ancient MacOS 9 and Genera.

Of course, this required writing abstraction layers
over functionality that was never standardized,
and working around a few bugs in each system.
But the greatest source of portability woe was in handling @emph{pathnames}:
the standard specification of their behavior is so lacking,
and the implementations so differ in their often questionable behavior,
that instead of the problem being an abundance of corner cases,
the problem was a dirth of common case.
So great is the disaster of @(CL) pathnames,
that they deserve their own appendix to this article.

Lisp programmers can now "write once, run anywhere",
as far as defining systems go;
but they still have to otherwise avoid non-standardized behavior
and implementation-specific extensions (unless hidden behind a portability layer)
if they want their programs to be portable
— @(ASDF) cannot solve these issues intrinsic to @(CL).

Portability decoupled which implementation and operating system
were used to develop a system from which it could be compiled with,
where previously any non-trivial use of pathnames, filesystem access
or subprocess invocation was a portability minefield.

@subsubsection{Configurability}

@(ASDF1) was much improved over what preceded it,
but its configuration mechanism was still lacking:
there was no modular way
for whoever installed software systems to register them
in a way that users could see them;
and there was no way for program writers to deliver executable scripts
that could run without knowing where libraries were installed.

One key feature introduced with @(ASDF2) @~cite[Evolving-ASDF] was
a new configuration mechanism for programs to find libraries,
the @bydef{source-registry}, that followed this guiding principle:
@bold{Each can specify what he knows, none need specify what he doesn't}.
Configuration information is taken from multiple sources,
with the former partially or completely overriding the latter:
argument explicitly passed to @cl{initialize-source-registry},
environment variable,
central user configuration file,
modular user configuration directory,
central system configuration files,
modular system configuration directories,
implementation configuration.
Also, the source-registry is optionally capable
of recursing through subdirectories
(excluding source control directories),
where @tt{*central-registry*} itself couldn't.

A similar mechanism, the @bydef{output-translations},
also allows to specify where output files are to be stored,
which by default seggregates them by
implementation, operating system, ABI, version, etc.,
allowing for sharing source code between several users
who themselves may use many different versions of many implementations, etc.
Thus, whoever or whichever software manages installation of source code
does not have to also know which compiler is to be used by which user;
configuration remains modular, and code can be shared by all who trust it,
without affecting those who don't.
There used to be an extension to @(ASDF1) called @tt{asdf-binary-locations}
that fulfilled the same functionality,
but apart from its suffering
from the same lack of modularity as the @tt{*central-registry*},
it also had a chicken-and-egg problem:
you couldn't use @(ASDF) to load it without having at least one
program compiled without @tt{asdf-binary-locations} enabled,
namely @tt{asdf-binary-locations} itself;
it thus required special purpose loading and configuration
in whichever file did the loading @(ASDF), making it not modular at all.
This was resolved by moving the functionality into @(ASDF) itself,
illustrating the design principle followed by @(ASDF2):
@bold{make it as simple as possible, but no simpler}.
This is in contrast with the principle apparently followed for @(ASDF1):
"make it as simple as possible while handling the common case correctly"
— which might have been a great principle for experimenting with new concepts,
as Dan Barlow was doing, but was not a good one for a robust product.

Configurability decoupled use and installation of software:
multiple parties could now each modularly contribute some software,
whether applications, libraries or implementations,
and provide configuration for it without being required
to know configuration of other software;
previously, whoever installed software couldn't notify users,
and users had to know and specify configuration of all software installed.

@subsubsection{Robustness}

During the development of @(ASDF2) (then 3),
a great number of bugs were introduced, and even more bugs were fixed.
We eventually acquired the discipline to systematically write regression tests
and tests for new features;
Furthermore, we took to regularly running the entire test suite
on all supported implementations (the list of which steadily grew),
particularly so just before releases.
And the code was not to be released unless
every regression test passed on every implementation
or was marked as a known failure due to some implementation bugs.
The test system itself was vastly improved
to make it easier to reproduce failures and debug them,
and to handle a wider variety of test cases.

This led to very robust code,
at least compared to previous @(CL) build systems,
that runs the same way in a great variety of contexts:
on different implementations and operating systems,
using various combinations of features,
after some kind of hot software upgrade, etc.

Robustness decoupled the testing of systems that use @(ASDF)
from testing of @(ASDF) itself:
assuming the @(ASDF) test suite is complete enough
(sadly, all too often a preposterous assumption),
a @tt{defsystem} definition that works on one implementation
will work identically on all implementations.
As for the code in the system itself —
it might still require testing on all supported implementations,
since the semantics of @(CL) is not fully specified
but leaves a lot of leeway to implementors, unlike e.g. ML or Java.

@subsubsection{Usability}

Usability was an important concern while developing @(ASDF2).
Portability, Configurability and Robustness already contribute to Usability,
as does all improvements to the software;
some changes were made, though, that were specifically introduced
to ease usability of @(ASDF).

As a trivial instance, the basic @(ASDF) invocation was the clumsy
@tt{(asdf:operate 'asdf:load-op :foo)} or
@tt{(asdf:oos 'asdf:load-op :foo)}.
With @(ASDF2), that would be the more obvious
@tt{(asdf:load-system :foo)}.

The way pathname are specified was made portable, with
@(ASDF2) adopting Unix pathname syntax
as an abstraction to specify pathnames while using portable @(CL) semantics.
It became easy to specify relative pathnames,
where previously doing it portably was extremely tricky.
@(ASDF2) similarly provided sensible rules for pathname types and type overrides.
(See the Appendix on pathnames.)

Usability decoupled the knowledge of how to use @(ASDF)
from the knowledge of @(ASDF) internals and @(CL) pathname idiosyncrasies.
Any beginner with a basic understanding of @(CL) and Unix pathnames
could now use @(ASDF) where defining a non-trivial system,
what more portably, was previously a task reserved to experts
and/or involving copy-pasting magical incantations.
The principle followed was that
@bold{the cognitive load on each kind of users must be minimized}.

@subsection{Features introduced in the @(ASDF2) series}

@subsubsection{Working @tt{defsystem} dependencies}

2011.
@tt{:defsystem-depends-on}

@subsubsection{Working selective system forcing}

2011, 2012.
@tt{:force}, @tt{:force-not}, @tt{require-system}.

@subsubsection{Encoding support}

2012.

@subsubsection{Hooks aroud compilation}

@tt{:around-compile}
@tt{:compile-check}

@subsubsection{The end}

The @(ASDF2) series culminated with @(ASDF) 2.26,
at which time all the code had been rewritten,
and the core @tt{traverse} algorithm had been refactored
into smaller, more understandable functions.

Last bug to fix... opened a Pandora's box.


@subsection{@(ASDF) 3: A Mature Build}

@(ASDF3) (2013) was completely revamped, twice over, to correctly compute timestamps
across an arbitrary action graph;
users may specify how operations do or don't propagate
along the component hierarchy.

@(ASDF3) was pre-released as 2.27 in February 2013,
then officially released as 3.0.0 on May 15th 2013, with further stable releases since.

@subsubsection{Portability Layer}

@(ASDF3) notably brought a user-visible portability layer UIOP.
Abstracting over pathnames, filesystem access;
but also run-program, Lisp image lifecycle, and many other everyday concerns.

@subsubsection{extensibility}

Better operations.

@subsubsection{configurability}

better supported are conditional compilation, versioning, and more.

@subsubsection{internal consistency}

@subsubsection{creating standalone executables}

there are several ways to deliver software in a single-file "bundle",
including a single FASL for a system and/or its dependencies, or
(on supported implementations) a standalone executable programs;


@subsection{@(ASDF) 3.1: Lifting Lisp}

@(ASDF) 3.1 (2014) builds on top of @(ASDF3) to provide new features.

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
@(ASDF) 3.1 provides solutions to these issues, and many more.

Future: Robert Goldman assumed maintainership in July 2013,
a few months after the release of @(ASDF3),
and but François-René Rideau remained main developer until
release of 3.1.1 in February 2014.


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

@subsection{Uniformity}

While developing @(ASDF), we sometimes made many things more uniform
at the cost of a slight backward incompatibility
with a few existing systems using kluges.
For instance, @(ASDF2) made pathname arguments uniformly non-evaluated in a @tt{defsystem} form,
when they used to be evaluated for toplevel systems but not for other (most) components;
this evaluation was used by a few users to use @tt{merge-pathnames}
to portably specify relative pathnames, a task made unnecessary by
@(ASDF2) being capable of specifying these pathnames portably with Unix syntax.

@(ASDF3) removed the magic undocumented capability of specifying a systems
as dependencies of a system or module by declaring it in the list of subcomponent
rather than the list of dependencies;
this capability seems to have been an undesigned artifact of how systems used to be parsed,
though at the same time it seems to have been compatible with how some older defsystems did things,
and one user relied on the capability whose system definition had been ported from @tt{mk-defsystem}.

At the cost of a handful of users having to cleanup their code a bit,
we could thus notably @bold{reduce the cognitive load on users} for all future systems.
No more need to learn complex syntactic and semantic constraints
and even more complex tricks to evade those constraints.

@section{Conclusion: Lessons for Language Growers}

@(generate-bib)

@section[#:style (make-style 'appendix '(unnumbered))]{Appendix A: Pathnames}

