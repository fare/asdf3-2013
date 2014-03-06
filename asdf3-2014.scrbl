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

Better late than never,
with the release of @(ASDF3) in May 2013,
all the programs for which one uses so-called "scripting" languages
may now be written @emph{portably} in @(CL):@note{
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
  to fully take advantage of extant libraries
  that harness modern hardware and software techniques
  requires the use of various extensions.
  Happily, each implementation provides its own extensions,
  and there exist libraries to abstract over
  the discrepancies these implementations, and provide portable access to
  threads (@tt{bordeaux-threads}), unicode support (@tt{cl-unicode}),
  a "foreign function interface" to libraries written in C (@tt{cffi}),
  ML-style pattern-matching (@tt{optima}), etc.
  A software distribution system, @(Quicklisp),
  makes it easy to install hundreds of such libraries,
  that were already using @(ASDF) as a basic software organization mechanism.
  The new features in @(ASDF3) were only the last missing pieces in this puzzle.
}
starting with quick scripts that glue together functionality provided
by the operating system, external programs, C libraries, or network services;
scaling them seamlessly into large, maintainable, modular, systems;
and making those new services available to other programs via the command-line
as well as via network protocols, etc.

@(ASDF) has been the @(de_facto) standard build system
for portable @(CL) software since shortly after its release
by Dan Barlow in 2002 @~cite[ASDF-Manual].
@moneyquote{The purpose of a build system is
                to enable division of labor in software development}:
source code is organized in separately developed components
that can depend on other components,
and the build system transforms the transitive closure of these components
into a working program.

@(ASDF3), latest rewrite of the system,
beside fixing numerous bugs deep and shallow
somewhat faster than were introduced,
sports a portability layer, @(UIOP),
that makes this portable scripting possible:
it enables the writing of Lisp programs
that may be invoked from the command line,
or may spawn external programs and capture their output;
it allows to deliver these programs as standalone executable files,
or, with the companion script @tt{cl-launch} (see @secref{cl-launch})
as light-weight scripts that can be run unmodified
on many different kinds of machines each differently configured.
Previously, key parts of a program had to be configured to match
one's specific @(CL) implementation and specific software installation paths.
Now, all of one's usual Unix scripting needs can be entirely fulfilled by @(CL),
while benefitting from its efficient implementations, hundreds of software libraries, etc.

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

@subsubsection{Components}

@(ASDF) is a build system for @(CL):
it helps developers divide software into a hierarchy of @bydef{component}s,
and automatically generate a working program from all the source code.

Top components are called @bydef{system}s in an age-old Lisp tradition,
while the bottom ones are source files, typically written in @(CL).
Users may then @tt{operate} on these components with various build @bydef{operation}s,
most prominently compiling the source code (operation @tt{compile-op}) and
loading the result in the current Lisp image (operation @tt{load-op}).

Several related systems may be developed together
in a same source code @bydef{repository}.
Each system may depend on code from other systems,
that may come from the same repository, or from a different repository.
@(ASDF) itself has no notion of repositories,
but other tools, on top of @(ASDF), do:
@(Quicklisp) @~cite[quicklisp] will provide together
systems from a same repository as a @bydef{release},
and provide hundreds of releases as a @bydef{distribution},
and automatically download on demand
required systems and all their transitive dependencies.

A system is itself organized in a hierarchy of components,
where each component may be either an individual @bydef{file}
(usually, a @(CL) source file),
or a @bydef{module} that may recursively contain other components.
Modules may or may not directly fit the filesystem directory hierarchy.

Further, each component may explicitly declare
a @bydef{dependency} on other components.
A component @tt{depends-on} other components
that contain definitions for
packages, macros, variables, classes, generic functions,
and any functions that it uses at compile-time,
notably during the read and macro expansion phases.

@subsubsection{Example system definitions}

For instance, here is how the @tt{fare-quasiquote} system is defined
in a file @tt{fare-quasiquote.asd}:@note{
  Examples in this article have been slightly edited down from actual code,
  both to fit in the article format and better illustrate the points at hand.
}

@clcode{
(defsystem "fare-quasiquote"
  :description "Portable, matchable
  implementation of quasiquote"
  :license "MIT"
  :depends-on ("fare-utils")
  :components
  ((:file "packages")
   (:file "quasiquote"
    :depends-on ("packages"))
   (:file "pp-quasiquote"
    :depends-on ("quasiquote")))
  :in-order-to
  ((test-op
    (test-op :fare-quasiquote-test))))
}

Notice how it depends on another system, @tt{fare-utils},
a collection of utility functions and macros from another repository,
whereas testing is specified to be done by @tt{fare-quasiquote-test},
a system defined in a different file @tt{fare-quasiquote-test.asd}
within the same repository.
It otherwise contains three files, @tt{packages.lisp},
@tt{quasiquote.lisp} and @tt{pp-quasiquote.lisp}
(the pathname type is automatically added based on the component class;
here, the default @tt{.lisp}; see @secref{Pathnames}).
The latter files two each depend on the first,
that defines the @(CL) packages@note{
  Each @(CL) image has a global flat two-level namespace of symbols in packages:
  packages are identified by their name, a string;
  in each package, symbols are identified by their name, a string.
  However, this namespace is not global across images,
  because packages can import symbols from other packages,
  but without renaming,
  and @(CL) between processes running different code bases
  will differently intern (or fail to intern) symbols.
}

The @tt{fare-utils.asd} file, in its own repository,
looks like this (with a lot of elisions):

@clcode{
(defsystem "fare-utils" ...
  :components
  ((:file "package")
   (:module "base"
    :depends-on ("package")
    :components
    ((:file "utils")
     (:file "strings" :depends-on ("utils"))
     ...))
   (:module "filesystem"
    :depends-on ("base")
    :components
    ...)
   ...))
}

This illustrates the use of modules,
with a file @tt{package.lisp},
a module @tt{base},
that, in absence of contrary declaration,
corresponds directory @tt{base/},
and itself contains files
@tt{utils.lisp}, @tt{strings.lisp}, etc.
As you can see, dependencies name @bydef{sibling} components
under the same @bydef{parent} system or module,
that can themselves be files or modules.

@subsubsection{Action graph}
@; TODO: add a graph!

Building software is modeled as a Direct Acyclic Graph (DAG) of @bydef{action}s:
each action is a pair of an operation and a component,
and must be @bydef{perform}ed but only after
all the actions it depends on have already been performed.

For instance, in @cl{fare-quasiquote} above,
the @emph{loading} of @tt{quasiquote.lisp} depends-on
the @emph{compiling} of @tt{quasiquote.lisp},
which itself depends-on the @emph{loading} of @tt{package.lisp}, etc.

Importantly, though, this graph is distinct
from the preceding graph of components:
the graph of actions is not a mere refinement of the graph of components,
but a transformation of it that also incorporates
crucial information about the structure of operations.

Unlike its immediate predecessor @tt{mk-defsystem},
@(ASDF) makes a @bydef{plan} of all actions needed
to obtain an up-to-date version of the build output,
before it @bydef{performs} these actions.
In @(ASDF) itself, this plan is a list of actions to be performed sequentially.
But it is possible to write an extension that reifies
the complete direct acyclic graph of actions to be performed in parallel.@note{
  Indeed, Andreas Fuchs in 2006 wrote a very small but quite brilliant @(ASDF) extension
  called @(POIU), the Parallel Operator on Independent Units,
  that compiles files in parallel on Unix multiprocessors using @tt{fork},
  while still loading them sequentially, minimizing latency.

  François-René Rideau later rewrote @(POIU), making it
  both more portable and simpler by, co-developing it with @(ASDF).
  Understanding the sometimes bizarre and useless-looking
  but actually extremely clever and emminently necessary tricks
  by which Andreas Fuchs overcame the limitations and conceptual bugs of @(ASDF)
  to build such a complete DAG of actions
  led to many aha moments, instrumental when fixing @(ASDF2) into @(ASDF3).
}
In making this plan,
@(ASDF) ensures that before the action that compiles or loads component is performed,
all the actions that compile and load its declared dependencies have themselves been performed,
which also includes all their own transitive dependencies.

@subsubsection{In-image}

Finally, it is important to note that
@moneyquote{@(ASDF) is an @q{in-image} build system}:
it compiles and loads systems in the current @(CL) image.
For better or worse, this notably differs from the practice in most other languages,
where the build system is a completely different piece of software running in a separate process:
on the one hand, it makes it somewhat easier to extend the build system;
on the other hand, it puts great pressure on @(ASDF) to remain minimal.

Qualitatively, it must be delivered as a single source file
and cannot use any external library,
since it itself defines the code that may load other files and libraries.
Quantitatively, @(ASDF) has to be present in all programs being built
and any memory it occupies is consumed for all —
though this mattered more in 2002 when @(ASDF) was first released
and was about a thousand line long,
than in 2014 when it has grown over ten times the size,
but memory sizes have increased even faster.

Still, for all these reasons, @(ASDF) follows the minimalist principle that
@moneyquote{anything that can be provided as an extension
            should be provided as an extension and left out of the core}.
@(ASDF) thus cannot afford to include, say,
management of an advanced persistence cache indexed by cryptographic digest of the contents,
or control of distributed network of cross-compiling workers, etc. —
but these features are conceivable as @(ASDF) extensions.

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

Nevertheless, there are also many ways in which @(ASDF) pales in comparison
to other build systems for @(CL), C, Java, or other systems:

@itemlist[
  @item{
    @(ASDF) is not a general-purpose build system.
    Its simplicity is directly related to its being custom made
    to build @(CL) software only.
    Seen one a way, it's a sign of how little you can get away with
    if you have a good basic architecture,
    the way that is not available in lesser programming languages,
    that require much more complex tools to achieve a similar purpose.
    Seen another way, it's also the @(CL) community failing to embrace
    the outside world and provide solutions with enough generality
    to solve more complex problems.@note{
      @(ASDF3) could be easily extended to support arbitrary build actions,
      if there were an according desire; but @(ASDF1) and 2 couldn't:
      their action graph was not general enough,
      being simplified and tailored for the common use case
      of compiling and loading Lisp code;
      and their ability to call arbitrary shell programs
      was a misdesigned afterthought (copied over from @tt{mk-defsystem})
      the implementation of which wasn't portable, with too many corner cases.
    }
  }
  @item{
    At the other extreme, a build system for @(CL) could have been made
    that is much simpler and more elegant than @(ASDF),
    if it could have required software to follow some simple organization constraints,
    without much respect for legacy:
    a constructive proof of that is Alastair Bridgewater's @tt{quick-build}
    (or the similar and earlier @tt{faslpath} by Peter Etter),
    being a fraction of the size of the original @(ASDF), which is a fraction of @(ASDF3)'s,
    and with a fraction of the bugs — but none of the generality and extensibility.
    @; See below @secref{asdf-package-system}. XXX
  }
  @item{
    Because of its age-old model of building software in-memory, what more
    in traditional friendly single-user, single-processor, single-machine environments
    with a single coherent view of source code and single target configuration,
    @(ASDF) isn't geared at all to build large software
    in modern adverseurial multi-user, multi-processor, distributed environments
    where source code comes in many divergent versions and yet as many configurations.
    Now that with @(ASDF3) the action model has been made consistent and general enough,
    it could conceivably be made to scale, but it's a far cry from anything like it,
    and not the most promising build system to start from —
    though if the alternative were to start from scratch,
    there are many good ideas in @(ASDF), and probably a lot of code to reuse
    if that alternative is written in @(CL).
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
  and therefore @tt{mk-defsystem} because @(de_facto) standard for free software.
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
wanted a good @tt{defsystem} variant suitable for his needs,
notably regarding extensibility.
Instead of attempting to modify @tt{mk-defsystem}, at high cost for little expected benefit,
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
without the need to modify the main source file.@note{
  Dan Barlow may also have gotten from Kent Pitman
  the idea of executing a reified plan rather than
  walking the dependencies on the go.
}
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
@emph{the simplest code that would work in the common case},
giving him most leeway to experiment.
His code had a lot of rough edges:
while @(ASDF) worked great on the implementation he was using
for the things he was doing with it,
it often in ugly ways when using other implementations,
or exercising corner cases he had never tested;
the naive use of lists as a data structure
didn't scale to large systems with thousands of files;
the extensibility API was lacking in many ways
such that power users took to redefining
or overriding @(ASDF) internals with modified variants,
which made maintenance costly.

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
  In @(ASDF3),
  some of the upgrade complexity described in our 2010 paper was done away with:
  even though @moneyquote{@(CL) makes dynamic data upgrade extraordinarily easy}
  as compared to other languages, we found that it's not easy enough to maintain;
  therefore instead of trying hard to maintain that code,
  we "punt" and drop in-memory data if the schema has changed in incompatible ways;
  thus we do not try hard to provide methods for @tt{update-instance-for-redefined-class}.
  The only potential impact of this reduction in upgrade capability
  would be users who upgrade code in a long-running live server;
  but considering how daunting that task is, properly upgrading @(ASDF)
  despite reduced support might be the least of their problems.
  To partly compensate this issue,
  @(ASDF3) will preemptively attempt to upgrade itself
  at the beginning of every build
  (if an upgrade is available as configured)
  — that was recommended but not enforced by @(ASDF2).
  This reduces the risk of either having data to drop from a previous @(ASDF),
  or much worse, being caught upgrading @(ASDF) in midflight.
  In turn, such special upgrading of @(ASDF) itself makes code upgrade easier.
  Indeed, we had found that
  @moneyquote{@(CL) support for hot upgrade of code may exist
                    but is anything but seamless}.
  These simpler upgrades allow us to simply use @tt{fmakunbound} everywhere,
  instead of having to @tt{unintern} some functions before redefinition.
}
Soon enough, users felt confident relying on bug fixes and new features,
and all implementations started providing @(ASDF2).

These days, you can @tt{(require "asdf")} on pretty much any @(CL) implementation,
and start building systems using @(ASDF).
Most implementation already provide @(ASDF3).
A few still lag with @(ASDF2), or fail to provide @(ASDF);
the former can be upgraded with @cl{(asdf:upgrade-asdf)},
we provide a script to automatically install @(ASDF3),
that works on all those implementations that are not too completely obsolete.

Upgradability crucially decoupled what @(ASDF) users could rely on
from implementations provided, enabling a virtuous circle of universal upgrades,
where previously where everyone was waiting for others to upgrade, in a deadlock.
@moneyquote{Allowing for divergence creates an incentive towards convergence}.

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
@(ASDF) 2.26 (last in the @(ASDF2) series) supported 15, adding:
@tt{cormanlisp}, @tt{genera}, @tt{mkcl}, @tt{rmcl}, @tt{scl}, @tt{xcl}.
Since then, new implementations are being released with @(ASDF) support:
@tt{mocl}, and hopefully soon @tt{clasp}.

@(ASDF) as originally designed would only reliably work on Unix variants
(Linux, BSD, etc., maybe cygwin, and now also MacOS X, Android, iOS).
It can now deal with very different operating system families:
most importantly Windows, but also the ancient MacOS 9 and Genera.

Portability is important, because depending on which
specific task you're performing on which operating system, architecture, etc.,
you'll want a different implementation.
For instance, SBCL is quite popular for its runtime speed
on intel-compatible Linux machines;
but since it is slower at compiling,
isn't the best choice for a quick script where fast startup matters;
it also won't run on ARM, and doesn't have the best Windows support;
and so you might prefer Clozure CL or another implementation,
depending on your constraints.

Portability was achieved by following the principle that
@moneyquote{we must abstract away semantic discrepancies between underlying implementations}.
This is in contrast with the principle apparently followed by @(ASDF1),
to @q{provide a transparent layer on top of the implementation,
and let users deal with discrepancies}.
@(ASDF2) thus started growing an abstraction layer
that works around bugs in each implementation and
smoothes out incompatibilities, which made the @(ASDF) code itself larger,
but allowed user code to be smaller for portable results.

The greatest source of portability woes was in handling @emph{pathnames}:
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
implementation configuration, with sensible defaults.
Also, the source-registry is optionally capable
of recursing through subdirectories
(excluding source control directories),
where @tt{*central-registry*} itself couldn't.
Software management programs at either user or system level
could thus update independent configuration files in a modular way
to declare where the installed software was located;
users could manually edit a file describing where he manually downloaded software;
users could export environment variables to customize or override
the default configuration with context-dependent information;
and scripts could completely control the process
and build software in a predictable, deterministic way;
it is always possible to take advantage of well-configured system,
and always possible to avoid and inhibit any misconfiguration
that was out of one's control.

A similar mechanism, the @bydef{output-translations},
also allows to specify where output files are to be stored,
depending on where the corresponding input files are located.
By default, @(ASDF) will seggregate these output files by
implementation, operating system, ABI, version, etc.,
allowing for sharing source code between several users
who themselves may use many different versions of many implementations, etc.
Thus, whoever or whichever software manages installation of source code
does not have to also know which compiler is to be used by which user at what time.
The configuration remains modular, and code can be shared by all who trust it,
without affecting those who don't.

There used to be an extension to @(ASDF1) called @tt{asdf-binary-locations}
that fulfilled the same functionality;
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

@(ASDF) used to pay no attention to robustness.
A glaring issue, for instance, that was causing much aggravation in large projects
was that interrupting a build while in the middle of compiling a file
would result in a corrupt output file that would poison further builds
until it was manually removed:
    @; https://bugs.launchpad.net/asdf/+bug/587889
@(ASDF) would fail the first time, then when restarted a second time,
would silently load the partially compiled file,
leaving the developer believing the build had succeeded when it hadn't,
and then having to either debug an incomplete system.
The problem could be even more aggravating, since it would sometimes happen
because the compilation itself resulted in a fatal error
(especially since in @(CL), developers can run arbitrary code
to run during compilation, and that code can have fatal bugs);
the developer, after restarting compilation, might not see the issue,
and committing a change that others had to track down and painfully debug,
because it broke their build in a way that mattered more directly to them.
This was fixed by having @(ASDF) compile into a temporary location,
and move the outputs to their destination only in case of success, atomically.@note{
  Not all Lisp implementations and/or underlying operating systems
  allowed this replacement to be atomic.
  In the latest @(ASDF3), the function @cl{uiop:rename-file-overwriting-target}
  abstracts over the details.
}
A lot of corner cases similarly had to be handled to make the build system robust.

We eventually acquired the discipline to
@moneyquote{systematically write tests for new features and fixed bugs}.
The test system itself was vastly improved
to make it easier to reproduce failures and debug them,
and to handle a wider variety of test cases.
Furthermore, we adopted the policy that the code was not to be released
unless every regression test passed on every supported implementation
(the list of which steadily grew),
or was marked as a known failure due to some implementation bugs.
Unlike @(ASDF1), that focused on getting the common case working,
and letting users sort out non-portable uncommon cases with their implementation,
@(ASDF2) followed the principle that code should either work of fail everywhere the same,
and in the latter case,
@moneyquote{fail early for everyone rather than pass as working for some}
and fail mysteriously for others.
These two policies led to very robust code,
at least compared to previous @(CL) build systems including @(ASDF1).

Robustness decoupled the testing of systems that use @(ASDF)
from testing of @(ASDF) itself:
assuming the @(ASDF) test suite is complete enough,
(sadly, all too often a preposterous assumption),
systems defined using @(ASDF2) idioms
will run the same way in a great variety of contexts:
on different implementations and operating systems,
using various combinations of features,
after some kind of hot software upgrade, etc.
As for the code in the system itself —
it might still require testing on all supported implementations
in case it doesn't strictly adhere to a portable subset of @(CL)
(which is not automatically enforceable so far),
since the semantics of @(CL) is not fully specified
but leaves a lot of leeway to implementors, unlike e.g. ML or Java.

@subsubsection{Performance}

@(ASDF1) performance didn't scale well to large systems,
because Dan Barlow was using the @tt{list} data structure everywhere
while walking a system, leading to a polynomial increase of running time
as the size of systems increased.
However, it did scale reasonably well to a large number of small systems,
because it was using a hash-table to find systems.
We assume that Dan Barlow made these choices
for the sake of coding simplicity while experimenting,
and that his minimalist tendency was skewed towards using lists
by the presence of many builtin functions
supporting this old school Lisp programming style.
In any case,
@(ASDF2) followed the principle that
@moneyquote{good data structures and algorithms matter},
and should be tailored to the target problem;
it supplemented or replaced the lists used by @(ASDF1)
with hash-tables for name-lookup
and append-trees to recursively accumulate actions,
and achieved linear increase in running time
as the size of systems increased.
@(ASDF2) therefore performed well whether or not the code
was split in a large number of systems.

Sound performance decoupled the expertise in writing systems
from the expertise in how systems are implemented.
Now, developers could organize or reorganize their code
without having to shape it in a particular way
to suit the specific choice of internals by @(ASDF) itself.

@subsubsection{Usability}

Usability was an important concern while developing @(ASDF2).
Portability, Configurability, Robustness and Performance
already contribute to Usability,
as do all improvements to the software;
some changes were made, though, that were specifically introduced
to ease usability of @(ASDF).

As a trivial instance, the basic @(ASDF) invocation was the clumsy
@tt{(asdf:operate 'asdf:load-op :foo)} or
@tt{(asdf:oos 'asdf:load-op :foo)}.
With @(ASDF2), that would be the more obvious
@tt{(asdf:load-system :foo)}.@note{
  @tt{load-system} was actually implemented
  by Gary King, the last maintainer of @(ASDF1), in June 2009;
  but it isn't until @(ASDF2) made it possible in 2010
  for everyone to use an up-to-date @(ASDF)
  that users could @emph{rely} on @tt{load-system} being there.
}

@(ASDF2) provided a portable way to specify pathnames
by adopting Unix pathname syntax as an abstraction,
while using standard @(CL) semantics underneath.
It became easy to specify hierarchical relative pathnames,
where previously doing it portably was extremely tricky.
@(ASDF2) similarly provided sensible rules for pathname types and type overrides.
(See @secref{Pathnames}.)
@(ASDF) made it hard to get pathname specifications right portably;
@(ASDF2) @moneyquote{made it hard to get it wrong} or make it non-portable.

Usability decoupled the knowledge of how to use @(ASDF)
from the knowledge either @(ASDF) internals or @(CL) pathname idiosyncrasies.
Any beginner with a basic understanding of @(CL) and Unix pathnames
could now use @(ASDF) where defining a non-trivial system,
what more portably, was previously a task reserved to experts
and/or involving copy-pasting magical incantations.
The principle followed was that
@moneyquote{the cognitive load on each kind of users must be minimized}.

@subsection{Features introduced in the @(ASDF2) series}

@subsubsection{Working @tt{defsystem} dependencies}

@(ASDF2) introduced a @tt{:defsystem-depends-on} option to @tt{defsystem},
whereby a system could declaratively specify dependencies on build extensions.
Before that option, users would imperatively load any extension they need:
in their @tt{asd} system definition file,
they would for instance evaluate @cl{(asdf:load-system :cffi-grovel)},
before they use @cl{defsystem} to define their systems.
Indeed, a @tt{asd} file is just a Lisp source file
that is loaded in a controlled context and may contain arbitrary side-effects;
but such side-effects are frowned upon and
a declarative style is more maintainable,
hence this improvement.

However, this feature was only made be usable in 2.016 (June 2011),
when @(ASDF) started to accept keywords as designators
for classes defined in an extension in the @tt{:asdf} package.
Before then, there was a chicken-and-egg problem,
because the @tt{defsystem} form containing the @tt{:defsystem-depends-on} declaration
was read before the extension was loaded
(what more, read in a temporary package, under @(ASDF1) and @(ASDF2));
therefore, the extension had nowhere to intern or export any symbol
that the rest of the @tt{defsystem} form could use.

These days, this feature is the recommend way of loading extensions.
But from the story of it, we can learn that
@moneyquote{a feature isn't finished until it is used in production and tested}.
Until then, there are likely issues that will need to be addressed.

As an example use, the proper way to use the CFFI library
is to use @cl{:defsystem-depends-on ("cffi")} as below,
which will ensure CFFI is loaded before the system is processed;
then CFFI defines the class @tt{asdf::cffi-grovel},
that can be designated by the keyword @tt{:cffi-grovel}
amongst the components of the system.

@clcode{
(defsystem "some-system-using-ffi"
  :defsystem-depends-on ("cffi")
  :components
  ((:cffi-grovel "foreign-functions")
   ...))
}

@subsubsection{Working selective system forcing}

Since the beginning, @(ASDF) has a mechanism
to force recompilation of everything:
@clcode{
  (asdf:oos 'asdf:load-op 'my-system :force t)
}
Which in a recent @(ASDF) would be more colloquially:
@clcode{
  (asdf:load-system :my-system :force :all)
}
As early as 2003, Dan Barlow introduced in @(ASDF)
a mechanism to @emph{selectively} @cl{:force}
the recompilation of some systems, but not others:
@cl{:force :all} would force recompilation of all systems;
@cl{:force t} would only force recompilation of the requested system;
and @cl{:force '(some list of systems)}
would only force recompilation of the specified systems.
However, his implementation had two bugs:
@cl{:force t} would continue to force everything, like @cl{:force :all};
and @cl{:force '(some list of systems)} would cause a runtime error
(that could have been found at compile-time with static strong typing).

Instead, the bugs were found in 2010 while working on @(ASDF2);
the code was partially fixed, but
support for the selective syntax was guarded by a continuable error message
inviting users to contact the maintainer.@note{
  @(CL) possesses a mechanism for continuable errors, @tt{cerror},
  whereby users can interactively or programmatically
  tell the system to continue despite the error.
}
Despite the feature demonstrably not ever having had any single user,
it had been partially documented, and so was finally
fixed and enabled in @(ASDF 2.015) (May 2011) rather than removed.

Then the feature was extended in @(ASDF 2.21) (April 2012)
to cover a negative @tt{force-not} feature,
allowing the fulfillment of a user feature request:
a variant @tt{require-system} of @tt{load-system}
that makes no attempt to upgrade already loaded systems.
This is useful in some situations: e.g. where large systems
already loaded and compiled in a previously dumped image
are known to work, and need to promptly load user-specified extensions,
yet do not want to expensively scan every time
the (configured subset of the) filesystem
for updated (or worse, outdated) variants of their source code.
The hook into the @cl{require} mechanism was then amended to use it.@note{
  The two mechanisms were further enhanced in @(ASDF3), then in @(ASDF3.1).
  One conceptual bug was having the @cl{:force} mechanism take precedence over @cl{:force-not};
  this didn't fit the common use cases of users having
  a set of immutable systems that shouldn't be refreshed at all, and
  needing to stop a @cl{:force :all} from recursing into them.
  This was only fixed in @(ASDF3.1).
}

This illustrates both Dan Barlow's foresight and
his relatively lack of interest in developing @(ASDF)
beyond the point where it got the rest of his software off the ground;
and by contrast the obsession to detail of his successor.

@subsubsection{Hooking into @cl{require}}

Imitating the example set by Dan Barlow on SBCL in @(ASDF1),
@(ASDF2) was made to hook into the @cl{require} mechanism
of a growing number of implementations, so far (March 2014), seven:
ABCL, GNU CLISP, Clozure CL, CMUCL, ECL, MKCL, SBCL
— and this list notably coincides with that of all
the maintained free software implementations.
Thus, on all these implementations, users could,
after they @cl{(require "asdf")},
use this standard but deliberately underspecified @(CL) mechanism
for extending the language:
by evaluating @cl{(require :foo)} they could have it implicitly rely on @(ASDF)
to load the system if not present yet.

However, users ended up mostly not using it, we presume for the following reasons:
@itemlist[
  @item{
    This mechanism is still not ubiquitous enough,
    therefore for portability and reliability,
    you have to know about @(ASDF) and be able fall back to it explicitly, anyway;
    thus trying to "optimize" the easy case with @tt{require}
    is just gratuitous cognitive load for no gain;
    this is illustrates once again the principle that
    @moneyquote{it's counter-productive to standardize underspecified software};
    it's better to specify some situations to cause an error,
    and to reserve any resolution to a later version of the standard
    (and then follow up on it), or to delegate it to a different standard;
    but underspecifying a specification is only inviting
    a hell of mutually incompatible variants the use of which will bring
    as many portability landmines.
    In case of disagreement, it is much better to let each implementation's variant
    exist in its own, distinct namespace, which avoids any confusion.
  }
  @item{
    The @tt{require} mechanism purposefully avoids loading a module that has already been provided,
    thereby making it unpopular in a culture of ubiquitous modifiable source code;
    if you modified a file, you really want it to be reloaded automatically.@note{
      At the same time, @(ASDF) wasn't reliable in avoiding to reload provided modules,
      since most systems don't call @cl{provide} with their name to signal that such
      call to @cl{require} was successful, and therefore next call to @cl{require}
      would cause a new load attempt — this was fixed with the introduction of
      the above-mentioned @cl{require-system} in @(ASDF 2.21) in 2012,
      and its use instead of @cl{load-system}.
    Maybe the more general point is that @(ASDF) did not have a good story with regards to
    extending the set of things that are considered "system" versus "user" defined.
    @(ASDF3.1) adds a notion of "immutable systems"
    that should not be refreshed from filesystem once loaded into memory.
    @; https://bugs.launchpad.net/asdf/+bug/1184002
  } } ]

@subsubsection{Encoding support}

Back in 2002, most programmers were still using 8-bit characters in various encodings
(latin1, koi8-r, etc.), and Emacs did not support unicode very well, if at all.
@(ASDF1) in its typical minimalist manner, just didn't specify any @cl{:external-format}
and let the programmer deal with the implementation-dependent configuration
of character encodings, if such an issue mattered to them.
Most code was written in 7-bit ASCII, so usually, there was no issue;
but occasionally, one would attempt to load a file encoded with latin1
in a Lisp expecting strictly UTF-8 input, resulting in an error;@note{
  The encoding issues during compilation were made worse by the (legitimate) behavior of SBCL:
  at the same time, (1) SBCL set the default encoding in a given session
  based on the same environment variable as the @tt{libc} locale,
  which could vary wildly between developers, even more between hypothetical end-users, and
  (2) SBCL would issue an error rather accept invalid UTF-8.
}
or one would load a UTF-8 or Shift-JIS encoded file
in a latin1 configured Lisp, resulting in mojibake.

By 2012, however, Unicode was ubiquitous,
UTF-8 was a @(de_facto) standard, and Emacs supported it well.
A few library authors had started to rely on it (if only for their own names).
To make the loading of library code more predictable,
@(ASDF2) added an @tt{:encoding} option to @tt{defsystem},
so that files may be loaded in the encoding they were written in,
irrespective of which encoding the user may otherwise be using.
Once again, the principle
	@emph{each can specify what he knows,
              none need specify what he doesn’t.}
In this case, the person who knows what encoding he used
is the author of the library.

The encoding option of a system or module was inherited by its components,
unless they overrode it.
The accepted syntax of the option is a keyword, abstracting over
the implementation-dependent @cl{:external-format},
which is not specified by the @(CL) standard.@note{
  And indeed, though all other implementations that support unicode
  accept the keyword @cl{:utf-8} as an external format,
  GNU CLISP, always the outlier,
  wants the symbol @cl{charset:utf-8} in a special package @cl{charset}.
}
The only encoding supported out of the box is @cl{:utf-8},
because that's the only universally accepted encoding that's useful;
but if your system specifies
@cl{:defsystem-depends-on ("asdf-encodings")},
it can use any encoding your implementation supports.
However, the only other completely portable option is @cl{:latin1},
the previous implicit standard being evolved from.
On old implementations without support for Unicode or external-formats,
@(ASDF) fallback to using the 8-bit implementation @cl{:default}.

Though @cl{:utf-8} was the @(de_facto) standard,
the default was temporarily left to @cl{:default}
for backward-compatibility,
to give time to adapt to the authors of a dozen libraries
that were implicitly using @cl{:latin1}, out of over 700 in @(Quicklisp),
compared to a hundred that implicitly used @cl{:utf-8}
and the rest plain ASCII.
This default changed to @cl{:utf-8} one year later,
with the pre-release of @(ASDF3),
under the theory that
@moneyquote{it's good practice to release
                 all small backward-incompatible changes
                 together with a big one},
since that's the time users will have to pay attention, anyway.
Though it did break a few libraries that were still unmaintained after a year,
the new defaults actually made things more reliable for many other libraries,
as witnessed by the automated testing tool @tt{cl-test-grid}. @XXX{Give URL!}

Because we had learned that a feature isn't complete until it's tested,
we published library to demonstrate how to put this new infrastructure to good use:
@cl{lambda-reader}, a utility that lets you use the unicode character @cl{λ}
instead of @cl{lambda} in your code.@note{
  And yes, it does feel good to write @cl{λ} this way,
  and it does improve code that uses higher-order functions.
  My @tt{.emacs} has a @tt{(global-set-key "\C-cl" "λ")},
  and my @tt{.XCompose} has @tt{<Multi_key> <period> <backslash> : "λ" U03BB}.
}
Originally based on code originally by Brian Mastenbrook,
it was modified to fall back gracefully to working mojibake
on implementations that do not support Unicode, and
to offer the syntax modification via the @(de_facto) standard
@cl{named-readtables} extension.
Users still had to enable the modified syntax
at the beginning of every file,
then carefully disable it at the end of the file,
least they cause havoc in other files and/or in the user's environment.

@subsubsection{Hooks around compilation}

A recurrent question to @(ASDF) developers was about how to properly
modify the @(CL) syntax for some files,
without breaking the syntax for other files:
locally giving short nicknames to packages,
changing the readtable, or the reader function, etc.

The answer in 2011 was that it possible to define
a new subclass @cl{my-cl-file} of @cl{cl-source-file},
and then a method on @cl{perform :around ((o compile-op) (c my-cl-file))},
that would wrap the processing of the function inside a context
where syntax was modified.
But not only was it a cumbersome interface, it had the annoying corner case
of having to also define a method for the seldom used operation @cl{load-source-op},
and for any future such imaginable operation involving reading the file.

A better, more declarative interface was desirable,
and implemented in @(ASDF 2.018) (October 2011):
each component could define an option @cl{:around-compile},
or inherit it from its parent module or system,
that could designate a function that, when defined,
would be called around the compilation.
If @cl{nil} is explicitly specified, the function inherited from the system
will not be called, which is usually a necessity in the first few files of a system,
before said function was defined.

Actually, the function usually cannot be named by a symbol,
because at the time the @tt{.asd} file is read, none of the code has been compiled,
and the package in which the symbol will be interned doesn't exist yet;
therefore, @(ASDF 2.019) (November 2011) made it possible
to designate a function by a string that will be @cl{read} later.
Hence, for instance, systems defined in Stelian Ionescu's IOLib,@note{
  IOLib is a comprehensive general purpose I/O library for @(CL),
  written by Stelian Ionescu, that strives at doing the Right Thing™
  where many other libraries sacrifice code quality,
  feature coverage or portability for the sake of expediency.
}
use @cl{:around-compile "iolib/asdf:compile-wrapper"},
except for the system @cl{iolib/asdf} itself,
that defines these package and function.

@subsubsection{Enforcing user-defined invariants}

Another related feature, added in @(ASDF 2.23) (July 2012),
was the ability for users to define invariants to be enforced by @(ASDF)
when compiling their code:
a file might be compliant @(CL) code, and compile correctly,
yet failed to satisfy application-specific invariants
essential to the robustness of the application.
Without the build system checking after every file's compilation,
the user would be left with an invalid system,
and after he eventually gets a runtime error,
would have to chase which of thousands of files broke the invariant.
Thanks to the @cl{:compile-check} feature,
an @cl{:around-compile} hook could tell @(ASDF) to check the invariant
before it would accept a compilation output
that would otherwise poison future builds
(see @secref{Robustness} above about poisoned builds).

There were two notable use cases at ITA Software.
In the simpler one, the error logging infrastructure
was registering at compile-time all strings that could be seen by end-users,
to build a database that could be localized to another language,
as per legal requirements of the customer.
But it was not enough to do the registering at compile-time,
because unless you were building everything from scratch in the same process,
the compile-time state was lost before the final build image was dumped;
and it was not possible to do the registering as part of the macro's expansion,
because this expansion was not for code to be evaluated at the toplevel,
but only for code called conditionally, in exceptional situations.
One solution would have been to side-effect external files;
a better solution was for the macro to defer registration
to a cleanup form to be evaluated at the toplevel before the end of the file's compilation.
But since there is no standard mechanism to achieve this effect,
this required users to explicitly include a @cl{(final-forms)}
at the end of their file.
Now, users are prone to forgetting to include such a statement,
when they are aware at all that they need to.
But thanks to the new @cl{:compile-check} feature,
the system could automatically check the invariant that
no deferred form should be left dangling without a @cl{final-forms},
and reject the file with a helpful error message
instructing the programmer to insert said form.
The @cl{asdf-finalizers} provides such an infrastructure of
@cl{eval-at-toplevel} to evaluate a form and defer it for later inclusion at the top-level,
and @cl{final-forms} to include all registered such forms at the top-level;
user code would then specify in their @cl{defsystem}
the @cl{:around-compile "asdf-finalizers:check-finalizers-around-compile"} hook
for @(ASDF) to enforce the invariant.

The more complex use case was similarly solved with @cl{asdf-finalizers}.
Our data schema included hundreds of parametric types
such as @cl{(list-of passenger)} of @cl{(ascii-string 3 5)}
(for strings of ASCII characters length between 3 and 5).
Checking that data verified the proper invariants
before to insert corrupted data records in the database
or to message them to partners was an essential robustness feature.
But to define the type via the @(CL) @cl{deftype} mechanism,
these types had to expand to things like
@cl{(and list (satisfies list-of-passenger-p))},
where the predicate function @cl{list-of-passenger-p}
could not be provided additional parameters,
and had to be independently defined by a form @cl{(declare-list-of passenger)};
there again, this form could not be part of the type expansion,
and was not enough to @cl{eval}uate at compile-time,
but had to be explicitly included at the top-level.
Manually managing those forms was a maintenance burden,
and @cl{asdf-finalizers} eliminated this burden.

The principle we recognized was that
@moneyquote{every large enough application is a Domain-Specific Language with its own invariants,
                  and the programming language is but the implementation language of the DSL}.
This implementation will be extremely fragile
if it cannot automatically enforce the invariants of the DSL.
A good programming language will let you define new invariants,
and a good build system will enforce them.
In @(CL), this can all happen without leaving the language.

@subsubsection{The end of @(ASDF2)}

@;https://bugs.launchpad.net/asdf/+bug/479522   wanted: recompile system when dependency changes
@;https://bugs.launchpad.net/asdf/+bug/627173   asdf doesn't recompile when .asd file has changed
@;https://bugs.launchpad.net/asdf/+bug/656450   Forcing logic is baked into traverse
@; This was spawned from 479522 after 2.26.8:
@;https://bugs.launchpad.net/asdf/+bug/1087609  failure to check timestamps of dependencies

The @(ASDF2) series culminated with @(ASDF) 2.26,
after a few months when the few changes were all
portability tweaks, fixes to remote corner cases, or minor cleanups.
Only one serious bug remained in the bug tracker,
with maybe two other minor bugs;
all of them were bugs as old as @(ASDF) itself,
related to the @tt{traverse} algorithm that walks the dependency tree.

That algorithm had been inherited essentially unchanged from the early days of @(ASDF1),
with only some superficial bugs fixed, and
no one dared change the essentials of it, because
no one really understood what it was doing, why it was doing it,
and why that did or didn't work — and the original author was long gone,
and not available to answer questions.
However, its spaghetti implementation had been refactored and broken up
into smaller, more understandable function while fixing bugs for @(ASDF2),
and now at least it was clear what it was doing, if not why that was right or wrong;
and since that was the "last" bug, and there seemed nothing better to do,
the bug was investigated... and that opened a Pandora's Box of bigger issues.
Fixing one issue led to another, etc., which resulted in...


@subsection{@(ASDF) 3: A Mature Build}

@(ASDF3) was a complete rewrite of @(ASDF), several times over,
to correctly deal with its core issues.
The unintended result of these rewrites was to turn it into
a much more robust and versatile product than it was:
not only does it cover the robust building of @(CL) software from @(CL),
it also includes runtime software management functionality
and integration both ways with the Unix command line.

@(ASDF3) was pre-released as 2.27 in February 2013,
then officially released as 3.0.0 on May 15th 2013.

@subsubsection{Proper Timestamp Propagation}

to correctly compute timestamps
across an arbitrary action graph;
users may specify how operations do or don't propagate
along the component hierarchy.

@subsubsection{A Consistent, Extensible, Model}


@subsubsection{Understandable Internals}

Before a deep refactoring changing how things were done was possible,
a more shallow refactoring was needed:
reorganizing the source code so that it becomes understandable,
by cutting it in smaller chunks, each with clear dependencies to other chunks.

@subsubsection{Portability Layer}

@(ASDF3) notably brought a user-visible portability layer UIOP.
Abstracting over pathnames, filesystem access;
but also run-program, Lisp image lifecycle, and many other everyday concerns.

@cl{inferior-shell}, that builds upon @cl{uiop:run-program}, can:
@clcode{
  (in-package :inferior-shell)
  (run `(pipe (ps fauwwwwwwwwwwwwx) (sort -r -k5)))
}

@subsubsection{extensibility}

Better operations.

@subsubsection{configurability}

better supported are conditional compilation, versioning, and more.

@subsubsection{internal consistency}

@subsubsection{creating standalone executables}

there are several ways to deliver software in a single-file "bundle",
including a single FASL for a system and/or its dependencies, or
(on supported implementations) a standalone executable programs;

@subsubsection{cl-launch}

Running Lisp code to portably create executable commands from Lisp is great,
but there is a bootstrapping problem: when all you have is the command line,
how are you going to portably invoke the Lisp code that creates a command,
to begin with?

This bootstrapping problem was solved some years ago with @(cl-launch),
a bilingual program in a mix of portable shell script and @(CL),
that provides a nice shell command interface to
building shell-executable commands from Lisp code,
including delivery as either portable shell scripts or
self-contained precompiled executable files.

In its latest incarnation, @(cl-launch 4) (March 2014),
it was updated to take full advantage of @(ASDF3),
its build specification interface was made more general,p
and its Unix integration was improved.

You thus may directly invoke Lisp code from the shell command-line:
@verbatim|{
cl -sp lisp-stripper \
   -i "(print-loc-count \"asdf.lisp\")"
}|

It can also be used as a script "interpreter",
except with a Lisp compiler underneath, where desired:
@verbatim|{
#!/usr/bin/cl -sp lisp-stripper -E main
(defun main (argv)
  (if argv
      (map () 'print-loc-count argv)
      (print-loc-count *standard-input*)))
}|

In the examples above, option @tt{-sp}, shorthand for @tt{--system-package},
simultaneously loads a system using @(ASDF) during the build phase,
and appropriately selects the current package;
@tt{-i}, shorthand for @tt{--init} evaluates a form after the software is built;
@tt{-E}, shorthand for @tt{--entry} evaluates after the software is built
a form that calls specified function with the command-line arguments.
As for @tt{lisp-stripper}, it's a simple library that counts lines of code
after removing comments, blank lines, docstrings, and multiple lines in strings.

@(cl-launch) will automatically detect an @(CL) implementation
that is installed on your machine, with sensible defaults.
You can easily override the defaults with a proper command-line option,
a configuration file, or some installation-time configuration.
See @tt{cl-launch --more-help} for complete information.
Note that @(cl-launch) is on a bid to homestead the executable path
@tt{/usr/bin/cl} on Linux distributions, but may slightly more portably
be invoked as @tt{cl-launch}.

A nice use of @(cl-launch) is to compare how various implementations
evaluate some form, to see how portable it is in practice,
whether the standard mandates a specific result or not:
@verbatim|{
for l in sbcl ccl clisp cmucl ecl abcl \
         scl allegro lispworks gcl xcl ; do
 cl-launch -l $l -i \
 '(format t "'$l': ~S~%" `#5(1 ,@`(2 3)))' \
 2>&1 | grep "^$l:" # LW, GCL are verbose
done
}|

@(cl-launch) compiles all the files and systems that are specified to be run,
and keeps the compilation results in the same output-file cache as @(ASDF3),
likewise nicely segregated by implementation, ABI, version, etc.@note{
  Historically, it is more accurate to say that @(ASDF) imported
  the cache technology previously implemented by @(cl-launch),
  which itself was reusing a technique popularized by
  Debian's @tt{common-lisp-controller} as far back as 2002,
  and by the more widely portable @tt{asdf-binary-locations} after it.

  By defining an @cl{:around} method for the @tt{output-files} function,
  it was possible for the user to control where @(ASDF)
  would store its compilation output,
  without the authors of @(ASDF) having had to create an explicit hook.
  It is unclear who first came up with the idea,
  but the fact that this technique could be developed
  as an obvious extension to @(ASDF)
  without the author explicitly designing the idea into it,
  and without having to modify the source code,
  is an illustration of how expressive and modular the @(CL) Object System can be.

  Now, @tt{asdf-binary-locations} and other variants of the same idea
  had a bootstrapping issue: the extension had to be specially loaded
  before it could be used, whether as source code or precompiled code,
  otherwise the potential clashes regarding its own compiled file
  would negate any advantage in avoiding clashes for files of other systems.
  @tt{common-lisp-controller} was trying to solve the same issue
  by having managed software installations in a controlled way;
  it failed eventually, because these efforts were only available
  to @(CL) programmers using Debian or a few select other Linux software distributions,
  and only for the small number of slowly updated libraries,
  making the maintainers a bottleneck in a narrow distribution process.
  @tt{common-lisp-controller} also attempted to manage a system-wide cache of compiled objects,
  but this was ultimately abandoned for security issues;
  a complete solution would have required a robust and portable build service,
  which was much more work than justified by said narrow distribution process.
  The solution in @(ASDF2) was to merge this functionality in @(ASDF) itself,
  yielding its @tt{output-translations} layer,
  according to the principle of making everything as simple as possible,
  @emph{but no simpler}.
  As another, more successful take on the idea of @tt{common-lisp-controller},
  Zach Beane eventually wrote @(Quicklisp), that uses @(ASDF2),
  and makes deployment of software ubiquitous by not requiring a particular system,
  but working in any user's environment on any system,
  avoiding the hard problem of sharing rather than failing at it.
}
Therefore, the first time it sees a given file or system,
or after they have been updated, there may be a startup delay
while the compiler processes the files;
but subsequent invocations will be faster as the compiled code is directly loaded.
This is in sharp contrast with other "scripting" languages,
that have to slowly interpret or recompile everytime.
For security reasons, the cache is not shared between users.

@subsection{@(ASDF) 3.1: Lifting Lisp}

@(ASDF 3.1) (March 2014) builds on top of @(ASDF3) to provide new features.

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
*load-system-operation*,
deferred-warnings, unconstrained versions, build-op
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
we could thus notably @moneyquote{reduce the cognitive load on users} for all future systems.
No more need to learn complex syntactic and semantic constraints
and even more complex tricks to evade those constraints.

@subsection{Overriding the default pathname extension}

Back in the bad old days of @(ASDF1),
the official recipe, described in the manual,
to override the default pathname type @tt{lisp} for Lisp source file to
e.g. @tt{cl}, used to be:

@clcode{
(defmethod source-file-type
   ((c cl-source-file) (s (eql (find-system 'my-sys))))
   "cl")
}

Another recipe that got some following instead encouraged
defining a new subclass of @cl{cl-source-file}
and specializing a method on that class and @cl{module},
which caused much grief when we tried to make @cl{system}
not a subclass of @cl{module} anymore,
but both be subclasses of @cl{parent-component} instead.

In @(ASDF) 2.015, two new subclasses of @tt{cl-source-file} were introduced,
@tt{cl-source-file.cl} and @tt{cl-source-file.lsp},
that provide the respective types @tt{cl} and @tt{lsp},
which covers the majority of systems that don't use @tt{lisp}.
Users need simply add to their @cl{defsystem} the option
@cl{:default-component-class :cl-source-file.cl}
and files will have the specified type.
Individual modules or files can be overridden, too,
either by changing their class from @cl{:file} to @cl{:cl-source-file.cl},
or more directly by specifying a @cl{:pathname} parameter.

In any case, the protocol was roundabout both for users and implementers,
and a new protocol was invented.
Verbosity is a bad smell.

@subsection{Syntax Control}

Guy Steele has been quoted
as vaunting the programmability of Lisp's syntax by saying:
@emph{If you give someone Fortran, he has Fortran.
If you give someone Lisp, he has any language he pleases.}
Unhappily, if he were speaking about @(CL) specifically,
he would have had to add:
@emph{but it can't be the same as any one else's}.
Indeed, syntax in @(CL) is controled via
a fuzzy set of global variables,
and making non-trivial modifications to the content of these variables,
notably the @cl{*readtable*}, is both possible and frowned upon,
because if such modifications escape their intended scope,
they can cause unexpected breakage in unrelated parts of the system,
written by different people, and there is
Even though the Lisp ideal is one of ubiquitous syntax extension,
and indeed extension through macros is ubiquitous,
extens

@moneyquote{any language feature has to be safe before it may be ubiquitous}.


Now, other Lisp dialects, such as Racket,
have succeeded at making syntax customization both safe and ubiquitous,
by having it be strictly scoped to the current file or REPL.

Now, @(ASDF3)

Due to build issues with managing the global variables
that control syntax, he has any language he pleases,
Systems like @cl{named-readtables} are making it easier
to manage local syntax modification,
but cooperation from the build system is necessary
before syntax customization may become safe; and

With @(ASDF3), we tried to impose stricter rules on syntax modifications,
but that broke too many systems, and



@section{Conclusion: Lessons for Language Growers}

The CL standard leaves many things underspecified about pathnames,
in an effort to define a useful subset common to many existing implementations.
However, the result is that portable programs can forever only access
but a small subset of the complete required functionality,
making it standard less useful than if it had not specified anything,
and left the job to another standard.
The lesson is @emph{don't standardize partially specified features}.
Instead, @moneyquote{do delegate to existing or future standards}.
Better have pathname protocol per operating system,
and let libraries sort out a portability layer over N operating systems,
than have one pathname protocol per implementation per operating system,
and now libraries have to take into account N*M combinations
of operating systems and implementations.

The general problem with @(CL) is that
its semantics are defined in terms of
@emph{irreversible side-effects to a global environment}.
A better principle would be to
@moneyquote{define a programming language's semantics in terms of
                   pure transformations with local environments}.

@(generate-bib)

@section[#:tag "Pathnames" #:style (make-style 'appendix '(unnumbered))]{Appendix A: Pathnames}
