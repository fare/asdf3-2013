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
that can access and "glue" together
functionality from the underlying system or external programs —
except this time in a language with reasonable semantics,
efficient implementations, and extensible syntax.
We describe the most salient improvements in @(ASDF)
and how they enable previously difficult and portably impossible
uses of the programming language.
We discuss past and future challenges
in improving this key piece of software infrastructure,
and what approaches did or didn't work
in bringing change to the @(CL) community.
}

@section[#:style (make-style 'introduction '(unnumbered))]{Introduction}

Better late than never,
with the release of @(ASDF3) in May 2013,
all the programs for which one uses so-called "scripting" languages
may now be written @emph{portably} in @(CL) (CL):@note{
  CL is a language defined in the ANSI standard X3.226-1994.
  It is a multi-paradigm, dynamically-typed high-level language.
  Though it is known for its decent support for functional programming,
  its support for Object-Oriented Programming
  is what remains unsurpassed still in many ways,
  and sadly few languages even attempt to match its syntactic extensibility
  or support for interactive development.
  It was explicitly designed to allow for high-performance implementations;
  some of them, depending on the application,
  may rival compiled C programs in terms of speed,
  usually far ahead of "scripting" languages and their implementations.
@XXX{
  Over a dozen actively maintained implementations
  purport to conform to the ANSI CL standard, plus a few unmaintained ones.
  No single implementation is at the same time
  the best, shiniest, leanest, fastest, cheapest,
  and the one ported to the most platforms.
  While it is possible to write useful programs
  using only the standardized parts of the language,
  fully taking advantage of extant libraries
  that harness modern hardware and software techniques
  requires the use of various extensions.
  Happily, each implementation provides its own extensions
  and there exist libraries to abstract over
  the discrepancies between these implementations
  and provide portable access to threads (@cl{bordeaux-threads}),
  unicode support (@cl{cl-unicode}),
  a "foreign function interface" to libraries written in C (@cl{cffi}),
  ML-style pattern-matching (@cl{optima}), etc.
  A software distribution system, @(Quicklisp),
  makes it easy to install hundreds of such libraries,
  that were already using @(ASDF) as a basic software organization mechanism.
  The new features in @(ASDF3) were only the last missing pieces in this puzzle.
}}
one can write small scripts that glue together functionality provided
by the operating system, external programs, C libraries, or network services;
one can scale them seamlessly into large, maintainable, modular, systems;
and one can make those new services available to other programs via the command-line
as well as via network protocols, etc.

@(ASDF) has been the @(de_facto) standard build system
for portable CL software since shortly after its release
by Dan Barlow in 2002 @~cite[ASDF-Manual].
@moneyquote{The purpose of a build system is
                to enable division of labor in software development}:
source code is organized in separately-developed components
that depend on other components,
and the build system transforms the transitive closure of these components
into a working program.

@(ASDF3), the latest rewrite of the system,
beside fixing numerous bugs,
sports a portability layer, @(UIOP),
that makes portable scripting possible.
It enables the writing of Lisp programs
that may be invoked from the command line
or may spawn external programs and capture their output.
It allows delivering these programs as standalone executable files;
moreover the companion script @cl{cl-launch} (see @secref{cl-launch})
can create light-weight scripts that can be run unmodified
on many different kinds of machines, each differently configured.
Previously, key parts of a program had to be configured to match
one's specific CL implementation and specific software installation paths.
Now, all of one's usual Unix scripting needs can be entirely fulfilled by CL,
benefitting from its efficient implementations, hundreds of software libraries, etc.

In this article, we will describe
the various achievements and failures of @(ASDF)'s development,
and try to extract lessons from them.
To make things easier to programmers
who may or may not have used the tool at some point,
we break down improvements by major milestones.
For each improvement, we provide a rationale for the change and
a simple example of how it enables
better programming practices or more powerful programs.

In @secref{what_it_is}, we explain what @(ASDF) is about;
we will compare it to what is done in the C world;
we will lay down the historical context in which it was designed;
we will tell what it initially brought to the CL community.

In @secref{asdf2},
we will describe the innovations introduced with @(ASDF "2.000"),
and how they made @(ASDF) reliable.

In @secref{asdf2.26},
we will describe the features added during the days of @(ASDF2),
until version 2.26, to make it more usable.

In @secref{asdf3},
we will describe the innovations introduced in @(ASDF3)
and @(ASDF3.1), to encompass software delivery.

In @secref{evolving}, we will discuss the challenges
of making acceptable changes to a piece of community software,
and conclude with lessons that growers of other programming languages
can learn from our experience with CL and @(ASDF).

@section[#:tag "what_it_is"]{What @(ASDF) is}

@subsection{@(ASDF): Basic Concepts}

@subsubsection{Components}

@(ASDF) is a build system for CL:
it helps developers divide software into a hierarchy of @bydef{component}s
and automatically generates a working program from all the source code.

Top components are called @bydef{system}s in an age-old Lisp tradition,
while the bottom ones are source files, typically written in CL.
Users may then @(operate) on these components with various build @bydef{operation}s,
most prominently compiling the source code (operation @(compile-op)) and
loading the result into the current Lisp image (operation @(load-op)).

Several related systems may be developed together
in the same source code @bydef{repository}.
Each system may depend on code from other systems,
either from the same repository or from a different repository.
@(ASDF) itself has no notion of repositories,
but other tools on top of @(ASDF) do:
@(Quicklisp) @~cite[quicklisp] packages together
systems from a repository into a @bydef{release},
and provides hundreds of releases as a @bydef{distribution},
automatically downloading on demand
required systems and all their transitive dependencies.

A system is itself organized into a hierarchy of components,
where each component may be either an individual @bydef{file}
(usually, a CL source file),
or a @bydef{module} that may recursively contain other components.
Modules may or may not directly fit the filesystem directory hierarchy.

Further, each component may explicitly declare
a @bydef{dependency} on other components:
whenever a component relies at compile-time or load-time
on declarations or definitions of packages, macros, variables, classes, functions, etc.,
present in another component, the programmer must
declare that the former component @(depends-on) the latter.

@subsubsection{Example System Definitions}

For instance, here is how the @cl{fare-quasiquote} system is defined
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
    (test-op "fare-quasiquote-test"))))
}

Notice how it @(depends-on) another system, @cl{fare-utils},
a collection of utility functions and macros from another repository,
whereas testing is specified to be done by @cl{fare-quasiquote-test},
a system defined in a different file, @cl{fare-quasiquote-test.asd},
within the same repository.
It otherwise contains three files, @tt{packages.lisp},
@tt{quasiquote.lisp} and @tt{pp-quasiquote.lisp}
(the @tt{.lisp} suffix is automatically added based on the component class;
see @secref{pathnames}).
The latter files each depend on the first,
that which the CL packages@note{
  Each CL image has a global flat two-level namespace of symbols in packages:
  packages are identified by their name, a string;
  in each package, symbols are identified by their name, a string.
  However, this namespace is not global across images,
  because packages can import symbols from other packages,
  but without renaming,
  and CL between processes running different code bases
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

This illustrates the use of modules:
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

@subsubsection{Action Graph}
@; TODO: add a graph!

Building software is modeled as a Direct Acyclic Graph (DAG) of @bydef{action}s.
Each action is a pair of an operation and a component.
The DAG defines a partial order, whereby each action must be @bydef{perform}ed,
but only after all the actions it (transitively) depends on have already been performed.

For instance, in @cl{fare-quasiquote} above,
the @emph{loading} of (the compilation output of) @tt{quasiquote.lisp}
@(depends-on) the @emph{compiling} of @tt{quasiquote.lisp},
which itself depends-on
the @emph{loading} of (the compilation output of) @tt{package.lisp}, etc.

Importantly, though, this graph is distinct
from the preceding graph of components:
the graph of actions is not a mere refinement of the graph of components
but a transformation of it that also incorporates
crucial information about the structure of operations.

Unlike its immediate predecessor @(mk-defsystem),
@(ASDF) makes a @bydef{plan} of all actions needed
to obtain an up-to-date version of the build output
before it @bydef{performs} these actions.
In @(ASDF) itself, this plan is a list of actions to be performed sequentially:
a total order that is a linear extension of the partial order of dependencies;
performing the actions in that order ensures that
the actions are always performed after the actions they depend on.

It is of course possible to reify the complete DAG of actions
rather than a linear extension of its implied order.
Indeed, Andreas Fuchs in 2006 wrote a very small but quite brilliant @(ASDF) extension
called @(POIU), the "Parallel Operator on Independent Units".
@(POIU) compiles files in parallel on Unix multiprocessors using @tt{fork},
while still loading them sequentially into a main image, minimizing latency.
François-René Rideau later rewrote @(POIU), making it
both more portable and simpler by co-developing it with @(ASDF).
Understanding the many clever tricks
by which Andreas Fuchs overcame the issues with the @(ASDF1) model
to compute such a complete DAG led to many aha moments,
instrumental when writing @(ASDF3) (see @secref{traverse}).

@subsubsection{In-image}

Last but not least,
@moneyquote{@(ASDF) is an @q{in-image} build system},
just like the build systems that preceded it in the Lisp @(defsystem) tradition:
it compiles (if necessary) and loads software into the current CL image.
For better or worse, this notably differs from common practice in most other languages,
where the build system is a completely different piece of software running in a separate process.@note{
  Of course, a build system could compile CL code in separate processes,
  for the sake determinism and parallelization:
  our XCVB did @~cite[XCVB-2009]; so does Google's blaze.
  @; As for the wide variety of Lisp dialects beside CL,
  @; they have as many different build systems, often integrated with a module system.
}
On the one hand, it minimizes overhead to writing build system extensions.
But on the other hand, it puts great pressure on @(ASDF) to remain minimal.

Qualitatively, @(ASDF) must be delivered as a single source file
and cannot use any external library,
since it itself defines the code that may load other files and libraries.
Quantitatively, @(ASDF) must minimize its memory footprint,
since it is present in all programs that are built,
and any resource spent is spent by all.@note{
  This arguably mattered more in 2002 when @(ASDF) was first released
  and was about a thousand line long:
  By 2014, it has grown over ten times in size,
  but memory sizes have increased even faster.
}

Still, for all these reasons, @(ASDF) follows the minimalist principle that
@moneyquote{anything that can be provided as an extension
            should be provided as an extension and left out of the core}.
Thus it cannot afford to support a persistence cache
indexed by the cryptographic digest of build expressions,
or a distributed network of workers, etc.
However, these features are conceivable as @(ASDF) extensions.

@subsection{Comparison to C programming practice}

Most programmers are familiar with C, but not with CL.
It is therefore worth constrasting @(ASDF) to the tools used by C programmers
to provide similar services.

To build and load software, C programmers typically use
@(make) to build the software and @tt{ld.so} to load it.
Additionally, they use a tool like @tt{autoconf}
to locate available libraries and identify their features.@note{
  @(ASDF3) also provides functionality which would correspond
  to small parts of the @tt{libc} and of the linker @tt{ld}.
}
In many ways these C solutions are
vastly better engineered than @(ASDF).
But in other important ways @(ASDF) demonstrates how
these C systems have a vast amount of accidental complexity
that CL does away with thanks to better architecture.

@itemlist[
  @item{
    Lisp makes the full power of runtime available at compile-time,
    so it's easy to implement a Domain-Specific Language (DSL):
    only the new functionality needs be defined,
    as an extension that seamlessly combines
    with the rest of the language, including other extensions.
    In C, instead, every utility needs to onerously grow
    an entire domain-specific language from scratch;
    since the domain expert is seldom also language expert
    with resources to do that right,
    this means plenty of mutually incompatible, misdesigned,
    power-starved, misimplemented languages that have to be combined
    through an unprincipled chaos of
    expensive and unexpressive means of communication.
  }
  @item{
    Lisp provides full introspection at runtime and compile-time alike,
    as well as a protocol to declare @bydef{features}
    and conditionally read code and data based on them.
    Therefore you don't need dark magic at compile-time
    to detect the features available.
    In C, instead, people resort to
    horribly unmaintainable configuration scripts
    in a hodge podge of shell script, m4 macros, C preprocessing and C code,
    plus often bits of python, perl, sed, etc.
  }
  @item{
    @(ASDF) possesses a standard and standardly extensible way to configure
    where to find the libraries your code depends on, further improved in @(ASDF2).
    In C, then are tens of incompatible ways to do it,
    between @tt{libtool}, @tt{autoconf}, @tt{gconf}, @tt{kde-config},
    various manual @tt{./configure} scripts, and countless other protocols,
    so that each new piece of software requires
    to learn a new ad hoc configuration method,
    making it an expensive endeavour to use and/or distribute libraries.
  }
  @item{
    @(ASDF) uses the very same mechanism
    to configure both runtime and compile-time,
    so there is only one configuration mechanism to learn and to use,
    and no risk of discrepancy between the two.@note{
      There is still discrepancy @emph{inherent} with these times being distinct,
      with the opportunity for the installation to have changed,
      or for a program being run on a different machine.
    }
    In C, completely different and incompatible mechanisms are used
    at runtime (@tt{ld.so}) and compile-time (unspecified),
    which further makes it hard to match
    source code, compilation headers, static and dynamic libraries,
    requiring some huge and complex "software distribution" infrastructure
    (that admittedly also manages versioning, downloading and precompilation),
    and causing very hard to understand bugs when subtle discrepancies creep in.
  }
]

Nevertheless, there are also many ways in which @(ASDF) pales in comparison
to other build systems for CL, C, Java, or other systems:

@itemlist[
  @item{
    @(ASDF) is not a general-purpose build system.
    Its relative simplicity is directly related to it being custom made
    to build CL software only.
    Seen one way, it's a sign of how little you can get away with
    if you have a good basic architecture;
    a similarly simple solution is not available to most other programming languages,
    that require much more complex tools to achieve a similar purpose.
    Seen another way, it's also the CL community failing to embrace
    the outside world and provide solutions with enough generality
    to solve more complex problems.@note{
      @(ASDF3) could be easily extended to support arbitrary build actions,
      if there were an according desire. But @(ASDF1) and 2 couldn't:
      their action graph was not general enough,
      being simplified and tailored for the common use case
      of compiling and loading Lisp code;
      and their ability to call arbitrary shell programs
      was a misdesigned afterthought (copied over from @(mk-defsystem))
      the implementation of which wasn't portable, with too many corner cases.
    }
  }
  @item{
    At the other extreme, a build system for CL could have been made
    that is much simpler and more elegant than @(ASDF),
    if it could have required software to follow some simple organization constraints,
    without much respect for legacy:
    a constructive proof of that is Alastair Bridgewater's @(quick-build)
    (or the similar and earlier @(faslpath) by Peter Etter),
    being a fraction of the size of the original @(ASDF), which is a fraction of @(ASDF3)'s,
    and with a fraction of the bugs — but none of the generality and extensibility
    (See below @secref{asdf-package-system}).
  }
  @item{
    Because of its age-old model of building software in-image, what more
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
    if that alternative is written in CL.
  }
]

@subsection{DEFSYSTEM before @(ASDF)}

Ever since the late 1970s, Lisp implementations
have each been providing their variant of the original
Lisp Machine @(DEFSYSTEM) @~cite[CHINE-NUAL].
These build systems allowed users to define @bydef{systems},
units of software development made of many @bydef{files},
themselves often grouped into @bydef{modules};
many @bydef{operations} were available to transform those systems and files,
mainly to compile the files and to load them,
but also to extract and print documentation,
to create an archive, issue hot patches, etc.;
@(DEFSYSTEM) users could further declare dependency rules
between operations on those files, modules and systems,
such that files providing definitions
should be compiled and loaded before files using those definitions.

Since 1990, the state of the art in free software CL build systems
was @(mk-defsystem) @~cite[MK-DEFSYSTEM].@note{
  The variants of @(DEFSYSTEM) available
  on each of the major proprietary CL implementations
  (Allegro, LispWorks, and formerly, Genera),
  seem to have been much better than @(mk-defsystem).
  But they were not portable, not mutually compatible, and not free software,
  and therefore @(mk-defsystem) became @(de_facto) standard for free software.
  @; XXX what about the ILU defsystem from Xerox Parc?
}
Like late 1980s variants of DEFSYSTEM on all Lisp systems,
it featured a declarative model to define a system in terms of
a hierarchical tree of @bydef{components},
with each component being able to declare dependencies on other components.
The many subtle rules constraining build operations
could be automatically deduced from these declarations,
instead of having to be manually specified by users.

However, @(mk-defsystem) suffered from several flaws,
in addition to a slightly annoying software license.
These flaws were probably justified at the time it was written,
several years before the CL standard was adopted,
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
which would be a lot of work, yet eventually would probably still break
the support for now obsolete implementations that couldn't be tested anymore.

@subsection{@(ASDF1): A Successful Experiment}

In 2001, Dan Barlow, a then prolific CL hacker,
dissatisfied @(mk-defsystem),
wrote a new @(defsystem) variant, @(ASDF).@note{
  In a combined reverence for tradition and joke,
  @(ASDF) stands for "Another System Definition Facility",
  as well as for consecutive letters on a QWERTY keyboard.
}
Thus he could abandon the strictures of supporting long obsolete implementations,
and instead target modern CL implementations.
In 2002, he published @(ASDF), made it part of SBCL,
and used it for his popular CL software.
It was many times smaller than @(mk-defsystem)
(under a thousand line of code, instead of five thousand),
much more usable, easy to extend,
trivial to port to other modern CL implementations,
and had an uncontroversial MIT-style software license.
It was an immediate success.

@(ASDF) featured many brilliant innovations in its own right.

Perhaps most importantly as far as usability goes,
@(ASDF) cleverly used the @cl{*load-truename*} feature of modern Lisps,
whereby programs (in this case, the @(defsystem) form)
can identify from which file they are loaded.
Thus, system definition files didn't need to be edited anymore,
as was previously required with @(mk-defsystem),
since pathnames of all components could now be deduced
from the pathname of the system definition file itself;
furthermore, because the @cl{truename} resolved Unix symlinks,
you could have symlinks to all your Lisp systems
in one or a handful directories that @(ASDF) knew about,
and it could trivially find all of them.
Configuration was thus a matter of configuring @(ASDF)'s
@cl{*central-registry*} with a list of directories
in which to look for system definition files,
and maintaining "link farms" in those directories
— and both aspects could be automated.
(See below for how @(ASDF2) improved on that.)

Also, following earlier suggestions by Kent Pitman @~cite[Pitman-Large-Systems],
Dan Barlow used object-oriented style to make his @(defsystem) extensible
without the need to modify the main source file.@note{
  Dan Barlow may also have gotten from Kent Pitman
  the idea of reifying a plan then executing it
  rather than walking the dependencies on the go.
}
Using the now standardized @(CLOS) (CLOS),
Dan Barlow defined his @(defsystem) in terms of @bydef{generic functions}
specialized on two arguments, @(operation) and @(component),
using multiple dispatch, an essential OO feature unhappily not available
in lesser programming languages, i.e. sadly almost of them —
they make do by using the "visitor pattern".
Extending @(ASDF) is a matter of simply defining new subclasses
of @(operation) and/or @(component)
and a handful of new methods for the existing generic functions,
specialized on these new subclasses.
Dan Barlow then demonstrated such simple extension with his @cl{sb-grovel},
a system to automatically extract low-level details
of C function and data structure definitions,
so they may be used by SBCL's foreign function interface.

@subsubsection{Limitations of @(ASDF1)}

@(ASDF) was a great success at the time,
but after many years, it was also found to have its downsides:
Dan Barlow was experimenting with new concepts,
and his programming style was to write
@emph{the simplest code that would work in the common case},
giving him most leeway to experiment.
His code had a lot of rough edges:
while @(ASDF) worked great on the implementation he was using
for the things he was doing with it,
it often failed in ugly ways when using other implementations,
or exercising corner cases he had never tested.
The naive use of lists as a data structure
didn't scale to large systems with thousands of files.
The extensibility API while basically sane was missing many hooks,
so that power users had to redefine or override @(ASDF) internals
with modified variants, which made maintenance costly.

What more, there was a vicious circle preventing
@(ASDF) bugs from being fixed or features from being added @~cite[Software-Irresponsibility]:
Every implementation or software distribution (e.g. Debian) had its own version,
with its own bug fixes and its own bugs;
so developers of portable systems could not assume anything
but the lowest common denominator, which was very buggy.
On the other hand, because users were not relying on new features,
but instead wrote kluges and workarounds that institutionalized old bugs,
there was no pressure for providers to update;
indeed the pressure was to not update and risk be responsible for breakage,
unless and until the users would demand it.
Thus, one had to assume that no bug would ever be fixed everywhere;
and for reliability one had to maintain one's own copy of @(ASDF),
and closely manage the entire build chain:
start from a naked Lisp, then
get one's fixed copy of @(ASDF) compiled and loaded
before any system could be loaded.@note{
  It was also impossible to provide a well configured @(ASDF)
  without pre-loading it in the image;
  and it was impossible to upgrade @(ASDF) once it was loaded.
  Thus Debian couldn't reliably provide "ready to go" images
  that would work for everyone who may or may not need an updated @(ASDF),
  especially not with stability several years forward.
}
In the end, there was little demand for bug fixes,
and supply followed by not being active fixing bugs.
And so @(ASDF) development stagnated for many years.

@section[#:tag "asdf2"]{@(ASDF2): Productizing @(ASDF)}

In November 2009, François-René Rideau
took over @(ASDF) maintainership and development.
A first set of major changes led to @(ASDF2), released in May 2010.
The versions released by Dan Barlow and the maintainers who succeeded him,
and numbered 1.x are thereafter referred to at @(ASDF1).
These changes are explained in more detail in
our ILC 2010 article @~cite[Evolving-ASDF].

@subsection{Upgradability}

The first bug fix was to break the vicious circle
preventing bug fixes from being relevant.
We enabled hot upgrade of @(ASDF),
so that users could always load a fixed version
on top of whatever the implementation or distribution did or didn't provide.
@note{
  In @(ASDF3),
  some of the upgrade complexity described in our 2010 paper was done away with:
  even though @moneyquote{CL makes dynamic data upgrade extraordinarily easy}
  as compared to other languages, we found that it's not easy enough to maintain;
  therefore instead of trying hard to maintain that code,
  we "punt" and drop in-memory data if the schema has changed in incompatible ways;
  thus we do not try hard to provide methods for @cl{update-instance-for-redefined-class}.
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
  @moneyquote{CL support for hot upgrade of code may exist
                    but is anything but seamless}.
  These simpler upgrades allow us to simply use @cl{fmakunbound} everywhere,
  instead of having to @cl{unintern} some functions before redefinition.
}
Soon enough, users felt confident relying on bug fixes and new features,
and all implementations started providing @(ASDF2).

These days, you can @cl{(require "asdf")} on pretty much any CL implementation,
and start building systems using @(ASDF).
Most implementations already provide @(ASDF3).
A few still lag with @(ASDF2), or fail to provide @(ASDF);
the former can be upgraded with @cl{(asdf:upgrade-asdf)};
all but the most obsolete ones can be fixed
by an installation script we provide with @(ASDF3.1).

Upgradability crucially decoupled what @(ASDF) users could rely on
from implementations provided, enabling a virtuous circle of universal upgrades,
where previously everyone was waiting for others to upgrade, in a deadlock.
@moneyquote{Allowing for divergence creates an incentive towards convergence}.

@subsection{Portability}

A lot of work was spent on portability.@note{
  Portability is important, because depending on which
  specific task you're performing on which operating system, architecture, etc.,
  you'll want a different implementation.
  For instance, SBCL is quite popular for its runtime speed
  on intel-compatible Linux machines;
  but since it is slower at compiling and loading,
  it isn't the best choice for a quick script where fast startup matters;
  it also won't run on ARM, and doesn't have the best Windows support;
  and so you might prefer Clozure CL or another implementation,
  depending on your constraints.
}
Originally written for @tt{sbcl},
@(ASDF1) eventually supported 5 more implementations:
@tt{allegro}, @tt{ccl}, @tt{clisp}, @tt{cmucl}, @tt{ecl}.
Each implementation shipped its own old version, often slightly edited;
system definition semantics often varied subtly between implementations,
notably regarding pathnames.
@(ASDF) 2.000 supported 9 implementations, adding:
@tt{abcl}, @tt{lispworks}, @tt{gcl};
system definition semantics was uniform across platforms.
@(ASDF) 2.26 (last in the @(ASDF2) series) supported 15, adding:
@tt{cormanlisp}, @tt{genera}, @tt{mkcl}, @tt{rmcl}, @tt{scl}, @tt{xcl}.
Since then, new implementations are being released with @(ASDF) support:
@tt{mocl}, and hopefully soon @tt{clasp}.

@(ASDF) as originally designed would only reliably work on Unix variants
(Linux, BSD, etc., maybe cygwin, and now also MacOS X, Android, iOS).
It can now deal with very different operating system families:
most importantly Windows, but also the ancient MacOS 9 and Genera.

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
So great is the disaster of CL pathnames,
that they deserve their own appendix to this article.

Lisp programmers can now "write once, run anywhere",
as far as defining systems go;
but they still have to otherwise avoid non-standardized behavior
and implementation-specific extensions (unless hidden behind a portability layer)
if they want their programs to be portable
— @(ASDF) cannot solve these issues intrinsic to CL.

Portability decoupled which implementation and operating system
were used to develop a system from which it could be compiled with,
where previously any non-trivial use of pathnames, filesystem access
or subprocess invocation was a portability minefield.

@subsection{Configurability}

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
@bold{Each can specify what they know, none need specify what they don't}.

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
where @cl{*central-registry*} itself couldn't.
Software management programs at either user or system level
could thus update independent configuration files in a modular way
to declare where the installed software was located;
users could manually edit a file describing where they manually downloaded software;
users could export environment variables to customize or override
the default configuration with context-dependent information;
and scripts could completely control the process
and build software in a predictable, deterministic way;
it is always possible to take advantage of a well-configured system,
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

There used to be an extension to @(ASDF1) called @cl{asdf-binary-locations}
that fulfilled the same functionality;
but apart from its suffering
from the same lack of modularity as the @cl{*central-registry*},
it also had a chicken-and-egg problem:
you couldn't use @(ASDF) to load it without having at least one
program compiled without @cl{asdf-binary-locations} enabled,
namely @cl{asdf-binary-locations} itself;
it thus required special purpose loading and configuration
in whichever file did load @(ASDF), making it not modular at all.
This was resolved by moving the functionality into @(ASDF) itself,
illustrating the design principle:
@bold{make it as simple as possible, but no simpler}
— but whereas @(ASDF1) followed this principle under the constraint
that the simple case should be handled correctly,
@(ASDF2) updated the constraint to include handling all cases correctly.
Dan Barlow's weaker constraint may have been great for experimenting,
it was not a good one for a robust product.

Configurability decoupled use and installation of software:
multiple parties could now each modularly contribute some software,
whether applications, libraries or implementations,
and provide configuration for it without being required
to know configuration of other software;
previously, whoever installed software couldn't notify users,
and users had to know and specify configuration of all software installed.

@subsection{Robustness}

During the development of @(ASDF2) (then 3),
a great number of bugs were introduced, and even more bugs were fixed.

@(ASDF) used to pay no attention to robustness.
A glaring issue, for instance, was causing much aggravation in large projects:
killing the build process while a file was being compiled
would result in a corrupt output file that would poison further builds
until it was manually removed:
    @; https://bugs.launchpad.net/asdf/+bug/587889
@(ASDF) would fail the first time, then when restarted a second time,
would silently load the partially compiled file,
leading the developer to believe the build had succeeded when it hadn't,
and then to debug an incomplete system.
The problem could be even more aggravating,
since a bug in the program itself
could be causing a fatal error during compilation
(especially since in CL, developers can run arbitrary code during compilation).
The developer, after restarting compilation, might not see the issue;
he would then commit a change that others had to track down and painfully debug.
This was fixed by having @(ASDF) compile into a temporary file,
and move the outputs to their destination only in case of success, atomically.@note{
  Not all Lisp implementations and/or underlying operating systems
  allowed this replacement to be atomic.
  In the latest @(ASDF3), the function @cl{rename-file-overwriting-target}
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
@(ASDF2) followed the principle that code should either work of fail everywhere the same way,
and in the latter case,
@moneyquote{fail early for everyone rather than pass as working for some}
and fail mysteriously for others.
These two policies led to very robust code,
at least compared to previous CL build systems including @(ASDF1).

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
in case it doesn't strictly adhere to a portable subset of CL
(which is not automatically enforceable so far),
since the semantics of CL are not fully specified
but leave a lot of leeway to implementors, unlike e.g. ML or Java.

@subsection{Performance}

@(ASDF1) performance didn't scale well to large systems,
because Dan Barlow was using the @cl{list} data structure everywhere
while walking a system, leading to a polynomial increase of running time
as the size of systems increased.
It did scale reasonably well to a large number of small systems,
because it was using a hash-table to find systems;
but there was a dependency propagation bug in this case
(see @secref{traverse}).
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

@subsection{Usability}

Usability was an important concern while developing @(ASDF2).
Portability, Configurability, Robustness and Performance
already contribute to Usability,
as do all improvements to the software;
some changes were made, though, that were specifically introduced
to ease usability of @(ASDF).

As a trivial instance, the basic @(ASDF) invocation was the clumsy
@cl{(asdf:operate 'asdf:load-op :foo)} or
@cl{(asdf:oos 'asdf:load-op :foo)}.
With @(ASDF2), that would be the more obvious
@cl{(asdf:load-system :foo)},
and starting with @(ASDF3.1), we recommend
@cl{(asdf:build :foo)}.@note{
  @(load-system) was actually implemented
  by Gary King, the last maintainer of @(ASDF1), in June 2009;
  but users couldn't casually @emph{rely} on it being there
  until @(ASDF2) made it possible in 2010
  for everyone to hot upgrade whatever their implementation was providing.
}

@(ASDF2) provided a portable way to specify pathnames
by adopting Unix pathname syntax as an abstraction,
while using standard CL semantics underneath.
It became easy to specify hierarchical relative pathnames,
where previously doing it portably was extremely tricky.
@(ASDF2) similarly provided sensible rules for pathname types and type overrides.
(See @secref{pathnames}.)
@(ASDF) made it hard to get pathname specifications right portably;
@(ASDF2) @moneyquote{made it hard to get it wrong} or make it non-portable.

Usability decoupled the knowledge of how to use @(ASDF)
from both the knowledge of @(ASDF) internals and CL pathname idiosyncrasies.
Any beginner with a basic understanding of CL and Unix pathnames
could now use @(ASDF) to portably define non-trivial systems,
a task previously reserved to experts and/or
involving copy-pasting magical incantations.
The principle followed was that
@moneyquote{the cognitive load on each kind of user must be minimized}.

@section[#:tag "asdf2.26"]{Features introduced in the @(ASDF2) series}

@subsection{@(defsystem) dependencies}

@(ASDF2) introduced a @cl{:defsystem-depends-on} option to @(defsystem),
whereby a system could declaratively specify dependencies on build extensions.
Before that option, users would imperatively load any extension they need:
in their @(asd) system definition file,
they would for instance evaluate @cl{(asdf:load-system :cffi-grovel)},
before they use @(defsystem) to define their systems.
Indeed, a @(asd) file is just a Lisp source file
that is loaded in a controlled context and may contain arbitrary side-effects;
but such side-effects are frowned upon and
a declarative style is more maintainable,
hence this improvement.

However, this feature was only made usable in 2.016 (June 2011),
when @(ASDF) started to accept keywords as designators
for classes defined in an extension in the @cl{:asdf} package.
Before then, there was a chicken-and-egg problem,
because the @(defsystem) form containing the @cl{:defsystem-depends-on} declaration
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
then CFFI defines the class @cl{asdf::cffi-grovel},
that can be designated by the keyword @cl{:cffi-grovel}
amongst the components of the system.

@clcode{
(defsystem "some-system-using-ffi"
  :defsystem-depends-on ("cffi")
  :components
  ((:cffi-grovel "foreign-functions")
   ...))
}

@subsection{Selective System Forcing}

Since the beginning, @(ASDF) has had a mechanism
to force recompilation of everything:
@clcode{
  (asdf:oos 'asdf:load-op 'my-system :force t)
}
In @(ASDF2) that would be more colloquially:
@clcode{
  (asdf:load-system 'my-system :force :all)
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
  CL possesses a mechanism for continuable errors, @(cerror),
  whereby users can interactively or programmatically
  tell the system to continue despite the error.
}
Despite the feature demonstrably not ever having had any single user,
it had been partially documented, and so was finally
fixed and enabled in @(ASDF 2.015) (May 2011) rather than removed.

Then the feature was extended in @(ASDF 2.21) (April 2012)
to cover a negative @cl{force-not} feature,
allowing the fulfillment of a user feature request:
a variant @cl{require-system} of @(load-system)
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
his relative lack of interest in developing @(ASDF)
beyond the point where it got the rest of his software off the ground;
and by contrast the obsession to detail of his successor.

@subsection{Hooking into @cl{require}}

@; TODO: move to concluding section about problems?

Imitating the example set by Dan Barlow on SBCL in @(ASDF1),
@(ASDF2) was made to hook into the @cl{require} mechanism
of a growing number of implementations, so far (March 2014), seven:
ABCL, GNU CLISP, Clozure CL, CMUCL, ECL, MKCL, SBCL
— and this list notably coincides with that of all
the maintained free software implementations.
Thus, on all these implementations, users could,
after they @cl{(require "asdf")},
use this standard but deliberately underspecified CL mechanism
for extending the language:
by evaluating @cl{(require :foo)} they could have it implicitly rely on @(ASDF)
to load the system if not present yet.

However, users ended up mostly not using it, we presume for the following reasons:
@itemlist[
  @item{
    This mechanism is still not ubiquitous enough,
    therefore for portability and reliability,
    you have to know about @(ASDF) and be able to fall back to it explicitly, anyway;
    thus trying to "optimize" the easy case with @cl{require}
    is just gratuitous cognitive load for no gain;
    this illustrates once again the principle that
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
    The @cl{require} mechanism purposefully avoids loading a module that has already been provided,
    thereby making it unpopular in a culture of ubiquitous modifiable source code;
    if you modified a file, you really want it to be reloaded automatically.@note{
      At the same time, @(ASDF) wasn't reliable in avoiding to reload provided modules,
      since most systems don't call @cl{provide} with their name to signal that such
      call to @cl{require} was successful, and therefore next call to @cl{require}
      would cause a new load attempt — this was fixed with the introduction of
      the above-mentioned @cl{require-system} in @(ASDF 2.21) in 2012,
      and its use instead of @(load-system).
    Maybe the more general point is that @(ASDF) did not have a good story with regards to
    extending the set of things that are considered "system" versus "user" defined.
    @(ASDF3.1) adds a notion of "immutable systems"
    that should not be refreshed from filesystem once loaded into memory.
    @; https://bugs.launchpad.net/asdf/+bug/1184002
  } } ]

@subsection{Encoding support}

Back in 2002, most programmers were still using 8-bit characters in various encodings
(latin1, koi8-r, etc.), and Emacs did not support unicode very well.
@(ASDF1) in its typical minimalist manner, just didn't specify any @cl{:external-format}
and let the programmer deal with the implementation-dependent configuration
of character encodings, if such an issue mattered to them.

By 2012, however, Unicode was ubiquitous,
UTF-8 was a @(de_facto) standard, and Emacs supported it well.
A few library authors had started to rely on it (if only for their own names).
Out of over 700 in @(Quicklisp),
most were using plain ASCII (or maybe some 7-bit encoding),
but 87 were implicitly using @cl{:utf-8}, and
20 were using some other encoding, mostly latin1.

Problem is, one would sometimes attempt to load a file encoded with latin1
in a Lisp expecting strictly UTF-8 input, resulting in an error;
or one would load a UTF-8 or Shift-JIS encoded file
in a latin1 configured Lisp, resulting in mojibake.
These issues were made worse by the (legitimate) behavior of SBCL:
at the same time (1) it set the default encoding in a given session
based on the same environment variables as the @tt{libc} locale,
which could vary wildly between developers, even more between hypothetical end-users,
and (2) it would issue an error rather accept invalid UTF-8.
Unhappily, the person who chose the encoding was whoever wrote the code,
and had no control on what environment was used at compile-time;
whereas the user, who may or may not be aware of such encoding issues,
had no idea what encoding an author used, and didn't care until an error was raised
from an unknown library that was depended on by a program he used or wrote.

To make the loading of library code more predictable,
@(ASDF2) added an @cl{:encoding} option to @(defsystem),
so that files may be loaded in the encoding they were written in,
irrespective of which encoding the user may otherwise be using.
Once again, the principle
	@emph{each can specify what they know,
              none need specify what they don’t.}
In this case, the person who knows what encoding he used
is the author of the library.

The encoding option of a system or module was inherited by its components,
unless they overrode it.
The accepted syntax of the option is a keyword, abstracting over
the implementation-dependent @cl{:external-format},
which is not specified by the CL standard.@note{
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
@(ASDF) falls back to using the 8-bit implementation @cl{:default}.

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
as witnessed by the automated testing tool @cl{cl-test-grid}. @XXX{Give URL!}

Because we had learned that a feature isn't complete until it's tested,
we published a library to demonstrate how to put this new infrastructure to good use:
@cl{lambda-reader}, a utility that lets you use the unicode character @cl{λ}
instead of @cl{lambda} in your code.@note{
  And yes, it does feel good to write @cl{λ} this way,
  and it does improve code that uses higher-order functions.
  My @tt{.emacs} has a @tt{(global-set-key "\C-cl" "λ")},
  and my @tt{.XCompose} has @tt{<Multi_key> <period> <backslash> : "λ" U03BB}.
}
Originally based on code by Brian Mastenbrook,
it was modified to fall back gracefully to working mojibake
on implementations that do not support Unicode, and
to offer the syntax modification via the @(de_facto) standard
@cl{named-readtables} extension.
Users still had to enable the modified syntax
at the beginning of every file,
then carefully disable it at the end of the file,
least they cause havoc in other files and/or in the user's environment.

@subsection{Hooks around compilation}

A recurrent question to @(ASDF) developers was about how to properly
modify the CL syntax for some files,
without breaking the syntax for other files:
locally giving short nicknames to packages,
changing the readtable, or the reader function, etc.

The answer in 2011 was to define
a new subclass @cl{my-cl-file} of @(cl-source-file),
then a method on @cl{perform :around ((o compile-op) (c my-cl-file))}
to wrap the usual processing inside a context where syntax was modified.
But not only was it a cumbersome interface, it had the annoying corner case
of having to also define a method for the seldom used operation @cl{load-source-op},
and for any future such imaginable operation involving reading the file.

A better, more declarative interface was desirable,
and implemented in @(ASDF 2.018) (October 2011):
each component could specify an @cl{:around-compile} option
or inherit it from its parent;
if not @(nil), this would designate a function to be called around compilation
(but not loading, to preserve the semantics of bundle operations).
An explicit @(nil) is often needed in the first few files of a system,
before the usual function was defined.

Actually, the function usually cannot be named by a symbol,
because at the time the @(asd) file is read, none of the code has been compiled,
and the package in which the symbol will be interned doesn't exist yet;
therefore, @(ASDF 2.019) (November 2011) made it possible
to designate a function by a string that will be @cl{read} later.
Hence, for instance, systems defined in Stelian Ionescu's IOLib,@note{
  IOLib is a comprehensive general purpose I/O library for CL,
  written by Stelian Ionescu, that strives at doing the Right Thing™
  where many other libraries sacrifice code quality,
  feature coverage or portability for the sake of expediency.
}
use @cl{:around-compile "iolib/asdf:compile-wrapper"},
except for the system @cl{iolib/asdf} itself,
that defines the package and the function.

@subsection{Enforcing user-defined invariants}

Another related feature, added in @(ASDF 2.23) (July 2012),
was the ability for users to define invariants
that @(ASDF) would enforce when compiling their code:
a file might be compliant CL code, and compile correctly,
yet fail to satisfy application-specific invariants
essential to the robustness of the application.
Without the build system checking after every file's compilation,
users would be left with an invalid system;
after they eventually get a runtime error,
they would have to chase which of thousands of files broke the invariant.
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
@cl{asdf-finalizers}, an @(ASDF) extension distributed as a separate system,
provides such an infrastructure:
its @cl{eval-at-toplevel} both evaluates a form and defers it for later inclusion at the top-level,
and its @cl{final-forms} includes all registered such forms at the top-level;
user code can then specify in their @(defsystem)
the @cl{:around-compile "asdf-finalizers:check-finalizers-around-compile"} hook
for @(ASDF) to enforce the invariant.

The other use case was similarly solved with @cl{asdf-finalizers}.
Our data schema included hundreds of parametric types
such as @cl{(list-of passenger)} of @cl{(ascii-string 3 5)}
(for strings of ASCII characters length between 3 and 5).
Checking that data verified the proper invariants
to avoid inserting corrupted data records in the database
or messaging them to partners was an essential robustness feature.
But to define the type via the CL @cl{deftype} mechanism,
these types had to expand to things like
@cl{(and list (satisfies list-of-passenger-p))},
where the predicate function @cl{list-of-passenger-p}
could not be provided additional parameters,
and had to be independently defined by a form @cl{(declare-list-of passenger)};
there again, this form could not be part of the type expansion,
and was not enough to @cl{eval}uate at compile-time,
for it had to be explicitly included at the top-level.
Manually managing those forms was a maintenance burden,
and @cl{asdf-finalizers} eliminated this burden.

The principle we recognized was that
@moneyquote{every large enough application is a Domain-Specific Language with its own invariants,
                  and the programming language is but the implementation language of the DSL}.
This implementation will be extremely fragile
if it cannot automatically enforce the invariants of the DSL.
A good programming language will let you define new invariants,
and a good build system will enforce them.
In CL, this can all happen without leaving the language.

@section[#:tag "asdf3"]{@(ASDF) 3: A Mature Build}

@subsection{Timeline}

@(ASDF3) was a complete rewrite of @(ASDF), several times over,
to correctly deal with its core issues.
The unintended result of these rewrites was to turn it into
a much more robust and versatile product than it was:
not only does it cover the robust building of CL software from CL,
it also includes runtime software management functionality
and integration both ways with the Unix command line.

@(ASDF3) was pre-released as 2.27 in February 2013,
then officially released as 3.0.0 on May 15th 2013.
After a new series of significant improvements,
it will be released again as 3.1.1 in March 2014.

Robert Goldman assumed maintainership in July 2013,
a few months after the release of @(ASDF3).
François-René Rideau remained main developer
until release of 3.1.1 in March 2014.

All the known bugs have been fixed, and the regression test suite has swollen;
but there will always be portability issues to fix,
and there is a big TODO file for suggested ways to improve @(ASDF).
It is uncertain whether a new maintainer will take over development.

@subsection{A Consistent, Extensible, Model}

Surprising as it may be to all CL programmers who used it daily,
there was an essential bug at the heart of the @(ASDF) design,
present from the very first day in 2001, and before it in @(mk-defsystem) since 1990,
and that survived to December 2012.
Fixing this bug required a complete rewrite of the core of @(ASDF).
The entire story is told in @secref{traverse}.

In the end, though, the object model of @(ASDF) is at the same time
more powerful, more robust, simpler to explain.
It now correctly computes and propagates timestamps,
when it was previously failing to.
Its @(traverse) function is no longer made of dark magic,
but instead is a well-designed algorithm.
The bundle operations are not a kludge anymore, but well-supported extensions.
It is easier than before to extend @(ASDF), with fewer limitations and fewer pitfalls:
users may control how their operations do or don't propagate along the component hierarchy.
Thus, @(ASDF) can now express an arbitrary action graph,
and could conceivably be used in the future to build something else than CL.

@XXX{EXAMPLES!}

A test that the model was indeed better and more extensible
was in adapting the most elaborate of the existing extensions to @(ASDF):
@(POIU), and the bundle operations.
The result was indeed cleaner, simply using or extending new infrastructure;
previously they needed to override sizable chunks of the old infrastructure.
Though chronologically, it could rather be said that this porting process itself
was consciously undertaken to test and improve the @(ASDF) architecture.
@moneyquote{The proof of a good design is in the ease to extend it}.
And in Lisp, extension doesn't require privileged access to the code base.

@subsection[#:tag "bundle_operations"]{Bundle Operations}

Bundle operations allow to create a single output file
for an entire system or collection of systems.
The most directly user-facing bundle operation is @(fasl-op),
that bundles into a single fasl
all the individual fasls from the @(compile-op) outputs
of each file in a system.
This bundle fasl may then be loaded by operation @(load-fasl-op).@note{
  FASL, for FASt Loading, is the name of CL compilation output files.
  @(fasl-op) is not a good name, since every individual @(compile-op) also outputs a fasl.
  This operation should have been called @cl{compile-bundle-op}
  and the corresponding loader @cl{load-bundle-op}.
  But backward compatibility makes it hard to change the name.
}
Also @cl{lib-op} links into a library all the object files in a system;
@cl{dll-op} creates a dynamically loadable libary out of these files.
The above bundle operations also have so-called "monolithic" variants,
that bundle all the files in a system @emph{and all its transitive dependencies}.
Bundle operations make delivery of code much easier.
On linker-based implementations such as ECL,
loading bundle fasls also consumes much fewer resources
than loading a lot of small fasls.@note{
  Most CL implementations maintain their own heap with their own garbage collector,
  and then are able to dump an image of the heap on disk,
  that can be loaded back in a new process with all the state of the former process;
  to build an application, you start a small initial image, load plenty of code, dump an image,
  and there you are.
  ECL, instead, is designed to be easily embeddable in a C program;
  it uses the popular C garbage collector by Hans Boehm & al.,
  and relies on linking and initializer functions rather than on dumping;
  to build an application, you link all the libraries and object files together,
  and call the proper initialization functions in the correct order.
  Because of the overhead of dynamic linking, loading a single fasl
  is much preferrable to a lot of fasls.
}

The latter point is why bundle operations were initially introduced as @cl{asdf-ecl},
an extension to @(ASDF) specific to ECL, back in the day of @(ASDF1).
It was distributed with @(ASDF2), though in a way
that made upgrade slightly awkward to ECL users,
who had to reload @cl{asdf-ecl} after upgrading @(ASDF),
even though it was loaded automatically when they used @cl{(require "asdf")}.
In May 2012, it was generalized to other implementations as @cl{asdf-bundle}.
When @(ASDF3) came, it was merged into @(ASDF) itself:
not only did it provide useful new operations,
but the way that @(ASDF3) was automatically upgrading itself for safety purposes
would otherwise have broken things badly for ECL users
if the bundle operations weren't bundled with @(ASDF).

In @(ASDF3.1), using @cl{deliver-asd-op} (previously misnamed @cl{binary-op})
you can create both the bundle fasl from @(fasl-op) and an @(asd) file
to use to deliver the system in binary format only.

@subsection{Understandable Internals}

After bundle support was merged into @(ASDF) (see @secref{bundle_operations} below),
it became trivial to add a new @(concatenate-source-op) operation to @(ASDF);
thus @(ASDF) could be developed as multiple files as would improve maintainability,
yet delivered as a single file as it was strongly required to be.

Breaking down @(ASDF) into smaller, more intelligible pieces
had been proposed in the past, after our takeover of @(ASDF);
but the proposal had been rejected
on the basis of not having that @(ASDF)
must not depend on external tools to upgrade itself,
another strong requirement.
With the new @(concatenate-source-op),
an external tool wasn't needed for delivery and regular upgrade,
only for bootstrap from older versions.
And breaking down @(ASDF) had also become more important,
since @(ASDF) had grown so much, having almost tripled in size since those days,
and promising to grow some more.
It was hard to navigate that one big file, even for the maintainer,
and probably impossible for newcomers to wrap their head around it.

And so, @(ASDF) was broken down (2.26.62).
However, to bring some principle to this refactoring,
each file was made to follow the principle of one-file, one-package,
as implemented by @(quick-build) but
not then by @(ASDF) itself (but see @secref{asdf-package-system}).
This made it easier to ensure that files were indeed providing related functionality,
only had explicit dependencies on other files, and
didn't have any forward dependencies without special declarations.
And indeed, this was a great success in making @(ASDF) understandable,
if not by newcomers, at least by the maintainer himself;
this triggered a series of enhancements that would not otherwise
have been obvious or obviously correct,
illustrating the principle that
@moneyquote{good code is code you can understand,
  organized in chunks you can each fit in your brain}.

@subsection{Package Upgrade}

Preserving the hot upgradability of @(ASDF) was always a strong requirement.
In the presence of this package refactoring,
this meant the development of @(define-package),
a variant of CL's @(defpackage) that plays nice with hot upgrade.
Instead of a change in a package definition being an error,
it is a normal condition handled by @(define-package),
that will make an old package match the new definition,
while recycling existing symbols from that package and other packages.

Thus, in addition to the regular clauses from @(defpackage),
@(define-package) accepts a clause @cl{:recycle},
such that every symbol being defined is attemptedly
recycled from the listed packages in the given order.
For idempotence, the package itself must be the first in the list.
For upgrading from an old @(ASDF), the @cl{:asdf} package is always named last.

Other extensions as compared to @(defpackage) include @cl{:mix} and @cl{:reexport}
@cl{:mix} makes it easy to mix imports from several packages,
automatically handling clashes in favor of the earlier named packages.
@cl{:reexport} reexports the same symbols as imported from given packages,
and/or exports instead the same-named symbols that shadow them.
@(ASDF3.1) adds @cl{:mix-reexport} and @cl{:use-reexport},
which allow to combine @cl{:reexport} with @cl{:mix} or @cl{:use} in a single statement,
which is more maintainable than repeating a long list of packages twice.

@subsection{Portability Layer}

The splitting of @(ASDF) into many files, and the development of this package library,
revealed that a large fraction of @(ASDF)
was already devoted to general purpose utilities.
This fraction only grew as @(ASDF3)
was taking shape under many pressures:
a lot of opportunities for improvement became obvious
after dividing @(ASDF) into many files;
features added from previous extensions required new general-purpose utilities;
functionality from @cl{xcvb-driver}, @cl{trivial-backtrace} and other libraries
was merged in and made fully portable;
as more tests were added for new features, and were run on all supported implementations,
on multiple operating systems, new portability issues cropped up
that required development of robust and portable abstractions.

The portability layer grew, and after it was fully documented,
ended up being slightly bigger than the rest of @(ASDF).
Long before that point, @(ASDF) was thus formally divided in two,
this portability layer, and the @(defsystem) itself.
The portability layer was initially dubbed @cl{asdf-driver},
because of merging in a lot of functionality from @cl{xcvb-driver}.
But under pressure by users to give it a shorter name that didn't include @(ASDF),
yet would somehow be remindful of it, the portability layer was eventually renamed @(UIOP):
the Utilities for Implementation- and OS- Portability (and also what follows QWERTY).
It was made available separately from @(ASDF) as a regular system to be used on its own;
yet since @(ASDF) still needed to be delivered as a single file @tt{asdf.lisp},
@(UIOP) was @emph{transcluded} inside that file, now built using the
@cl{monolithic-concatenate-source-op} operation.
At Google, the library is actually used as a portability layer
without the rest of @(ASDF), the build being handled by Google's @tt{blaze};
this did require improvements to @(UIOP) that will be released with @(ASDF3.1).

Most of the utilities deal with providing sane pathname abstractions (see @secref{pathnames}),
filesystem access, sane input/output, temporary files, basic operating system interaction:
many things for which the CL standard was lacking.
There is also an abstraction layer over the less-compatible legacy implementations,
a set of general-purpose utilities, a common core for the @(ASDF) configuration DSLs.@note{
  @(ASDF3.1) notably introduces a @cl{nest} macro,
  that allows to nest arbitrarily many forms
  without indentation drifting ever to the right.
  It can make for more readable code without sacrificing good scoping discipline.
}
Importantly for a build system, there are portable abstractions for compiling Lisp code
while controlling all the warnings and errors that can occur,
and there is a support for the lifecycle of a Lisp image:
dumping and restoring images, initialization and finalization hooks, error handling, etc.
What turned out the most complex piece though was a portable implementation of @(run-program),
a utility for executing external programs and capturing their output.

@subsection{@(run-program)}

@(ASDF3) thus allows you to run external commands.

@clcode|{
(run-program `("cp" "-lax" "--parents"
               "src/foo" ,destination))
}|
On Unix (or using Cygwin), this will recursively copy
the @tt{src/foo} directory into a directory named by the string @cl{destination},
preserving the prefix @tt{src/foo} and creating hardlinks instead of copying contents.
You might want to add @cl{:output t :error-output t}
to get error messages on your @cl{*standard-output*} and @cl{*error-output*} streams, though,
because the default value, @(nil), designates @tt{/dev/null}.
If an error occurs, @tt{cp} will return with an error code,
which will be converted to a structured Lisp @(error),
because you didn't specify @cl{:ignore-error-status t}.

Such a utility was essential so that CL programs may be used
as a replacement to shell scripts.
But it was an interesting challenge to write.
Each implementation had a completely different interface to running external programs,
with wildly different feature sets and countless corner cases.
Better implementations could spawn a process via @tt{fork(2)} and @tt{execve(2)} or equivalent,
and handle redirection of standard-input, standard-output and error-output file descriptors;
lesser implementations could only call out the @tt{system(3)} C library function.
What more, the behavior on Windows was significantly different from that on Unix.
@(ASDF1) itself actually had a @cl{run-shell-command} that had been copied over from @(mk-defsystem),
but it was more of an attractive nuisance inviting for problems than a solution:
capturing output was particularly contrived, and
what shell would be used varied between implementation, even more so on Windows.

@(ASDF3) provides a full-featured @(run-program), papering over all these discrepancies
to provide control of the program's standard output, using temporary files underneath if needed,
based on code originally from @(XCVB)'s @cl{xcvb-driver} @~cite[XCVB-2009].
Since @(ASDF "3.0.3"), @(run-program) also lets you control the standard input and error output.
It can both execute a program with a list of arguments, or invoke a shell on a command.
Thus your previous program could have been:
@clcode{
(run-program
  (format nil "cp -lax --parents src/foo ~S"
          (native-namestring destination))
  :output t :error-output t)
}
Where (UIOP)'s @cl{native-namestring} converts the @cl{pathname} object @cl{destination}
into a string suitable for use by the operating system,
as opposed to its standard CL @cl{namestring}, which might be escaped somehow.

You can also inject input and capture output:
@clcode{
(run-program '("tr" "a-z" "n-za-m")
    :input '("uryyb, jbeyq") :output :string)
}
will return the string @cl{"hello, world"}.
It will also return secondary and tertiary values @(nil) and @cl{0} respectively,
for the (non-captured) error-output and the (successful) exit code.

Since the functionality provided by @(UIOP) is basic, and
using it a lot can be cumbersome, a more practical interface to shell programs
was written on top of @(UIOP), available as the separate system @(inferior-shell).
It handles pipelines, @tt{zsh} style redirections,
implicit conversion of pathnames into native-namestrings,
symbols into downcased string, keywords into downcased string with a @cl{"--"} prefix,
splicing of strings and/or lists into arguments,
and many short-named functions @cl{run}, @cl{run/nil}, @cl{run/s}, @cl{run/ss},
to respectively run the external command with outputs to the Lisp standard and error output,
run it with no output, run it with output to a string, run it with output to a stripped string, etc.
Thus you could get the same result as previously with:
@clcode{
(run/ss '(pipe (echo (uryyb ", " jbeyq))
               (tr a-z (n-z a-m))))
}
Or you can get the number of processors on your Linux machine with:
@clcode{
(run '(grep -c "^processor.:"
            (< /proc/cpuinfo))
     :output #'read)
}

@subsection{Configuration management}

@(ASDF) always had quite minimal support for configuration management.
@(ASDF3) doesn't introduce radical change,
but provides more usable replacements or improvements for old features.

For instance, @(ASDF1) had always supported version-checking:
each component (usually, a system)
could be given a version string with e.g.
@cl{:version "3.1.0.94"}, and @(ASDF) could be told to check
that dependencies of at least a given version were used, as in
@cl{:depends-on ((:version "inferior-shell" "2.0.0"))}.
This allows early detection of dependency mismatch,
rather than users having to painfully face bugs
and figure out the hard way that they had to upgrade some libraries and which.

Now, @(ASDF) always required components to use "semantic versioning",
where versions are strings made of dot-separated numbers like @cl{3.1.0.94}.
But it didn't enforce it, leading to bad surprises for the users when the mechanism fails to work.
@(ASDF3) will issue a @(warning) when it finds a version that doesn't follow the format.
It would actually have issued an @(error), if that wouldn't have broken backward-compatibility.

Another problem with version strings was that they had to be written as literals in the @(asd) file,
unless that file took painful steps to extract it from another source file.
And while it was easy for source code to extract the version from the system definition,
some authors legitimately wanted their code to not depend on @(ASDF) itself.
Also, it was a pain to repeat the literal and/or the extraction code
in every system definition in a repository.
@(ASDF3) thus allows to extract version information from a file in the source tree, with, e.g.
@cl{:version (:read-file-line "version.text")}
to read the version as the first line of file @tt{version.text}.
To read the second line, that would have been
@cl{:version (:read-file-line "version.text" :at 1)}
(mind the off-by-one error in the English language).
Or you could extract the version from source code.
Thus @tt{poiu.asd} specifies
@cl{:version (:read-file-form "poiu.lisp" :at (1 2 2))}
which is the third subform of the third subform of the second form in the file @tt{poiu.lisp}.
The first form is an @cl{in-package} and must be skipped.
The second form is an @cl{(eval-when (...) body...)} the body of which starts with a
@cl{(defparameter *poiu-version* ...)} form.
@(ASDF3) thus solves this version extraction problem for all software except itself,
because its own version has to be readable by @(ASDF2)
as well as by who reads its single delivery file;
and so its version is inserted by a management script, written in Lisp of course.

Another painful configuration management issue with @(ASDF1) and @(ASDF2)
was lack of a good way to conditionally include files.
One could always use CL reader conditionals such as @(cl "#+(or sbcl clozure)")
but that means that @(ASDF) could not even see the components being excluded,
should some operation be invoked that involves printing or packaging the code
rather than compile it — or should it involve cross-compilation
for another implementation with a different feature set.
There was an obscure way to declare a dependency on a @cl{:feature},
and when it fails, annotate a module with @cl{:if-component-dep-fails :try-next}
to catch the missing feature and try another one.
This didn't allow for more complex feature expressions,
and the implementation was a kluge in @(traverse)
that short-circuited the usual dependency propagation
and had exponential worst case performance behavior.

@(ASDF3) gets rid of @cl{:if-component-dep-fails}:
it didn't fit at all the fixed dependency model.
A limited compatibility mode was preserved that is just good enough
to keep processing the use of this feature in old versions of SBCL.
As a replacement, @(ASDF3) introduces a new option @cl{:if-feature}
in component declarations, such that a component will only be included
in a build if the given feature expression is true during the planning phase.
Thus a component annotated with @cl{:if-feature (:or :sbcl :clozure)}
(and its children, if any) will only be included on the two specified implementations.
This is more expressive than what preceded,
without requiring inconsistencies in the dependency model,
and with no pathological performance behavior.

@subsection{Standalone Executables}

One of the bundle operations contributed by the ECL team was @(program-op),
that allows to create a standalone executable.
As this was now part of @(ASDF3), it was only natural to bring other implementations up to par,
for those that supported it, i.e. CLISP, Clozure CL, CMUCL, LispWorks, SBCL, SCL.
Therefore @(UIOP) features a @cl{dump-image} function to dump the current heap image
on implementations that follow this model, though ECL and its successors
that follow a linking model instead use a different @cl{create-image} function.
These functions were based on code from @cl{xcvb-driver} that had taken them from @(cl-launch).

One may thus specify an entry point to a system with the @(defsystem) option
@cl{:entry-point "my-package:entry-point"}.
The named function (as a string, to be read after the package is created)
will be called without arguments after the program image is initialized;
after doing it own initialization, it can explicitly consult @cl{*command-line-arguments*}
or pass it as an argument to some main function.

Now, our experience at ITA Software for its @cl{QRes} application server
showed the importance of hooks so that various software module may register
finalization functions to be called before dumping the image,
and initialization functions to be called before calling the entry point.
Therefore, support for image lifecycle was added to @(UIOP).
We also added basic support for running programs interactively or non-interactively,
so that non-interactive programs would exit with a backtrace and an error message
repeated above and below the backtrace,
instead of inflicting a debugger on unsuspecting end-users.
For non-interactive programs, any non-@(nil) return value from the entry-point is success,
whereas @(nil) is a failure, leading to the program exiting with according status.

Starting with @(ASDF3.1), for implementations that don't support standalone executables,
it is still possible to dump a heap image using the @(image-op) operation,
and a wrapper script, e.g. created by @(cl-launch), can invoke the program;
delivery is then in two files instead of one.
@(image-op) can also be used by all implementations
to create intermediate images in a staged build,
or to provide ready-to-debug images for otherwise non-interactive applications.

@subsection{@(cl-launch)}

Running Lisp code to portably create executable commands from Lisp is great,
but there is a bootstrapping problem:
when all you can assume is the shell command line,
how are you going to portably invoke the Lisp code that creates a command
to begin with?

This bootstrapping problem was solved some years ago with @(cl-launch).
This bilingual program, in a mix of portable shell script and CL,
provides a nice shell command interface to
building shell commands from Lisp code,
including delivery as either portable shell scripts or
self-contained precompiled executable files.

Its latest incarnation, @(cl-launch 4) (March 2014),
was updated to take full advantage of @(ASDF3);
its build specification interface was made more general,
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
@tt{-E}, shorthand for @tt{--entry} configures a function that will be called
when the program starts, with the list of command-line arguments.@note{
  Several systems are available to help you define an evaluator
  for your command-line argument DSL:
  @cl{command-line-arguments}, @cl{clon}, @cl{lisp-gflags}.
}
As for @tt{lisp-stripper}, it's a simple library that counts lines of code
after removing comments, blank lines, docstrings, and multiple lines in strings.

@(cl-launch) will automatically detect a CL implementation
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
  Debian's @cl{common-lisp-controller} as far back as 2002,
  and by the more widely portable @cl{asdf-binary-locations} after it.

  By defining an @cl{:around} method for the @cl{output-files} function,
  it was possible for the user to control where @(ASDF)
  would store its compilation output,
  without the authors of @(ASDF) having had to create an explicit hook.
  It is unclear who first came up with the idea,
  but the fact that this technique could be developed
  as an obvious extension to @(ASDF)
  without the author explicitly designing the idea into it,
  and without having to modify the source code,
  is an illustration of how expressive and modular CLOS can be.

  Now, @cl{asdf-binary-locations} and other variants of the same idea
  had a bootstrapping issue: the extension had to be specially loaded
  before it could be used, whether as source code or precompiled code,
  otherwise the potential clashes regarding its own compiled file
  would negate any advantage in avoiding clashes for files of other systems.
  @cl{common-lisp-controller} was trying to solve the same issue
  by having managed software installations in a controlled way;
  it failed eventually, because these efforts were only available
  to CL programmers using Debian or a few select other Linux software distributions,
  and only for the small number of slowly updated libraries,
  making the maintainers a bottleneck in a narrow distribution process.
  @cl{common-lisp-controller} also attempted to manage a system-wide cache of compiled objects,
  but this was ultimately abandoned for security issues;
  a complete solution would have required a robust and portable build service,
  which was much more work than justified by said narrow distribution process.
  The solution in @(ASDF2) was to merge this functionality in @(ASDF) itself,
  yielding its @cl{output-translations} layer,
  according to the principle of making everything as simple as possible,
  @emph{but no simpler}.
  As another, more successful take on the idea of @cl{common-lisp-controller},
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

@subsection[#:tag "asdf-package-system"]{@(asdf/package-system)}

@(ASDF3.1) introduces a new @(asdf/package-system) extension,
that supports a one-file, one-package, one-system style of programming.
This style was pioneered by Peter Etter's @(faslpath),
and more recently by Alastair Bridgewater's @(quick-build).
@(asdf/package-system) is actually compatible with the latter,
though not with the former, for @(ASDF3.1) and @(quick-build)
use a slash @cl{"/"} as a hierarchy separator,
where @(faslpath) used a dot @cl{"."}.

The principle of this lightweight system definition style is that
every file thus starts with a @(defpackage) or @(define-package) form;
from its @cl{:use} and @cl{:import-from} and similar clauses,
the build system can identify a list of packages it depends on,
then map the package names to the names of systems and/or other files,
that need to be loaded first.
Thus package name @cl{lil/interface/all} (downcased if needed),
refers to the file @tt{interface/all.lisp},
under the package-system hierarchy registered by system @cl{lil},
defined as follows in @cl{lil.asd}:
@clcode{
(defsystem "lil" ...
  :description "LIL: Lisp Interface Library"
  :class :package-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("lil/interface/all"
               "lil/pure/all" ...)
  ...)
}
The @cl{:defsystem-depends-on ("asdf-package-system")}
is an external extension that provides backward compatibility with @(ASDF 3.0),
and is part of @(Quicklisp).
Because not all package names can be directly mapped back to a system name,
@(asdf/package-system) allows you to register new mappings.
The @tt{lil.asd} file thus allows contains forms such as:
@clcode{
(register-system-packages :closer-mop
 '(:c2mop :closer-common-lisp :c2cl ...))
}
Then, a file @tt{interface/order.lisp} under the @tt{lil} hierarchy,
that defines abstract interfaces for order comparisons,
starts with the following form:
@clcode{
(uiop:define-package :lil/interface/order
  (:use :closer-common-lisp
   :lil/interface/definition
   :lil/interface/base
   :lil/interface/eq :lil/interface/group)
  (:mix :fare-utils :uiop :alexandria)
  (:export ...))
}
And all the dependencies are trivially computed.

This style provides many maintainability benefits:
by imposing upon programmers a discipline of smaller namespaces,
with explicit dependencies, and forward dependencies especially so,
the style encourages good factoring of the code into coherent units;
by contrast, the tradition style of "everything in one package"
has low overhead, but doesn't scale very well.
@(ASDF) itself was re-written in this package-system style as part of @(ASDF 2.27),
the initial @(ASDF3) pre-release, for very positive results.

@(asdf/package-system) is not light weight like @(quick-build),
that is two orders of magnitude smaller than @(ASDF).
But it does interoperate perfectly with the rest of @(ASDF),
from which it inherits the many features, and the portability and robustness.

@subsection[#:tag "backwarder_compatibility"]{Backwarder compatibility}

@(ASDF3) had to break compatibility with @(ASDF1) and @(ASDF2):
any operation that inherited from the @(operation) class hierarchy used
to be propagated sideway and downward along the component DAG.
In most cases, this was an unwanted behavior, and indeed,
@(ASDF3) was predicated upon the introduction of a new operation @(prepare-op)
that instead propagates upward along the DAG (see @secref{traverse}).
But the existing extensions to @(ASDF) for which this was unwanted
had various workarounds and approximations to deal for the issue.
Then there were a handful extensions that did expect this behavior,
and now they were broken.

Before the release of @(ASDF3), authors of all known @(ASDF) extensions
part of the free software @(Quicklisp) distribution had been contacted,
to make them compatible with the new fixed model.
But there was no way to contact unidentified authors of proprietary extensions,
beside sending an announcement to the mailing-list.
Yet, whatever message was sent didn't attract enough attention,
because our co-maintainer Robert Goldman got bitten hard
when an extension used at work stopped working,
wasting days of debugging to figure out the issue.

Therefore, @(ASDF3.1) features a new feature to make it more backward-compatible.
The class @(operation) will implement sideway and downward propagation
on all classes that do not explicitly inherit from any of the automatic propagation mixins
@(downward-operation), @(upward-operation), @(sideway-operation) or @(selfward-operation),
unless they explicitly inherit from the new mixin @(non-propagating-operation).
@(ASDF) will issue a @(warning) at runtime when an operation class is instantiated
that doesn't inherit from any of the propagating or non-propagating mixins,
which will hopefully tip off any author of proprietary extension that it is time to upgrade.
To signal to @(ASDF) that their operation class is not backward,
extension authors will thus has to have their non-propagating operations inherit from
@(non-propagating-operation) as in:
@verbatim|{
(defclass my-op (#+asdf3.1 non-propagating-operation operation) ())
}|

This is a case of "negative inheritance", a technique usually frowned upon,
for the explicit purpose of backward compatibility.
Now @(ASDF) cannot use the CLOS Meta-Object Protocol (MOP),
because it hasn't been standardized enough to be portably used
without using an abstraction library such as @cl{closer-mop},
yet it cannot depend on any external library,
and this is too small an issue to make a sizable MOP support part of @(UIOP).
Therefore, the negative inheritance is implemented
in an @emph{ad hoc} way at runtime.

@section[#:tag "evolving"]{Code Evolution in an Conservative Community}

@subsection{Mission Creep, not Feature Creep}

Throughout the many features added and decupling in size from @(ASDF1) to @(ASDF3),
@(ASDF) remained true to its minimalism — but the mission,
relative to which the code remains minimal, was extended, several times:
In the beginning, @(ASDF) was the simplest extensible variant of @(defsystem) that builds CL software.
With @(ASDF2), it had to be upgradable, portable, modularly configurable, robust, performant, usable.
Then it had to be more declarative, more reliable, more predictable,
and capable of supporting language extensions.
Now, it has to support a cleaner new model for representing dependencies,
software delivery as either scripts or binaries, a cleaner one-package-per-file style,
a documented portability layer including image lifecycle and external program invocation, etc.

@subsection{Backward Compatibility is Hard}

As efforts were made to improve @(ASDF),
a constant constraint was that of @emph{backward compatibility}:
every new version of @(ASDF) had to be compatible with the previous one,
i.e. systems that were defined using previous versions
had to keep working with new versions.
But what more precisely is backward compatibility?

In an overly strict definition that precludes any change in behavior whatsoever,
even the most uncontroversial bug fix, is not backward-compatible:
any change, for the better as it may be, is incompatible,
since by definition, some behavior has changed!

One might be tempted to weaken the constraint a bit,
and define "backward compatible" as being the same as
a "conservative extension":
a conservative extension may fix erroneous situations,
and give new meaning to situations that were previously undefined,
but may not change the meaning of previously defined situations.
Yet, this definition is doubly non satisfactory.
On the one hand, it precludes any amendment to previous bad decisions;
hence, the jest @moneyquote{if it's not backwards, it's not compatible}.
On the other hand, even if it only creates new situations
that work correctly where they were previously in error,
some analysis tool might exist that assumed these situations could never arise,
and if stumbled that they now do.

Indeed this happened when @(ASDF3) tried to better support "secondary systems".
@(ASDF) looks up systems by name: if you try to load system @cl{foo},
@(ASDF) will search in registered directories for a file call @tt{foo.asd}.
Now, it was common practice that programmers
may define multiple "secondary" systems in a same @(asd) file,
such as a test system @cl{foo-test} in addition to @cl{foo}.
This could lead to "interesting" situations when a file @tt{foo-test.asd}
existed, possibly from a different, otherwise shadowed, version of the same library.
@(ASDF2) required robustifying against the infinite loops that could result.
To make these situations less likely,
@(ASDF3) recommends that you name your secondary system @cl{foo/test} instead of of @cl{foo-test},
which should work just as well in @(ASDF2), but with reduced risk of clash.
Moreover, @(ASDF3) can recognize the pattern and automatically load @tt{foo.asd}
when requested @cl{foo/test}, in a way guaranteed not to clash with previous usage,
since no directory could contain a file thus named in any modern operating system.
In contrast, @(ASDF2) has no way to automatically locate the @(asd) file
from the name of a secondary system, and so you must ensure that you loaded the primary @(asd) file
before you may use the secondary system.
This feature may look like a textbook case of a backward-compatible "conservative extension".
Yet, it is the major reason why @(Quicklisp) itself hasn't adopted @(ASDF3):
@(Quicklisp) assumed it could always create a file named after each system,
which happened to be true in practice (though not guaranteed) before this @(ASDF3) innovation.
Refactoring @(Quicklisp) to account for this new style of secondary systems
was more work than the author Zach Beane has so far been willing to spend.

What then, is backward compatibility? It is not a technical constraint.
@moneyquote{Backward compatibility is a social constraint}.
The new version is backward compatible if the users are happy.
This doesn't mean matching the previous version on all the mathematically conceivable inputs;
it means improving the results for users on all the actual inputs they use;
or providing them with alternate inputs they may use for improved results.

@subsection{Weak Synchronization}

Even when some "incompatible" changes are not controversial,
it is often necessary to provide temporary backward compatible solutions
until all the users can migrate to the new design.
Changing the semantics of one software system while other systems keep relying on it
is akin to changing the wheels on a running car:
you cannot usually change them all at once,
at some point you must have both kinds active,
and you cannot remove the old ones until you have stopped relying on them.
Within a fast moving company,
such migration of an entire code base can happen in a single checkin.
if it's a large company with many teams, it can take many weeks or months.
When the software is used by a weakly synchronized group like the CL community,
the change can take years.

We discussed the change in default encoding (see @secref{Encoding_support}),
and how it took a year to change the default from @cl{:default} to @cl{:utf-8}.
When introducing @(ASDF3), it took a few months to fix all the publicly available systems
that were affected by its minor incompatibilities;
and a lot of those months though were fixing @(ASDF3) itself to be more compatible.
Indeed, several intended changes had to be forsaken,
that didn't have an incremental upgrade path,
and for which it proved infeasible to fix all the clients.

Among these changes was an innovative system to control warnings issued by the compiler.
On the one hand, the @cl{*uninteresting-conditions*} mechanism allows system builders
to hush the warnings they know they don't care for,
so that any compiler output will be something they care for,
and whatever they care for won't be drowned into a sea of uninteresting output.
The mechanism itself is included in @(ASDF3), but disabled by default,
because there was no consensually agreeable value except an empty set,
and no good way (so far) to configure it both modularly without pain.
Another related mechanism that was similarly disabled is @cl{deferred-warnings},
whereby @(ASDF) can check warnings that are deferred by SBCL or other compilers
until the end of the current "compilation-unit".
These warnings notably include forward references to functions and variables.
In the previous versions of @(ASDF), these warnings were output at the end
of the session at the first time a file was built, not checked, and not displayed afterwards.
If in @(ASDF3) you @cl{(uiop:enable-deferred-warnings)},
these warnings are displayed and checked every time a system is compiled or loaded.
This helps catch more bugs, but the catch is that enabling it prevents the successful
loading of a lot of systems in @(Quicklisp) that have such bugs,
but the main functionality of which is not affected by these bugs.
Until there exists some configuration system that allows
for those checks to happen on new code without breaking old code,
the feature will have to remain disabled by default.

@subsection{Innovation is Hard}

The road from @(mk-defsystem) to @(ASDF3) is undeniably one of overall improvements.
Yet, along the way, many innovations were attempted that didn't pan out in the end.

For instance, at some point, some concept of preference files was attempted,
so that users may customize how the build takes place, or fix some systems
without modifying their source code.
The feature was never used, and eventually was removed.
On the one hand, anything that makes the build less predictable is a nuisance.
On the other hand, sometimes things are broken, and
you do need a non-intrusive way of fixing them.
@(ASDF) will probably need to grow some way
to configure fixes to builds without patching code, but it is not there yet.

Later versions of @(ASDF1) also introduced
their own generalized @cl{asdf:around} method combination,
that wrapped around the tradition @cl{:around} method combination,
so it may define some methods without blocking users from defining their own extensions.
This was causing portability issues with implementations
that didn't fully implement this corner of CLOS.
@(ASDF2) removed this feature, instead dividing in two the function @(perform)
that was using it, with the method around it
being explicitly called @cl{perform-with-restarts}.
Indeed, in a cross-compilation environment, you'd want your restarts in the master Lisp,
whereas the @(perform) method takes place on the target compiler,
so it really makes sense.
@(ASDF1) authors liked to experiment with how far they could push the use of CLOS;
but at some point there can be too much fanciness.
As another smaller example of that, Dan Barlow made a lot of uses of anaphoric macros
as then popularized by Paul Graham, such as @cl{aif} that implicitly
(and "non-hygienically") binds a variable @cl{it} in its success branch;
but the experiment was eventually considered a failure, and the rough community consensus
is that anaphoric macros are in poor taste, and so in @(ASDF3),
all remaining occurrences of @cl{aif} where replaced by an explicitly binding macro
@cl{if-bind} copied from the @cl{alexandria} library.

The @cl{asdf-binary-locations} extension ultimately failed because
it didn't fully solve its configuration problem,
only concentrated it in a single point of failure.
The @cl{*system-cache*} feature to share build outputs between users
and associated @cl{get-uid} function, introduced by @cl{common-lisp-controller}
and used by @(ASDF2)'s @cl{output-translation} layer,
were removed because of security issues.
See @secref{Configurability}.
A @cl{:current-directory} keyword in the configuration DSL was removed,
because not only did its meaning vary wildly with implementation and operating system,
this meaning varied with what the value of that global state
at the time the configuration file was read,
yet because of lazy loading and implicit or explicit reloading of configuration,
no one was really in control of that value.
On the other hand, the @cl{:here} keyword was a successful replacement:
it refers to the directory of the configuration file being read,
the contents of which are clearly controlled by whoever writes that file.

In an attempt to solve namespace clashes between @(asd) files,
Dan Barlow had each of them loaded in
its own automatically created private package @cl{asdf0}, @cl{asdf1}, etc.,
automatically deleted afterwards.
But this didn't help. If the file contained no new definition,
this hassle wasn't needed; and if there were new definitions,
either users were using the same kind of prefixing conventions
as were necessary anyway to avoid clashes in existing packages,
or they were defining their own package @cl{foo-system},
to hold the definitions.
Otherwise, when the definitions were left in the default package,
their symbol became unreachable and the definitions impossible to debug.
In the end, to solve the namespace issues of CL would have required
a complete intrusive change of the package system,
and that was not a task for @(ASDF).
If anything, @(faslpath), @(quick-build) and @(asdf/package-system)
seem to have a better approach at enforcing namespace discipline.

There were other namespace fiascos.
In the last days of @(ASDF1), there was an attempt to export
its small set of general purpose utilities as package @cl{asdf-extensions},
quickly renamed @cl{asdf-utilities} before the release of @(ASDF2),
to avoid a misnomer. Still, because @(ASDF) had been changing so much in the past,
and it was hard to rely on a recent version,
no one wanted to depend on @(ASDF) for utilities,
especially not when the gain was so small in the number of functions used.
A brief attempt was make these (now more numerous) utilities available as
a completely separate system @cl{asdf-utils} with its own copy of them
in its own package. But the duplication felt like both a waste of
both runtime resources and maintainer time.
Instead, @cl{asdf-driver}, once renamed @(UIOP), was relatively successful,
because it was also available as a system that could be updated independently
from the rest of @(ASDF), yet shared the same source code and same package
as the version used by @(ASDF) itself. No duplication involved.
However, a brief attempt to give @cl{asdf-driver} the nickname @cl{d}
was quickly met with reprobation, as many programmers feel that that short a name
should be available for a programmer's own local nicknames while developing.
Trying to homestead the @cl{:dbg} keyword for a debugging macro met the same opposition.

Some features were not actively rejected, but haven't found their users yet.
@(ASDF3) introduced @(build-op) as a putative default build operation
that is not specialized for compiling CL software;
but it isn't used yet either. It might be more useful if the associated function
@cl{asdf:build-system} were renamed to @cl{asdf:build}, or @cl{asdf:build-op},
or something shorter than @cl{asdf:load-system}, anyway.
Similarly, the @cl{*load-system-operation*} was designed so that
ECL may use @(load-fasl-op) instead of @(load-op) by default;
but that's still not the case, and won't be until ECL users more actively test it,
which they might not do until it's the default, since they haven't otherwise heard of it.
The similar variable @cl{*compile-file-function*} on the other hand,
whereby ECL was overriding @(ASDF)'s @cl{compile-file*}
abstraction and extension of CL's standard @cl{compile-file} function,
was wholly rejected as only being one more way users would call the wrong function,
in favor of making @cl{compile-file*} itself more clever and aware of the peculiarities of ECL.

@subsection{Interface Rigidity}

There were many cases during @(ASDF) development
where we wanted to rename a function or change the behavior of a class.
Often, we could do it, but sometimes, we found we couldn't:
when a generic function was simultaneously called by users and extended by users;
or when a class was simultaneously used as a base class to inherit from
and as a mixin class to get behavior from.

For instance, we found that @(component-depends-on) was a complete misnomer,
and should have been @(action-depends-on) or something similar.
But since there were user systems that defined methods on this function,
our @(action-depends-on) would have had to call @(component-depends-on)
at least as a fallback. Conversely, because some users do call @(component-depends-on),
that function would have to call @(action-depends-on).
To avoid infinite recursion would then require complex machinery
that could prove error-prone, for little gain beside a name change.
The rename was not to happen.

Similarly, we wanted to remove some behavior from the abstract class @(operation),
but found that some users relied on that behavior, so we couldn't remove it,
yet our software relied on that behavior being removed, so we had to remove it.
In the end, we implemented an ugly mechanism of "negative inheritance",
to selectively disable the behavior for appropriate subclasses of @(operation)
while keeping it for legacy operations (see @secref{backwarder_compatibility}).

By contrast, the CLOS protocol was cleverly designed so that users do not usually
call the functions on which they define methods (such as @cl{initialize-instance},
or @cl{update-instance-for-redefined-class}), and do not usually define methods on
the functions they call.

Do not impose overly rigid interfaces on yourself.

@subsection{Cognitive Load Matters}

While developing @(ASDF), we sometimes made many things more uniform
at the cost of a slight backward incompatibility
with a few existing systems using kluges.
For instance, @(ASDF2) made pathname arguments uniformly non-evaluated in a @(defsystem) form,
when they used to be evaluated for toplevel systems but not for other (most) components;
this evaluation was used by a few users to use @cl{merge-pathnames}
to portably specify relative pathnames, a task made unnecessary by
@(ASDF2) being capable of specifying these pathnames portably with Unix syntax.

@(ASDF3) removed the magic undocumented capability of specifying a systems
as dependencies of a system or module by declaring it in the list of subcomponent
rather than the list of dependencies;
this capability seems to have been an undesigned artifact of how systems used to be parsed,
though at the same time it seems to have been compatible with how some older defsystems did things,
and one user relied on the capability whose system definition had been ported from @(mk-defsystem).

At the cost of a handful of users having to cleanup their code a bit,
we could thus notably @moneyquote{reduce the cognitive load on users} for all future systems.
No more need to learn complex syntactic and semantic constraints
and even more complex tricks to evade those constraints.

@subsection{Verbosity Smells Bad}

Back in the bad old days of @(ASDF1),
the official recipe, described in the manual,
to override the default pathname type @tt{.lisp} for a Lisp source file to
e.g. @tt{.cl}, used to be to define a method on the generic function
@cl{source-file-type}, specialized on the class @cl{cl-source-file}
and on your system (in this example, called @cl{my-sys}):

@clcode{
(defmethod source-file-type
    ((c cl-source-file)
     (s (eql (find-system 'my-sys))))
  "cl")
}

Some people advertised this alternative, that also used to work,
to define your own sub-class @tt{foo-file} of @(cl-source-file), and use:
@cl{(defmethod source-file-type ((c foo-file) (s module)) "foo")}.
This caused much grief when we tried to make @(system)
not a subclass of @(module) anymore,
but both be subclasses of new abstract class @cl{parent-component} instead.

In @(ASDF) 2.015, two new subclasses of @(cl-source-file) were introduced,
@cl{cl-source-file.cl} and @cl{cl-source-file.lsp},
that provide the respective types @tt{.cl} and @tt{.lsp},
which covers the majority of systems that don't use @tt{.lisp}.
Users need simply add to their @(defsystem) the option
@cl{:default-component-class :cl-source-file.cl}
and files will have the specified type.
Individual modules or files can be overridden, too,
either by changing their class from @cl{:file} to @cl{:cl-source-file.cl},
or more directly by specifying a @cl{:pathname} parameter.

If needed, users can define their own subclass of @(cl-source-file)
and override its default @cl{type}, as in:
@clcode{
(defclass my-source-file (cl-source-file)
  ((type :initform "l")))
}
Or they can directly override the type while defining a component, as in:
@clcode{
(:file "foo" :type "l")
}

In any case, the protocol was roundabout both for users and implementers,
and a new protocol was invented that is both simpler to use and easier to extend.
@moneyquote{Verbosity is a bad smell, it suggests lack of abstraction, or bad abstractions}.

@subsection{Syntax Control}

Guy Steele has been quoted
as vaunting the programmability of Lisp's syntax by saying:
@emph{If you give someone Fortran, he has Fortran.
If you give someone Lisp, he has any language he pleases.}
Unhappily, if he were speaking about CL specifically,
he would have had to add:
@emph{but it can't be the same as any one else's}.

Indeed, syntax in CL is controled via
a fuzzy set of global variables,
and making non-trivial modifications to the content of these variables,
notably the @cl{*readtable*}, is both possible and frowned upon,
because if such modifications escape their intended scope,
they can cause unexpected breakage in unrelated parts of the system,
written by different people.
What is worse, changing syntax is only useful if it also happens
at the interactive REPL;
but unless the build system knows to control the syntax
around the files it compiles and loads,
these interactive changes can affect files built from the REPL
— which can cause catastrophic circular dependencies if you
compile with modified syntax files that in next sessions
will have to be loaded before the files that support the syntax modification.

Build support is therefore strongly required for safe syntax modification,
and this build support is not there yet in @(ASDF3)
because of backward-compatibility and/or performance reasons.
Creating a fresh copy of the standard readtable around each action is too expensive.
Trying to use a read-only copy of the standard readtable is not universally portable,
but it is possible, and authors of implementations that don't have that feature yet can be convinced.
We tried to make such a change before the @(ASDF3) release, however
there was only one catch, and that was Catch-22.
Some existing libraries in @(Quicklisp) modified the current @cl{*readtable*},
and had to be fixed before this change happened in @(ASDF);
the main culprit, though, was @cl{iolib}, the new version of which was fixed in this respect,
but already required a recent @(ASDF3) pre-release.
@(Quicklisp), though, couldn't upgrade to @(ASDF3) for other reasons
(see @secref{Backward_Compatibility_is_Hard}),
and thus couldn't adopt a newer @cl{iolib}.
This particular roadblock looks like it will go away in 2014,
but even after it is gone, the new @(ASDF) maintainers will have a lot of work to do
before they can safely enable syntax control by default in @(ASDF).
And they might even have to implement some configuration mechanism
to somehow loosen syntax control around some old systems.

In any case, until such issues are resolved,
even though the Lisp ideal is one of ubiquitous syntax extension,
and indeed extension through macros is ubiquitous,
extension though reader changes are rare in the CL community.
This is in contrast with other Lisp dialects, such as Racket,
that have succeeded at making syntax customization both safe and ubiquitous,
by having it be strictly scoped to the current file or REPL.
@moneyquote{any language feature has to be safe before it may become ubiquitous}.

@subsection[#:tag "conclusion"]{Problems with CL itself}

The CL standard leaves many things underspecified about pathnames,
in an effort to define a useful subset common to many existing implementations.
However, the result is that portable programs can forever only access
but a small subset of the complete required functionality,
making its standard less useful than if it had not specified anything,
and left the job to another standard.
The lesson is @emph{don't standardize partially specified features}.
Instead, @moneyquote{do delegate to existing or future standards}.
Better have pathname protocol per operating system,
and let libraries sort out a portability layer over N operating systems,
than have one pathname protocol per implementation per operating system,
and now libraries have to take into account N*M combinations
of operating systems and implementations.

Interestingly, the aborted proposal for including @(defsystem)
in the CL standard was also of the kind that would have specified
a minimal subset insufficient for large scale use
while letting the rest underspecified.
The CL community probably dodged a bullet thanks to the failure of this proposal.

Another general problem with CL is that
its semantics are defined in terms of
@emph{irreversible side-effects to a global environment}.
A better principle would be to
@moneyquote{define a programming language's semantics in terms of
                   pure transformations with local environments}.

There are many lessons to be learned by studying the successes and failures of the Lisp community.
The CL language and community are probably too rigid to apply these lessons;
but maybe your current or next programming language can.

@subsection{Final Lesson: Explain it}

While writing this article, I had to revisit many concepts and pieces of code,
which led to many bug fixes and small refactorings;
an earlier interactive "walkthrough" via Google Hangout also led to enhancements.
This illustrates the principle that you should always
@moneyquote{explain your programs}:
having to intelligibly verbalize the concepts will make @emph{you} understand them better.

@section[#:tag "pathnames" #:style (make-style 'appendix '(unnumbered))]{Appendix A: Pathnames}

Abandon all hopes, ye who enter here!

@section[#:tag "traverse" #:style (make-style 'appendix '(unnumbered))]{Appendix B: A @(traverse) across the build}

@subsection{The end of @(ASDF2)}

@;https://bugs.launchpad.net/asdf/+bug/479522   wanted: recompile system when dependency changes
@;https://bugs.launchpad.net/asdf/+bug/627173   asdf doesn't recompile when .asd file has changed
@;https://bugs.launchpad.net/asdf/+bug/656450   Forcing logic is baked into traverse
@; This was spawned from 479522 after 2.26.8:
@;https://bugs.launchpad.net/asdf/+bug/1087609  failure to check timestamps of dependencies

While the article itself describes
the @emph{features} introduced by the various versions of @(ASDF),
this appendix focuses on the @emph{bugs} that were the death of @(ASDF),
and its rebirth as @(ASDF3).

The @(ASDF2) series culminated with @(ASDF 2.26) in October 2012,
after a few months during which there were only minor cleanups,
portability tweaks, or fixes to remote corner cases.
Only one small bug remained in the bug tracker,
with maybe two other minor annoyances;
all of them were bugs as old as @(ASDF) itself,
related to the @(traverse) algorithm that walks the dependency DAG.

The minor annoyances were that a change in the @(asd) system definition file
ought to trigger recompilation in case dependencies changed in a significant way,
and that the @(traverse) algorithm inherited from @(ASDF1)
was messy and could use refactoring to allow
finer and more modular programmatic control of what to build or not to build.
The real but small bug was that dependencies were not propagated across systems.
Considering that my co-maintainer Robert Goldman
had fixed the same bug earlier in the case of dependencies across modules within a system,
and that one reason he had disabled the fix across systems
was that some people claimed they enjoyed the behavior,
it looked like the trivial issue of just enabling the obvious fix
despite the conservative protests of some old users.
It was a wafer thin mint of an issue.

And so, of course, since this was the "last" bug standing, and longstanding,
I opened it...
except it was a Pandora's Box of bigger issues,
where the fixing of one quickly led to another, etc.,
which resulted in the explosion of @(ASDF2).

@subsection{The @(traverse) algorithm}

In the last release by Dan Barlow, @(ASDF 1.85) in May 2004,
the @(traverse) algorithm was a 77-line function with few comments,
a terse piece of magic at the heart of the original 1101-line build system.
Shortly before I inherited the code, in @(ASDF 1.369) in October 2009,
it had grown to 120 lines, with no new comment but with some commented out debugging statements.
By the time of @(ASDF 2.26) in October 2012,
many changes had been made,
for correctness (fixing the incorrect handling of many corner cases),
for robustness (adding graceful error handling),
for performance (enhancing asymptotic behavior from O(n⁴) to O(n)
by using better data structures than naïve lists),
for extensibility (moving away support for extra features such as @cl{:version} and @cl{:feature}),
for portability (a trivial tweak to support old Symbolics Lisp Machines!),
for maintainability (splitting it into multiple smaller functions and commenting everything).
There were now 8 functions spanning 215 lines.
Yet the heart of the algorithm remained essentially unchanged,
in what was now a heavily commented 86-line function @cl{do-traverse}.
Actually, it was one of a very few parts of the @(ASDF1) code base that we hadn't completely rewritten.

Indeed, no one really understood the underlying design,
why the code worked when it did (usually) and why it sometimes didn't.
The original author was long gone and not available to answer questions,
and it wasn't clear that he fully understood the answers himself —
Dan Barlow had been experimenting, and how successfully!
His @(ASDF) illustrates the truth that
@moneyquote{code is discovery at least as much as design};
he had tried many things, and while many failed,
he struck gold once or twice, and that's achivement enough for anyone.

Nevertheless, the way @(traverse) recursed into children components was particularly ugly;
it involved an unexplained special kind of dependency, @(do-first),
and propagation of a @(force) flag.
But of course, any obvious attempt to simplify these things
caused the algorithm to break somehow.

Here is a description of @(ASDF1)'s @(traverse) algorithm,
reusing the vocabulary introduced in @secref{Action_Graph}.

@(traverse) recursively visits all the nodes in the DAG of actions,
marking those that are visited, and detecting circularities.
Each action consists of an operation on a component;
for a simple CL system with regular Lisp files,
these actions will be @(compile-op) for compiling the code in the component,
and @(load-op) for loading this compiled code;
a component will be a @(system), a recursive @(module), or a @(file)
(actually a @(cl-source-file)).

When visiting the action of an operation on a component,
it propagates the operations along the component hierarchy,
first sideways amongst siblings, then specially downwards toward children:
if A @(depends-on) B (in the component DAG),
then any operation on A @(depends-on) same operation on B
(this being a dependency in the distinct action DAG);
then, any operation on A @(depends-on) same operation on
each of A's children (if any).
Thus, to complete the @(load-op) (resp. @(compile-op)) of a @(module),
you must first complete the @(load-op) (resp. @(compile-op))
of all the components it was declared as @(depends-on),
then on all its own children.
Additionally, a @(load-op) on A @(depends-on) a @(compile-op) on A;
this is actually encoded in the extensible function @(component-depends-on):@note{
  This function is quite ill-named,
  since it describes dependencies between @emph{actions}, not between components.
  But the original @(ASDF1) code and documentation
  doesn't include an explicit notion of action,
  except to mention "visited nodes" in comments about @(traverse).
  The notion was made explicit while implementing @(ASDF3),
  reusing the word from an earlier technical report by Robbins @~cite[AITR-874].
}
user-defined operation classes can be defined, with according new methods
for the @(component-depends-on) function.

Now here comes the tricky part.
The action of a @(compile-op) on A has a special @(do-first) dependency
on a @(load-op) of each of A's sideways dependencies.
New @(do-first) dependencies can otherwise be specified in the @(defsystem) form,
though no one does it and there is no extensible @cl{component-do-first} function.
These dependencies are included in the plan not only before the action,
but also before any of the operations on the component's children;
yet they are not visited to determine whether the action needs to be performed,
and so the children are specially visited after the siblings but before the @(do-first),
yet the @(do-first) are inserted before the children.
And this careful sequencing is baked into the @(traverse) algorithm
rather than reified in dependencies of the action graph.

What if you transform these @(do-first) dependencies
into regular @(in-order-to) dependencies?
Then there is no incremental compilation anymore,
for the first time you attempt to @(load-op) a system,
any file that has dependencies would have a @(compile-op) action
that @(depends-on) the @(load-op) actions on its dependencies,
that obviously haven't been completed yet;
and so any file with dependencies would be recompiled every time.

@subsection[#:tag "force"]{@emph{Force} Propagation}

Now, as it @(traverse)d the action graph, @(ASDF) was propagating a @(force) flag
indicating whether an action needed to be performed again in the current session
due to some of its dependencies itself needing to be updated.

The original bug was that this flag was not propagated properly.
If some of the sideway dependencies were outdated,
then all childen needed to be forced;
but @(ASDF1) failed to do so.
For instance, if @(module) A @(depends-on) B,
and B is flagged for (re)compilation,
then all the children of A need to be flagged, too.
And so Robert Goldman had fixed a this bug in the leadup to the @(ASDF2) release,
by correctly propagating the flag;
except for some reason, he had declined at the time to propagate it for systems,
propagating it only for modules inside systems.
Glancing at the bug three years later,
I naïvely figured it was just a matter of removing this limitation (2.26.8).
Except that fix didn't work reliably between systems,
and that was why he hadn't just done it.

If system A @(depends-on) system B, both were once compiled,
B was subsequently modified and separately recompiled,
and you'd ask @(ASDF) to compile A again,
then it would not flag B for recompilation, and therefore not flag A.
Indeed, each compiled file in A looked up to date,
when comparing it to the corresponding source file, as @(ASDF) did;
since no @(force) flag from B was issued, @(ASDF) would think it was done.
Bug.

For modules within a system, the problem mostly did not arise,
because the granularity of an @cl{operate} request was a system,
and so there was no way to request compilation of B
without triggering compilation of A.
For the bug to be visible within a system,
it took an external build interruption such as a machine crash or power loss,
or angry programmer killing the process;
in case of such obvious event, programmers would know to rebuild from clean
if experiencing some seeming filesystem corruption.
On the other hand, across systems, the problem arose quite naturally:
working on a system B, compiling it and debugging it,
then working on a client system A, was not only possible but the @emph{usual} workflow.

Seeing no way to fix the bug reliably,
Robert had disabled propagation of the flag between systems,
which at least was predictable behavior.
The usual workaround was for programmers to force recompilation of A using @cl{:force t};
due to another bug (see @secref{Selective_System_Forcing}),
this was actually recompiling everything,
thus eschewing any other such issue in the current session.
The problem, when diagnosed, was easily solved in wetware.
Except of course it wasn't always easy to diagnose,
resulting in hours wasted trying to debug changes that didn't happen,
or worse, to committing bugs one was not seeing to a shared repository,
and having other programmers try to figure out why their code stopped working
after they updated their checkout.

@subsection[#:tag "timestamps"]{@emph{Timestamp} Propagation}

@(ASDF) should have been propagating timestamps,
not just force flags for whether recompilation was needed in the current session!
So we painfully modified the existing algorithm
to support timestamps rather than a flag.

As for @(do-first) dependencies such as loading a file,
we would stamp a @(load-op) not with the time at which the file was loaded,
but with the timestamp of the file being loaded.
As a side benefit, this wholly eliminated the previous need for kludges
to avoid clock skew between the processor clock and the fileserver clock
(though not clock skew between multiple file servers used during the build).

This wasn't enough, though. To wholly get rid of @(do-first),
we had to distinguish between actions that were done in the current image,
versus actions that weren't done in the current image,
but that might still be up-to-date, because their effects were all in the filesystem.
Therefore, when examining an action, we must separate the propagated timestamp
from a non-propagated flag telling whether the action needs to be done in the current image or not.
The generic function @(compute-action-stamp) looks at the dependencies of an action,
its inputs and outputs on disk, and possible stamps from it being done earlier in the current image,
and returns a stamp for it and a flag for whether it needs to be done (or redone) in the current image.
Thus, if a compiled file is up-to-date on disk and an up-to-date version was loaded,
the @(compute-action-stamp) function will return its timestamp and @cl{t} (true);
if the file is up-to-date on disk but either it wasn't loaded yet or an outdated version was loaded,
the @(compute-action-stamp) function will return its timestamp and @(nil) (false);
if the file is missing or out-of-date on disk, then no up-to-date version could be loaded yet,
and @(compute-action-stamp) will return an infinity marker and @(nil).
The infinity marker (implemented as boolean @cl{t}) is so that no timestamp is up-to-date in comparison,
and corresponds to the force flag of @(ASDF1).
A negative infinity marker (implemented as boolean @(nil)) also serves to mark as no dependency.@note{
  Interestingly, this @(compute-action-stamp) could be very easily updated
  to use cryptographic digests of the various files instead of timestamps,
  or any other kind of stamp.
  Because it is the only function for which the contents of stamps is not opaque,
  and is a generic function that takes a plan class as parameter,
  it might be possible to override this function either for a new plan class
  and make that the @cl{*default-plan-class*}), without destructively modifying any code.
  However, this hasn't been tested, so there's probably a bug lurking somewhere.
  Of course, such a modification cannot be part of the standard @(ASDF) core,
  because it has to be minimal and ubiquitous and can't afford to pull a cryptographic library (for now),
  but an extension to @(ASDF), particularly one that tries to bring determinism and scalability,
  could use this very simple change to upgrade from timestamps to using a peristent object cache
  addressed by digest of inputs.
}
(Of course, @(do-first) would come back with a vengeance, see below @secref{Needed_In_Image}).

Then, we started to adapt @(POIU) to use timestamps.
@(POIU) is an @(ASDF) extension, originally written by Andreas Fuchs,
that maintains a complete action graph to compile in parallel (see @secref{Action_Graph}).
However, our attempt to run the modified @(POIU) would fail,
and we'd be left wondering why, until we realized
that was because we had previously deleted what looked like an unjustified kludge:
@(POIU), in addition to the dependencies propagated by @(ASDF),
was also having each node in the action graph
depend on the dependencies of each of its transitive parents.
Indeed, the loading of dependencies (both @(in-order-to) and @(do-first))
of a component's parent (and transitively, ancestors),
were all implicitly depended upon by each action.
In an undocumented stroke of genius, Andreas Fuchs
had been making explicit in the DAG the implicit sequencing done by traverse!
However, these parent dependencies were being passed around
inefficiently and inelegantly in a list, updated using @cl{append}
for a quadratic worst time cost.
This cost wouldn't explode as long as there were few systems and modules;
but removing the magic sequencing of @(traverse)
to replace it a different and inefficient kluge wasn't seem appealing,
especially after having optimized @(traverse) into being of linear complexity only.

And the solution was of course to explicitly reify those implicit dependencies
in the action graph, making it a complete explicit model.

@subsection{Prepare operation}

And so we introduced a new operation, initially called @cl{parent-load-op} (2.26.14),
but eventually renamed @(prepare-op) (2.26.21),
corresponding to the steps required to be taken
in preparation for a @(load-op) or @(compile-op),
namely to have completed a @(load-op)
on all the sideway dependencies of all the transitive parents.

Now, unlike @(load-op) and @(compile-op)
that both were propagated @emph{downward} along the dependency graph,
from parents to children,
@(prepare-op) had to be propagated @emph{upward}, from children to parents.
And so, the @(operation) class had a new special subclass @(upward-operation),
to be specially treated by @(traverse)...

Or better, the propagation could be moved entirely out of @(traverse)
and delegated to methods on @(component-depends-on)!
A mixin class @(downward-operation) would handle
the downward propagation along the component hierarchy
for @(load-op), @(compile-op) and the likes,
whereas @(upward-operation) would handle @(prepare-op);
@(sideway-operation) would handle the dependency
from @(prepare-op) to the @(load-op) of a component's declared @(depends-on),
whereas @(selfward-operation) would handle the dependency of @(load-op) and @(compile-op)
to @(prepare-op).
Thanks to CLOS multiple inheritance and double dispatch, it all fell into place (2.26.21).

For instance, the good old downward propagation was implemented by this mixin:
@clcode|{
(defclass downward-operation (operation) ())
(defmethod component-depends-on
     ((o downward-operation)
      (c parent-component))
  `((,o ,@(component-children c))
    ,@(call-next-method)))
}|
The current version is more complex,
with all of nine (full-length) lines of code plus comments and doctrings,
for additional backward compatibility and extensibility, but this gives the gist:
The action of a downward operation on a parent component
depends on the same operation @cl{,o}
on each of the component's children,
followed by other dependencies from other aspects of the action.
Had backward-compatibility not been required,
the function would have been called @(action-depends-on),
and its @cl{method-combination} would have been @cl{append},
so that it wouldn't be necessary to write that @cl|{,@(call-next-method)}|
in each and every method definition.
But backward-compatibility was required.

In any case, classes like @(load-op) and @(compile-op) just inherit from this mixin,
and voilà, no need for any magic in @(traverse),
which at that point had been broken down in neat small functions,
none more than fifteen lines long.
If anything, some complexity had been moved to the function @(compute-action-stamp)
that computes timestamps and deals with corner cases of missing inputs or missing outputs,
which was 48 heavily commented lines of code (67 as of 3.1.1),
just slightly more than the misdesigned function @(operation-done-p)
it was superseding.

Now everything was much cleaner. But of course, it was a mistake to call it a victory yet,
for @(do-first) came back to enact revenge for my killing it;
and once again, Andreas Fuchs had prophesized the event and
provided a weapon to successfully defend against the undead.

@subsection{Needed In Image}

Former @(do-first) dependencies of an action used to not partake in the forcing,
but were nevertheless to be done before the action.
Reminder: in practice, they were the loading of dependencies before compiling a file.
With the new, saner, action graph, they were now regular dependencies;
the only difference was that they don't contribute anything to the action stamp (and thus forcing)
that wasn't already contributed by the action creating the file they loaded.
Still, they must be done, in order, in the current image.

Now, this last constraint was utterly defeating the purpose of some bundle operations,
where the whole point of using a bundle fasl was to not have to load the individual fasls
(see @secref{bundle_operations}).
In the old @(ASDF1) model, the @(load-fasl-op) @(depends-on) @(fasl-op)
which @(depends-on) a lot of individual @(compile-op),
which only @(do-first) the @(load-op) of their dependencies.
Therefore, if the individual files look up to date, no individual loading takes place.
Except of course @(ASDF1) is incapable of detecting that files are out of date
when the system's dependencies have changed.
In the new @(ASDF3) model, the fact that the @(compile-op) actions are out of date
is detected thanks to recursing through their @(prepare-op) and @(load-op) dependencies;
but with the naïve approach to building a plan that always load dependencies,
this causes all those individual @(load-op) to be issued.

The solution was again suggested by @(POIU).
For the sake of determining whether an action could be performed in parallel in a fork,
or had to be done in the image of the main process,
@(POIU) had introduced a predicate @(needed-in-image-p);
the notion was actually suggested by the old method @(operation-done-p)
from Dan Barlow's original @(ASDF1).
If an action has any @(output-files), then
@(ASDF) considers that the operation is worth it for its output,
and should have no meaningful or reliable side-effects in the current image;
it thus counts as @emph{not} @(needed-in-image-p).
If on the other hand, an action has no @(output-files), then
@(ASDF) considers that the operation is worth its side-effects in the current image;
it thus counts as @(needed-in-image-p).

What the new @(traverse-action) action had to do (2.26.46),
was to associate to each visited node a status depending on whether or not
the action was needed in the current image.
When visiting an action in a context where the goal is not (known to be) needed in image,
or where the action is intrinsically not @(needed-in-image-p)
because its value resides in filesystem side-effects,
then all the action's dependencies would themselves visited
in a mode where the goal is not (known to be) needed in image.
In that mode, the action is consulted for its timestamp,
but won't be included in the plan as long as it is up-to-date.
However, if the action is found to be out of date,
before it would be planned, all its dependencies will be visited a second time,
in a mode where the goal is known to be needed in image.
The top level action is initially requested with a goal of being needed in image,
which only applies of course if it's itself a @(needed-in-image-p) action.@note{
  The principle of visiting the action graph multiple times
  would be generalizable to other situations, and the maximum number of visits of a given node
  is the height of the semi-lattice of states in which the traversal is considered.
  For instance, in a @tt{blaze} extension I wrote to support CL,
  visited files would be upgraded between being not needed in image, needed loaded as cfasl,
  needed loaded from source, or needed loaded from fasl.
  The same technique could be used to improve XCVB.
}

The code was then refactored by introducing an explicit plan object (2.26.47),
to hold this action status information during the planning phase,
as distinguished from the execution phase
during which action status refers to what is actually done.

@subsection{Why Oh Why?}

Some will ask: how did @(ASDF) survive for over 11 years
with such an essential birth defect?
Actually, the situation is much worse:
the very same the bug was present in @(mk-defsystem), since 1990.
Actually, it looks like the bug might have been
as old as the original @(DEFSYSTEM) from the 1970s.
The various proprietary variants of @(defsystem)
from Symbolics, Franz, and LispWorks all include fixes to this issue;
however, the variants from Symbolics and Franz, require
using a @emph{non-default variant} @cl{:definitions} of @cl{:depends-on};
and the one from Franz still has bugs in corner cases.

This is all very embarrassing indeed:
in the world of C programming,
@(make) solved the issue of timestamp propagation, correctly, since 1976.
Though historical information is missing at this point,
it seems that the original @(DEFSYSTEM) was inspired by this success.
Even in the Lisp world the recent @(faslpath) and @(quick-build),
though they were much simpler than any @(defsystem) variant,
or quite possibly @emph{because} they were much simpler,
got it right on the first attempt.
How come the bug was not found earlier?
Why didn't most people notice?
Why didn't the few who noticed @emph{something} care enough
to bother fixing it, and fixing it good?

We can offer multiple explanations to this fact.
First, to put the bug back in perspective,
an analogy in the C world would be that
sometimes when a @tt{.h} file is modified in a different library
(and in some more elaborate cases, in the same library,
if it's divided in multiple modules),
the @tt{.c} files that use it are not getting recompiled.
Put that way, you find that most C builds actually have the same problem:
many simple projects fail to properly maintain dependency information
between @tt{.c} and @tt{.h} files, and even those that do
don't usually account for header files in other libraries,
unless they bother to use some automated dependency analysis tools.
Still, the situation is somewhat worse in the CL world,
first because every file serves the purpose of both @tt{.c} and @tt{.h}
so these dependencies are ubiquitous;
second because because software is both much more amenable to modification,
indeed, dynamic interactive modification,
so these changes happen more often;
thirdly because software libraries are indeed often lacking in finish,
because tinkering with the software is so easy that users are often @emph{expected} to do so
rather than have all the corner cases painfully taken care of by the original author.
In C, the development loop is so much longer,
jumping from one library to the next is so expensive,
that building from clean is the normal thing to do
after having messed with dependencies,
which often requires reconfiguring the software to use a special writable user copy
instead of the read-only system-provided copy.
The price usually paid in awkwardness of the development process in C
is vastly larger than the price paid to cope with this bug in Lisp.
Users of languages like Python or Java, where installation and modification
of libraries is more streamlined by various tools, do not have this problem.
But then their programs don't have any kind of macros,
so they lose, a lot, in expressiveness, as compared to Lisp,
if admittedly not to C.

Second, most Lisp programmers write software interactively in the small,
where the build system isn't a big factor.
This is both related to the expressive power of the language,
that can do more with less, and to the size of the community, which is smaller.
In the small, there are fewer files considered for build at a time;
only one file changes at a time, in one system, on one machine, by one person,
and so the bug isn't seen often;
when a dependency changes incompatibly,
clients are modified before the system is expected to work anyway.
Those who have written large software in the past tended
to use proprietary implementations,
that did provide a fix to that bug.
ITA Software was one of the few companies using @(ASDF)
to write really large software, and indeed,
it's by managing the build there that we eventually cared enough to fix @(ASDF).
In the mean time, and because of all the issues discussed above,
the policy had long been to build from clean before running the tests
that would qualify a change for checkin into the code repository.

Third, and relatedly, Lisp has historically encouraged an interactive style of development,
where programs compile very fast, while the programmer is available at the console.
In the event of a build failure, the programmer is there to diagnose the issue, fix it,
and interactively abort or continue the build,
which eliminates most cases of the bug due to an externally interrupted build.
Utter build failures and interruptions are obvious,
and programmers quickly learn that a clean rebuild is the solution in case of trouble.
They don't necessarily suspect that the bug is the build system,
rather than in their code or in the environment, especially since the bug usually shows
only in conjunction with such other bug in their code or in the environment.

Fourth, indeed for the @(defsystem) bug to show
without the conjunction of an obvious other bug,
it takes quite the non-colloquial use of "stateful" or "impure" macros,
that take input from the environment
(such as the state of packages or some special variables)
into account in computing their output expansions.
Then, a change in a dependency can lead in expecting a change in the macro expansion,
without the client site being modified, and that change will fail to take place
due to the @(defsystem) bug.
But most macros are "stateless", "pure", and have no such side-effect.
Then, a meaningful change in a macro defined in a dependency usually requires
a change in the client file that depends on it,
in which case the client will be recompiled after that change and no bug will be seen.
The one case that the programmer may notice, then,
is when the macro interface didn't change,
but a bug in its implementation was fixed,
and the clients were not recompiled.
But the programmer is usually too obsessed with his bug and fixing it
to pay close attention to a bug in the build system.

@subsection{The Aftermath}

At the end of this epic battle against a tiny old bug,
@(ASDF) was found completely transformed:
much more sophisticated, yet much simpler.
For instance, the commented @(traverse-action) function is 43 lines long,
which is still significantly less than the original @(traverse) function.
Reading the @(ASDF3) source code requires much less figuring out what is going on,
but much more understanding the abstract concepts
— at the same time, the abstract concepts are also well documented,
when they were previously implicit.

Interestingly, this new @(ASDF3) can still meaningfully be said to be "but"
a debugged version of Dan Barlow's original @(ASDF1).
Dan probably had no idea of all the sophistication
required to make his @tt{defsystem} work @emph{correctly};
if he had, he might have been scared and not tried.
Instead, he was daringly experimenting many ideas;
many of them didn't pan out in the end,
but most were clear improvement on what preceded,
and he had quite a knack for finding interesting designs.

And the design of @(ASDF) is undoubtly interesting.
It masterfully takes advantage of the multiple inheritance and multiple dispatch
provided by CLOS to deliver in a thousand lines or so
a piece of software that is extremely extensible,
and unlike anything written in languages missing these features.
@(ASDF3) is ten times this thousand lines,
because of all the infrastructure for robustness and portability,
because of all the burden of hot upgrade and backward compatibility,
because of all the builtin documentation and comments,
and because of all the several extensions that it bundles.
But the core is still a thousand lines of code or so,
and these extensions, built on top of this core,
illustrate its expressive power,
as well as provide essential services to Lisp programmers.

In the end, we find that @moneyquote{software designs are discovered},
not created @emph{ex nihilo}.
Dan extracted a raw design from the mud of conceptual chaos,
and gave birth to @(ASDF).
Tasked with maintaining the software, I refined the design,
removing the mud, until what was left was a polished tool.
I certainly won't claim that my task was harder or more worthwhile than his,
or that @(ASDF3) is a jewel among build systems.
But I believe that it has a clean and original design worth explaining, yet
that neither Dan Barlow nor I can honestly be said to have designed this design;
we merely stumbled upon it.

@(generate-bib)
