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
may now be written @emph{portably} in @(CL):@note{
  @(CL) is a language defined in the ANSI standard X3.226-1994.
  It is a multi-paradigm, dynamically-typed high-level language.
  Though it is known for its decent support for functional programming,
  its support for Object-Oriented Programming
  is what remains unsurpassed still in many ways,
  and sadly few languages even attempt to match its syntactic extensibility
  or support for interactive development.
  It was explicitly designed to allow for high-performance implementations;
  some of them, depending on the application,
  may rival with compiled C programs in terms of speed,
  usually far ahead of "scripting" languages and their implementations.
}
@XXX{
  Over a dozen actively maintained implementations
  purport to conform to the ANSI @(CL) standard, plus a few unmaintained ones.
  No single implementation is the best, shiniest, smallest and fastest,
  cheapest, and ported to the most platforms.
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
It allows delivering these programs as standalone executable files,
or, with the companion script @cl{cl-launch} (see @secref{cl-launch})
as light-weight scripts that can be run unmodified
on many different kinds of machines each differently configured.
Previously, key parts of a program had to be configured to match
one's specific @(CL) implementation and specific software installation paths.
Now, all of one's usual Unix scripting needs can be entirely fulfilled by @(CL),
benefitting from its efficient implementations, hundreds of software libraries, etc.

In this article, we will describe
the various achievements and failures of @(ASDF) development,
and try to extract lessons from the adventure.
To make things easier to programmers
who may or may not have used the tool at some point,
we break down improvements by major milestones.
For each improvement, we provide a rationale for the change and
a simple example of how it enables
better programming practices or more powerful programs.

In part one @secref{what_it_is}, we will give some context about what @(ASDF) is,
how it compares to what is done in the C world,
what is the historical context in which it was designed,
and what it initially brought to the @(CL) community.

In part two @secref{asdf2},
we will describe the innovations in @(ASDF2).

In part three @secref{asdf2.26},
we will describe in  innovations in @(ASDF 2.26).

In part four @secref{asdf3},
we will describe in  innovations in @(ASDF 3).

In part five @secref{asdf3.1},
we will describe in  innovations in @(ASDF 3.1).

In part six @secref{evolving}, we will discuss the challenges
of making acceptable changes to a piece of community software
that is not only used by hundreds of developers,
but also extended in various ways by tens of them.
What it means to be backward compatible is not
keeping all the old bugs that people have come to rely on;
it is providing a smooth upgrade path to better software
despite weak synchronization.

In our conclusion @secref{conclusion},
we explore the improvements we had to make
and the way we could make them,
illustrating what is wrong and is right about @(CL),
so that growers of other programming languages can learn from the experience.

@section[#:tag "what_it_is"]{What @(ASDF) is}

@subsection{@(ASDF): Basic Concepts}

@subsubsection{Components}

@(ASDF) is a build system for @(CL):
it helps developers divide software into a hierarchy of @bydef{component}s
and automatically generates a working program from all the source code.

Top components are called @bydef{system}s in an age-old Lisp tradition,
while the bottom ones are source files, typically written in @(CL).
Users may then @(operate) on these components with various build @bydef{operation}s,
most prominently compiling the source code (operation @(compile-op)) and
loading the result into the current Lisp image (operation @(load-op)).

Several related systems may be developed together
in a same source code @bydef{repository}.
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
(usually, a @(CL) source file),
or a @bydef{module} that may recursively contain other components.
Modules may or may not directly fit the filesystem directory hierarchy.

Further, each component may explicitly declare
a @bydef{dependency} on other components:
a component @(depends-on) other components
that contain definitions for
packages, macros, variables, classes, generic functions,
and any functions that it uses at compile-time,
notably during the read and macro expansion phases.

@subsubsection{Example system definitions}

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
that which the @(CL) packages@note{
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
The DAG defines a partial order, whereby
each action must be @bydef{perform}ed but only after
all the actions it (transitively) depends on have already been performed.

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
a total order that is a linear extension of the partial order of dependencies,
ensuring that before the action that compiles or loads a component is performed,
all the actions that compile and load its declared dependencies have themselves been performed,
including all their own transitive dependencies.

It is of course possible to reify the complete direct acyclic graph of actions
rather than a linear extension of it.
Indeed, Andreas Fuchs in 2006 wrote a very small but quite brilliant @(ASDF) extension
called @(POIU), the "Parallel Operator on Independent Units".
@(POIU) compiles files in parallel on Unix multiprocessors using @tt{fork},
while still loading them sequentially into a main image, minimizing latency.
François-René Rideau later rewrote @(POIU), making it
both more portable and simpler by, co-developing it with @(ASDF).
Understanding the sometimes bizarre and useless-looking
but actually extremely clever and emminently necessary tricks
by which Andreas Fuchs overcame the limitations and conceptual bugs of @(ASDF)
to build such a complete DAG of actions
led to many aha moments, instrumental when fixing @(ASDF2) into @(ASDF3)
(see @secref{traverse}).

@subsubsection{In-image}

Finally, it is important to note that
@moneyquote{@(ASDF) is an @q{in-image} build system},
just like the build systems that preceded it in the Lisp @(defsystem) tradition:
it compiles (if necessary) and loads software into the current @(CL) image.
For better or worse, this notably differs from the practice in most other languages,
where the build system is a completely different piece of software running in a separate process:@note{
  Of course, it is possible to write build system for @(CL) that compile in other processes;
  we did in the past XCVB @~cite[XCVB-2009].
  As of the wide variety of Lisp dialects beside @(CL),
  they have as many different build systems, often integrated with a module system.
}
On the one hand, it makes it somewhat easier to extend the build system.
But on the other hand, it puts great pressure on @(ASDF) to remain minimal.

Qualitatively, @(ASDF) must be delivered as a single source file
and cannot use any external library,
since it itself defines the code that may load other files and libraries.
Quantitatively, @(ASDF) has to be present in all programs being built
and any memory it occupies is consumed for all.
This arguably mattered more in 2002 when @(ASDF) was first released
and was about a thousand line long:
In 2014, it has grown over ten times in size,
but memory sizes have increased even faster.

Still, for all these reasons, @(ASDF) follows the minimalist principle that
@moneyquote{anything that can be provided as an extension
            should be provided as an extension and left out of the core}.
@(ASDF) thus cannot afford to include, say,
management of an advanced persistence cache
indexed by a cryptographic digest of the contents,
or control a distributed network of cross-compiling workers, etc.
However, these features are conceivable as @(ASDF) extensions.

@subsection{Comparison to C programming practice}

Most programmers are familiar with C, but not with @(CL).
It is therefore worth constrasting @(ASDF) to the tools used by C programmers
to provide similar services.

To build and load software, C programmers typically use
@(make) to build the software and @tt{ld.so} to load it.
Additionally, they use a tool like @tt{autoconf}
to locate available libraries and identify their features.
In many ways is these C solutions are
vastly more and better engineered than @(ASDF).
But in other important ways @(ASDF) demonstrates how
a lot of the complexity present in these C systems
is extrinsic make-work that is drastically simplified away
by Lisp's vastly better architecture.

@itemlist[
  @item{
    Lisp makes the full power of runtime available at compile-time,
    so it's easy to write a Domain-Specific Language (DSL) as an extension,
    where only the new functionality needs be defined
    (such as support for defining systems)
    while the rest of the language remains available for the user.
    In C, instead, every utility needs to onerously grow
    an entire domain-specific language from scratch;
    since the domain expert is seldom also language expert ,(XXX "FIX")
    with resources to do that right,
    this means plenty of mutually incompatible misdesigned
    power-starved misimplemented languages that have to be combined
    through expensive and restricted interprocess communication.
  }
  @item{
    Lisp provides full introspection at run time and compile time alike,
    as well as a protocol to declare @bydef{features}
    and conditionally read code and data based on them;
    therefore you don't need dark magic at compile time
    to detect the features available.
    In C, instead, people resort to
    horribly unmaintainable configuration scripts
    in a hodge podge of shell script, m4 macros, C preprocessing and C code,
    plus often bits of python, perl, sed, etc.
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
    @(ASDF) uses the very same mechanism
    to configure both run time and compile time,
    so there is only one configuration mechanism to learn and to use,
    and no risk of discrepancy between the two.
    In C, completely different and incompatible mechanisms are used
    at run time (@tt{ld.so}) and compile time (an unspecified mess),
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
    the way that is not available in lesser programming languages
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
      was a misdesigned afterthought (copied over from @(mk-defsystem))
      the implementation of which wasn't portable, with too many corner cases.
    }
  }
  @item{
    At the other extreme, a build system for @(CL) could have been made
    that is much simpler and more elegant than @(ASDF),
    if it could have required software to follow some simple organization constraints,
    without much respect for legacy:
    a constructive proof of that is Alastair Bridgewater's @cl{quick-build}
    (or the similar and earlier @cl{faslpath} by Peter Etter),
    being a fraction of the size of the original @(ASDF), which is a fraction of @(ASDF3)'s,
    and with a fraction of the bugs — but none of the generality and extensibility
    (See below @secref{asdf-package-system}).
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

Since 1990, the state of the art in free software @(CL) build systems
was @(mk-defsystem) @~cite[MK-DEFSYSTEM].@note{
  The variants of @(DEFSYSTEM) available
  on each of the major proprietary @(CL) implementations
  (Allegro, LispWorks, and formerly, Genera),
  seem to have been much better than @(mk-defsystem).
  But they were not portable, not mutually compatible, and not free software,
  and therefore @(mk-defsystem) because @(de_facto) standard for free software.
}
Like late 1980s variants of DEFSYSTEM on all Lisp systems,
it featured a declarative model to define a system in terms of
a hierarchical tree of @bydef{components},
with each component being able to declare dependencies on other components.
The many subtle rules constraining build operations
could be automatically deduced from these declarations,
instead of having to be manually specified by users.

However, @(mk-defsystem) suffered from several flaws,
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

@subsection{@(ASDF1): A Successful Experiment}

In 2001, Dan Barlow, a then prolific @(CL) hacker,
wanted a good @(defsystem) variant suitable for his needs,
notably regarding extensibility.
Instead of attempting to modify @(mk-defsystem), at high cost for little expected benefit,
he wrote a new one, @(ASDF):@note{
  In a combined reverence to tradition and joke,
  @(ASDF) stands for "Another System Definition Facility",
  as well as for consecutive letters on a QWERTY keyboard.
}
thus he could abandon the strictures of supporting long obsolete implementations,
and instead target modern @(CL) implementations.
In 2002, he published @(ASDF), made it part of SBCL,
and used it for his popular @(CL) software.
It was many times smaller than @(mk-defsystem)
(under a thousand line of code, instead of five thousand),
much more usable, actually extensible,
and easy to port to other modern @(CL) implementations,
what more with an uncontroversial MIT-style software license.
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
@cl{*central-registry*} with a list directories
in which to look for system definition files,
and maintaining "link farms" in those directories
— and both aspects could be automated.
(See below for how @(ASDF2) improved on that.)

Also, following earlier suggestions by Kent Pitman @~cite[Pitman-Large-Systems],
Dan Barlow used object-oriented style to make his @(defsystem) extensible
without the need to modify the main source file.@note{
  Dan Barlow may also have gotten from Kent Pitman
  the idea of executing a reified plan rather than
  walking the dependencies on the go.
}
Using the now standardized @(CLOS),
Dan Barlow defined his @(defsystem) in terms of @bydef{generic functions}
specialized on two arguments, @(operation) and @(component),
(using multiple dispatch, an essential OO feature unhappily not available
in lesser programming languages, i.e. sadly almost of them —
they make do by using the "visitor pattern").
Extending @(ASDF) is a matter of simply by defining new subclasses
of @(operation) and/or @(component)
and a handful of new methods for the existing generic functions,
specialized on these new subclasses.
Dan Barlow then demonstrated such simple extension with his @cl{sb-grovel},
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
  @moneyquote{@(CL) support for hot upgrade of code may exist
                    but is anything but seamless}.
  These simpler upgrades allow us to simply use @cl{fmakunbound} everywhere,
  instead of having to @cl{unintern} some functions before redefinition.
}
Soon enough, users felt confident relying on bug fixes and new features,
and all implementations started providing @(ASDF2).

These days, you can @cl{(require "asdf")} on pretty much any @(CL) implementation,
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

@subsection{Portability}

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
where @cl{*central-registry*} itself couldn't.
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

There used to be an extension to @(ASDF1) called @cl{asdf-binary-locations}
that fulfilled the same functionality;
but apart from its suffering
from the same lack of modularity as the @cl{*central-registry*},
it also had a chicken-and-egg problem:
you couldn't use @(ASDF) to load it without having at least one
program compiled without @cl{asdf-binary-locations} enabled,
namely @cl{asdf-binary-locations} itself;
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

@subsection{Robustness}

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

@subsection{Performance}

@(ASDF1) performance didn't scale well to large systems,
because Dan Barlow was using the @cl{list} data structure everywhere
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
@cl{(asdf:load-system :foo)}.@note{
  @(load-system) was actually implemented
  by Gary King, the last maintainer of @(ASDF1), in June 2009;
  but users couldn't casually @emph{rely} on it being there
  until @(ASDF2) made it possible in 2010
  for everyone to hot upgrade whatever their implementation was providing.
}

@(ASDF2) provided a portable way to specify pathnames
by adopting Unix pathname syntax as an abstraction,
while using standard @(CL) semantics underneath.
It became easy to specify hierarchical relative pathnames,
where previously doing it portably was extremely tricky.
@(ASDF2) similarly provided sensible rules for pathname types and type overrides.
(See @secref{pathnames}.)
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

@section[#:tag "asdf2.26"]{Features introduced in the @(ASDF2) series}

@subsection{@(defsystem) dependencies}

@(ASDF2) introduced a @cl{:defsystem-depends-on} option to @(defsystem),
whereby a system could declaratively specify dependencies on build extensions.
Before that option, users would imperatively load any extension they need:
in their @tt{.asd} system definition file,
they would for instance evaluate @cl{(asdf:load-system :cffi-grovel)},
before they use @(defsystem) to define their systems.
Indeed, a @tt{.asd} file is just a Lisp source file
that is loaded in a controlled context and may contain arbitrary side-effects;
but such side-effects are frowned upon and
a declarative style is more maintainable,
hence this improvement.

However, this feature was only made be usable in 2.016 (June 2011),
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
Which in a recent @(ASDF) would be more colloquially:
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
  @(CL) possesses a mechanism for continuable errors, @(cerror),
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
his relatively lack of interest in developing @(ASDF)
beyond the point where it got the rest of his software off the ground;
and by contrast the obsession to detail of his successor.

@subsection{Hooking into @cl{require}}

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
    thus trying to "optimize" the easy case with @cl{require}
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
@(ASDF2) added an @cl{:encoding} option to @(defsystem),
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
as witnessed by the automated testing tool @cl{cl-test-grid}. @XXX{Give URL!}

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

@subsection{Hooks around compilation}

A recurrent question to @(ASDF) developers was about how to properly
modify the @(CL) syntax for some files,
without breaking the syntax for other files:
locally giving short nicknames to packages,
changing the readtable, or the reader function, etc.

The answer in 2011 was that it possible to define
a new subclass @cl{my-cl-file} of @(cl-source-file),
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

@subsection{Enforcing user-defined invariants}

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
user code would then specify in their @(defsystem)
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

@section[#:tag "asdf3"]{@(ASDF) 3: A Mature Build}

@(ASDF3) was a complete rewrite of @(ASDF), several times over,
to correctly deal with its core issues.
The unintended result of these rewrites was to turn it into
a much more robust and versatile product than it was:
not only does it cover the robust building of @(CL) software from @(CL),
it also includes runtime software management functionality
and integration both ways with the Unix command line.

@(ASDF3) was pre-released as 2.27 in February 2013,
then officially released as 3.0.0 on May 15th 2013.

@subsection{A Consistent, Extensible, Model}

There was an essential bug at the heart of the @(ASDF) design,
present from day 1.
to fix this bug required a complete rewrite of the core of @(ASDF)
See @secref{traverse}.



to correctly compute timestamps
across an arbitrary action graph;
users may specify how operations do or don't propagate
along the component hierarchy.



@subsection{Understandable Internals}

Before a deep refactoring changing how things were done was possible,
a more shallow refactoring was needed:
reorganizing the source code so that it becomes understandable,
by cutting it in smaller chunks, each with clear dependencies to other chunks.

@subsection{Portability Layer}

Running an external program and getting its output,
or even manipulating pathnames and accessing the filesystem,
were notably hard;
even more complexity was involved to
@emph{robustly} deal with countless corner cases.
@(ASDF3) provides solutions to these issues, and many more.

@(ASDF3) notably brought a user-visible portability layer UIOP.
Abstracting over pathnames, filesystem access;
but also run-program, Lisp image lifecycle, and many other everyday concerns.

@cl{inferior-shell}, that builds upon @cl{uiop:run-program}, can:
@clcode{
  (in-package :inferior-shell)
  (run `(pipe (ps fauwwwwwwwwwwwwx)
              (sort -r -k5)))
}

@subsection{extensibility}

Better operations.

@(POIU) didn't need to override a lot of @(ASDF)'s key infrastructure anymore.


@subsection{configurability}

better supported are conditional compilation, versioning, and more.

@subsection{internal consistency}

@subsection[#:tag "bundle_operations"]{Bundle Operations}

Bundle operations allow to create a single output file
for an entire system or collection of systems.
The most directly user-facing bundle operation is @(fasl-op),
that bundles into a single fasl
all the individual fasls from the @(compile-op) outputs
of each file in a system.
This bundle fasl may then be loaded by operation @(load-fasl-op).@note{
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
  Most @(CL) implementations maintain their own heap with their own garbage collector,
  and then are able to dump an image of the heap on disk,
  that can be loaded back in a new process with all the state of the former process;
  to build an application, you start an small initial image, load plenty of code, dump an image,
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
When came @(ASDF3), it was merged into @(ASDF) itself:
not only did it provide useful new operations,
the way that @(ASDF3) was automatically upgrading itself for safety purposes
would otherwise have broken things badly for ECL users
if the bundle operations weren't bundled with @(ASDF).

In @(ASDF3.1), using @cl{deliver-asd-op} (previously misnamed @cl{binary-op})
you can create both the bundle fasl from @(fasl-op) and a @tt{.asd} file
to use to deliver the system in binary format only.

@subsection{creating standalone executables}

there are several ways to deliver software in a single-file "bundle",
including a single FASL for a system and/or its dependencies, or
(on supported implementations) a standalone executable programs;

@subsection{cl-launch}

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
  is an illustration of how expressive and modular the @(CL) Object System can be.

  Now, @cl{asdf-binary-locations} and other variants of the same idea
  had a bootstrapping issue: the extension had to be specially loaded
  before it could be used, whether as source code or precompiled code,
  otherwise the potential clashes regarding its own compiled file
  would negate any advantage in avoiding clashes for files of other systems.
  @cl{common-lisp-controller} was trying to solve the same issue
  by having managed software installations in a controlled way;
  it failed eventually, because these efforts were only available
  to @(CL) programmers using Debian or a few select other Linux software distributions,
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

@section[#:tag "asdf3.1"]{@(ASDF) 3.1: Lifting Lisp}

@(ASDF 3.1) (March 2014) builds on top of @(ASDF3) to provide new features.

@subsection[#:tag "asdf-package-system"]{@cl{asdf/package-system}}

The @cl{asdf/package-system} extension supports
a one-file, one-package, one-system style of programming.
This style was pioneered by Peter Etter's @cl{faslpath},
and more recently by Alastair Bridgewater's @cl{quick-build}.
@cl{asdf/package-system} is compatible with the latter, not with the former.

Maintainability benefits.
@(ASDF) was re-written in this style as part of @(ASDF 2.27), the initial @(ASDF3) pre-release.
By forcing the user to make dependencies explicit, and forward dependencies especially so,
it encourages good factoring of the code.

@cl{asdf/package-system} is not light weight build like @cl{quick-build},
but portability and robustness of ASDF3.

@XXX{EXAMPLES NEEDED!}

Users who desire compatibility with @(ASDF 3.0) can use the @cl{asdf-package-system} extension,
which is part of @(Quicklisp), with
@verbatim|{:defsystem-depends-on (#-asdf3.1 :asdf-package-system)}|
The @verbatim|{#-asdf3.1}| may be omitted.

@subsection[#:tag "uiop_improvements"]{@(UIOP) improvements}

@(UIOP)'s @cl{run-program} was generalized
to accept redirection of input and error-output as well as of output.
The @cl{inferior-shell} extension knows how to take advantage of it.

A debugged and extended @cl{with-temporary-file}
makes it easy to work with temporary files,
automatically cleaning them up afterwards.

dump-image / restore-image improvements,
ensure-pathname, compile-file*, with-saved-deferred-warnings.

Tested with Google's build system, short circuiting @(ASDF)'s @(defsystem),
but taking advantage of the abstractions in @(UIOP).

The @cl{nest} macro allows to nest arbitrarily many forms
without indentation drifting ever to the right.
This can make for more readable code.

@subsection[#:tag "backwarder_compatibility"]{Backwarder compatibility}

Improved compatibility with extensions written for @(ASDF1) and @(ASDF2).
@(non-propagating-operation) is a mixin to signal to @(ASDF)
that your operation class is not backward.

@(operation) as a base class to inherit from.
Operations that do not propagate, should inherit from the @(non-propagating-operation) mixin.
Disables the backward compatibility mechanism.
Negative inheritance!

For backward compatibility with ASDF 1 and 2,
subclasses of @(operation) implicitly propagate @emph{downward} and @emph{sideways}.
Failure to explicitly inherit from own of the explicitly propagating
or non-propagating classes will result in a @(warning) for now.

  * force-not now takes precedence over force, and systems named in the
    set (represented as an equal hash-table) *immutable-systems* are always
    forced-not, and even their .asd is not refreshed from the filesystem.

  * portability is much improved, with support for the latest GCL, and
    fixes for ABCL, CLISP, ECL, LispWorks, SBCL, XCL, etc.

  * bundle support was refactored; ECL support is fixed;
    image-op added to create heap images for use with e.g. cl-launch.

  * robustness: Test improvements. Added missing dependencies in asdf.asd(!).
    Fixes to version-satisfies (thanks to stassats), to the file-stamp cache.
    Fixes regression from 3.0.2.12 whereby ASDF failed to avoid downgrading.
    Many cleanups. Many explanatory updates to source code comments.

  * run-program tweaked again, notably on Windows, on CLISP, and
    on implementations without a native run-program. On Windows, we
    punt on trying to ensure no final space is ever echo'ed by CMD.EXE " ".

  * provide both "asdf" and "ASDF" to play nicer with various Lisps.

  * upgrading from a sufficiently forward-compatible version (currently: 2.33)
    will be less disruptive of ASDF uses and enhancements in the current image:
    previously loaded systems will not be cleared anymore,
    variables defined with defparameter* will not be reset.
    Punting on upgrades from ASDF 1 for more robust behavior.


@subsection{The Future}

Robert Goldman assumed maintainership in July 2013,
a few months after the release of @(ASDF3).
François-René Rideau remained main developer until release of 3.1.1 in March 2014.

All the known bugs have been fixed, and the regression test suite has swollen,
but there will always be portability issues to fix,
and there is a big TODO file for suggested ways to improve @(ASDF).
It is uncertain whether a new maintainer will take over development.

@section[#:tag "evolving"]{Evolving Code in an Immutable Community}

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

@subsection{Overriding the default pathname extension}

Back in the bad old days of @(ASDF1),
the official recipe, described in the manual,
to override the default pathname type @tt{.lisp} for Lisp source file to
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


@section[#:tag "conclusion"]{Conclusion: Lessons for Language Growers}

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

While writing this article, I had to revisit many concepts and pieces of code,
which led to many bug fixed and small refactorings;
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

The minor annoyances were that a change in the @tt{.asd} system definition file
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
except it was Pandora's Box of bigger issues,
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
Each actions consist of an operation on a component;
for a simple @(CL) system with regular Lisp files,
these actions will be and @(compile-op) for compiling the code in the component,
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
  but an extension to @(ASDF), particularly one that who try to bring determinism and scalability,
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

And the solution was of course to explicitly reify in the action graph
those implicit dependencies...

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
the function would have been called @cl{action-depends-on},
and its @cl{method-combination} would have been @cl{append},
so that it wouldn't be necessary to write that @cl|{,@(call-next-method)}|
in each and every method definition.
But backward-compatibility was required.
As they say, @moneyquote{if it's not backwards, it's not compatible}.

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
  The principle of visiting the action graph multiple time
  would be generalizable to other situations, and the maximum number of visits of a given node
  is the height of the semi-lattice of states in which the traversal is considered.
  For instance, in a @tt{blaze} extension I wrote to support @(CL),
  visited files would be upgraded between being not needed in image, needed loaded as cfasl,
  needed loaded from source, or needed loaded from fasl.
  The same technique could be used to improve XCVB.
}

The code was then refactored by introducing an explicit plan object (2.26.47),
to hold this action status information during the planning phase,
as distinguished from the execution phase
during which action status refers to what is actually done.

@subsection{Why Oh Why?}

How did @(ASDF) survive for 11 years with such an essential birth defect?
Worse: very same the bug was present in @(mk-defsystem), since 1990.
It might have been as old as the original @(DEFSYSTEM) from the 1970s.
The proprietary variants of Genera, Franz, and LispWorks have fixes;
however, on Genera, the fix requires
@emph{using a non-default variant} @cl{:definitions} of @cl{:depends-on};
on LispWorks, it still has bugs in a corner case.

Why didn't most people notice?
Why didn't the few who noticed something care enough to bother fixing it?

Because...
Genera: one machine.
Lisp style: significant macro changes usually require client changes, too...
Interactive style: user is there to restart.
Programming in the small: fewer big system dependencies.
@(XXX)

@subsection{The Aftermath}

At the end of this epic battle against an tiny old bug,
@(ASDF) was found completely transformed,
and both much more sophisticated, yet its code much simpler.
For instance, the commented @(traverse-action) function is 43 lines long,
which is still significantly less than the original @(traverse) function.
Reading the @(ASDF3) source code requires much less figuring out what is going on,
but much more understanding the abstract concepts
— but the abstract concepts are also well documented,
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
But I believe that it has a clean and original design worth explaining, and
that neither Dan Barlow nor I can honestly be said to have invented it.

@(generate-bib)
