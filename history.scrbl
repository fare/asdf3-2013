#lang scribble/sigplan @nocopyright

@(require scribble/base
          "utils.rkt")

@title{Some @(ASDF) History}

This section tells the history of @(ASDF),
starting from early build systems.

You should probably skip this section
unless you're interested in the process of technical discovery within that narrow topic.


@section{Build Systems}

In the late 1970s, machines were getting bigger,
software was getting larger, and Common Lisp programmers,
like other programmers,
were starting to feel the need for some small piece of software
to help them build and maintain large pieces of software.

The obvious solution to managing the complexity of building large software
was to break it down into smaller parts.
Instead of one big source file containing the entire program,
the program could be divided into several independent source files.
If the break down was done according to proper conceptual criteria,
a programmer only had to master the internals of one file,
plus the interfaces of a few other files.
Source files (human-readbable and writable text)
are each compiled and assembled into an object file;
object files (binary, meant for machines, not humans)
are (for Unix) linked into a library or executable file,
or (for traditional Lisp) loaded into the current image;
the resulting program can then used to process some data
that may itself result in more "source" or "object" code, etc.

What more, since the same concepts recur in a lot of programs,
some of these files that implement said concepts
can be useful in more than one one program.
These files may then collected into reusable entities.
In Unix parlance, they are called libraries when they are only passive code,
or packages when they may also contain programs, background services, etc.
In the Lisp tradition, they are indiscriminately called systems,
for a system may take any or all of these roles within the Lisp image.

Now, to manage the many steps needed to build the complete program,
one may start by writing a build script
that goes through all the steps, one at a time.
But such a script gets harder to write and to slower run
as the program gets larger,
and is a pain to maintain as the parts that constitute the program change,
as well as the relationships between those parts.

That's where @emph{build systems} come into play:
it lets programmers maintain a more "declarative" description
of the parts that constitute the program
and how they are related to each other,
one that is more suited to human modification,
without requiring extraneous redundancy.
From this description, it will:
@itemlist[
@item{
  locate matching source code, interface definitions
  and/or pre-compiled object code
  for each library needed to build the current program;
}
@item{
  build files in a correct order such that
  if some computations depend on the results of other computations,
  those that are being depended on
  are all completed before those that depend on them;
}
@item{
  avoid unnecessary expensive computations,
  so that when each programmer makes a small change,
  only the computations that transitively depend on that change are updated;
}
@item{
  possibly take advantage of available hardware
  to dynamically schedule simultaneous computations,
  saving time as compared to a static sequential build plan.
}]

When building a program may involve creating intermediate programs
and processing data before the final program is complete;
when the system is large and slow to build from scratch,
but changes made by any individual programmer are relatively small and
only involve a few step to rebuild the entire system,

A good build system will create the final program from parts
without the user having to specify in detail each and every step;
it will also track the changes in which files to recompute in which order
when some of the ultimate inputs have been modified,
or some of the intermediate or final outputs have been invalidated;
and it will thus allow to rebuild the software incrementally,
without requiring a build from scratch every time.

@section{DEFSYSTEM}

In the now dominant paradigm of Unix, the answer was @tt{make}.
In the world of Lisp, it was @cl{DEFSYSTEM}.

With @tt{make}, programmers specify rules
which outputs depend on which inputs, and have as "production"
the commands to be run to compute missing or outdated outputs
(such as a @tt{.o} object file)
from the inputs
(such as the corresponding @tt{.c} source file
and the @tt{.h} headers it uses).
To allow for some ability for programmers
to abstract over similar such computations,
and for users to provide parameters specifying their local configuration,
@tt{make} itself includes a very limited text-substitution language.
Any actual computational expressive power to describe
and achieve the computations themselves resides outside of @tt{make} itself,
and inside the external Unix commands that are invoked via the Unix shell,
itself having a slightly less limited
but still extremely bad programming language based on text substitutions.
On the other hand, @tt{make} was not tied to compiling C code,
and soon enough was used and abused to drive all kinds of build-like tasks
modelled as things to do that could reuse previous results if available
or compute fresh results if those available were out of date.

In olden days, the Lisp @cl{DEFSYSTEM} was somewhat similar to @tt{make},
with programmers writing rules about what to do when certain events happened:
when some source file has changed, recompile it;
when a compiled file has changed, reload it; etc.
However, being tailored to Lisp, it gave a special status
to the common @emph{operations}
that were compiling source files to FASL (FASt Loading files),
then loading the FASL into the current image.
This required none of the zillions of configuration options
that are inflicted upon C programmers,
though for portability you might have to use the @cl{#+feature} syntax
for conditional compilation inside their files —
and since @cl{DEFSYSTEM} itself wasn't standardized,
they might need a slightly different one on each supported implementation.
Specifying rules triggered on change events
gave a level of abstraction similar to @tt{make},
and there was no need for a bad text substitution language,
since for any abstraction need the programmer could use arbitrary Lisp code,
for @cl{DEFSYSTEM} is just a small set of macros and function
within the current image.

Eventually, Lisp programmers found that writing individual rules
was quite tedious since there were two related operations on each Lisp file,
as contrasted to one per C file for @tt{make},
which made for double effort,
and was even more effort if you wanted to distinguish
between various kinds of dependencies
Therefore, instead of resorting to text matching and substitution like @tt{make},
@cl{DEFSYSTEM} instead resorted to a more declarative approach
of specifying dependencies between Lisp components,
from which all the many rules could be automatically deduced.
This made system description even simpler, but even more specific to Lisp.
(Some proposal even relied on automatically deducing the dependencies
when first compiling a file, the way @hyperlink["http://www.vestasys.org/"]{VESTA}
later did in a general setting on Unix;
but this likewise required special non-portable support from the Lisp Machine,
and didn't survive to this day.)

As compared to Unix solutions, the Lisp has a distinct flavor:
Lisp has plenty of tiny, domain-specific languages,
each filling its intended purpose by modelling the issue as a closed model,
yet able to escape to the same uniform general-purpose language
or be automatically generated using dynamic evaluation.
Unix has plenty of small semi-open languages,
each growing a mess of misdesigned language features
to accommodate of the impossibility to escape to a real language,
with the better ones being abused in various way beyond their original purpose.

Yet in the end, the Unix guys won.
Expensive Lisp Machines disappeared in favor of cheap hardware.
Commercial Lisp implementations dwindled.
Eventually, a new generation of free software Lisp hackers emerged,
but by that time, the Lisp community was in shambles;
there was little in terms of an active library ecosystem;
Lisp was playing catchup with a new generation of "scripting languages"
in terms of being able to interface
with a growing wealth of libraries and services on the Internet.
There was no obvious, simple, practical way
for Lisp hackers to sharing software;
each expensive commercial implementation had its own @cl{defsystem},
incompatible enough with the others,
and the one freely available @cl{defsystem} implementation
was buggy and clunky and tailored to obsolete implementations.
All of them were supposing your were running in a self-contained Lisp world,
and they didn't @emph{Play Nice With Unix}
which is what modern Lisp programmers needed,
in @hyperlink["http://www.cliki.net/Daniel%20Barlow"]{Dan Barlow}'s words.

Dan was a notable contributor to SBCL;
he was known as the author of araneida (a Lisp HTTP server)
and Cliki (a wiki in Lisp), and poured an incredible amount of energy
getting things @emph{working} on many fronts in the Lisp community,
without caring at all about getting them @emph{perfect},
indeed embracing imperfection
(to the point of naming his own domain telent.net with a deliberate typo),
where others like me were procrastinating on perfectionism.
That's when he wrote @(ASDF), and on top of it @(ASDF-Install),
as his attempt and contribution to making Lisp libraries
easier to write and distribute.

@section{@(ASDF)}

When Dan Barlow initially wrote @(ASDF) in 2002,
he didn't create it out of a vacuum;
he was basing his software upon upon existing designs.
Dan didn't perfectly understand the previous software,
and @emph{what} an ideal replacement should be;
instead, he explored a new design for @emph{how} to write the software,
and discovered great practical improvements.

His main inspirations were the then popular @cl{mk-defsystem} and
an old article by Kent Pitman.
@cl{mk-defsystem}, like late 1980s variants of @cl{DEFSYSTEM}s,
and unlike the early 1980s variants thereof,
notably sported a declarative model wherein
source files were organized in a recursive hierarchy of components
declaring dependencies on other components,
and operations to be performed on these components
were inferred from that the specified model.
[Systemd vs upstart]

with inferred actions based on declared dependencies,
rather than a fixed-depth list of components made of files,
with explicit operations on components triggered by events on other components.
Kent Pitman's @hyperlink["http://www.nhplace.com/kent/Papers/Large-Systems.html"]
{The Description of Large Systems} was proposing to structure
such software in an object-oriented way.
Dan then had two main innovations.

His first major innovation was the clever use of @cl{*load-truename*},
which significantly removed the need for users of Lisp libraries to go
through a pathname configuration step for each and every library.
Before that, each system's @tt{defsystem} file had to be edited,
or required you to setup a variable or logical pathname before you loaded it.
Now, a Lisp developer would simply do a one-time ever configuration
of the @cl{asdf:*central-registry*} in his Lisp initialization file
(e.g. @cl{~/.sbclrc}),
and libraries could be seen by managing a "farm"
of symlinks in a registered directory,
each pointing to an installed @tt{.asd} file.

The Lisp approach here may seem odd to non-Lispers,
but I will dispute any straight accusation of backwardness.
Indeed, users of others languages rely on a variety
of technical or social tools to manage their software:
environment variables, ad-hoc configuration steps before you build
(with horrors like @tt{autoconf}, @tt{automake} or @tt{pkgconfig}),
or someone else having done the job and built a package for
one of many software "distributions",
resulting in software being installed in standardized locations.
Whatever ease of installation they have achieved came at a heavy cost,
albeit a social cost diluted by large numbers of users.
And it is typical indeed of the Lisp programmers
how their cost structure is different:
their social costs are higher, with fewer users,
whereas they have a higher tolerance for simple technical solutions.

Dan Barlow's second innovation was in his use of
the modern Common Lisp Object System to implement his @cl{defsystem}.
He had two complementary object hierarchies,
@cl{operation} and @cl{component}, and his code was a collection
of generic functions taking operation and component as arguments,
taking advantage of CLOS's multiple-dispatch to both vastly automate
some of the bookkeeping and allow for extensibility.
Thanks to method combinations, he could provide sensible defaults
for a lot of methods such that you needed a minimal amount
of new definitions to extend the system,
and you didn't have to modify the original @cl{defsystem} itself
to have it take into account new type of components.
Dan himself demonstrated these capabilities by writing @cl{SB-GROVEL},
which generated C code that generated Lisp files
to automatically and portably create Lisp interfaces
to C functions and data structures,
all of it under the control of @(ASDF)'s @cl{defsystem}.

Dan Barlow's final release was only 919 lines of heavily commented code.
That's much, much less than any equivalent implementation of @tt{Make} in C
(or even than @cl{mk-defsystem}, for the matter, though
@cl{mk-defsystem} did provide portability to a lot of old platforms
that @(ASDF) didn't care to support).
Being so much shorter than Make is possible because
@(ASDF) doesn't have to reinvent its own limited-purpose programming language
on top of an existing low-level programming language;
it can just bathe in the Lisp environment.
Yet, within those few puny lines of code,
although his primary goal had always been to write it
for his own use on top of SBCL,
he nevertheless supported seven different Lisp implementations.
@(ASDF) may have had bugs and several features begging to be redone better,
for the main part it worked,
and it satisfied hundreds of Lisp hackers around the world
who quickly adopted it, and it soon deservedly became
the @(de_facto) standard for building and exchanging Lisp libraries.

@(ASDF-Install)

@section{@(ASDF) Issues}

By 2004, Dan Barlow had ceased being involved as much
in programming in Common Lisp, and eventually stopped completely.
Many hackers stepped in to help support this practical piece of infrastructure.
Christophe Rhodes notably took charge of @(ASDF), then after him Gary King;
but both eventually burned out, and neither
was trying to make any deep change to @(ASDF) or to address its design issues.

Meanwhile, victim of its own success, @(ASDF) had attracted more users,
who found that while @(ASDF) fulfilled their basic needs,
it also had bugs and quirks that were not being addressed,
while sorely missing a few features;
as a result, using libraries in Common Lisp was harder than in other languages,
and this making the barrier to adoption of Common Lisp accordingly higher.

For instance, although @(ASDF) made it much easier than before
for developers to install and use other developers' libraries,
it provided no good story for deploying software to end-users
who wouldn't know how to edit a @tt{~/.sbclrc}.
Because of the lack of Common Lisp standardization,
specifying component pathnames
for anything more complex than a flat directory of files
was surprisingly hard to do in a completely portable way,
and would notably break in horrible ways on Windows.
Portability of @(ASDF) itself was an issue,
and many different versions of @(ASDF) were circulated;
each implementation that provided a precompiled copy to its users
was using its own subtly different, somewhat incompatible variant.
While the same code could often be run on multiple Lisp implementations,
they might each fill directories with plenty of their own compiled files,
or worse, step on each other's compiled files.
Even cosmetically, @(ASDF) had almost but not quite the hooks
that would allow one to display nice progress report during a large build.
And then there were bugs in how dependencies
were failing to be propagated correctly across module or system boundaries.

These were all issues that I had to deal with at ITA,
where we had accumulated many bits of code to workaround these limitations,
or even to override builtin @(ASDF) functions with fixed ones.
An extreme piece of such modification to @(ASDF) was
our use of Andreas Fuchs' @(POIU).
@(POIU) was compiling code in parallel,
using as many background processes as you'd allow it,
and loading the files in serial
into the current Lisp image as they were available.
Since our build time was an issue, we tried @(POIU) to compile faster.
It too required poking inside @(ASDF), even redefining a class or two
to add a new slot or a hook function;
but it did so without changing the source code of @(ASDF) itself,
which we did not control or want to control at the time,
and indeed varied from one implementation to the next;
it was simply taking advantage of Lisp's existing language mechanisms
and its hot-patching abilities,
without any need for cheating the system
for going underneath the language
or for resorting to hidden internals of the implementation.
(Whatever implementation-specific bits are included in @(POIU)
are because there is no standard Common Lisp interface to Unix system calls.)
This would have been impossible with any static programming language,
yet relatively easy in Lisp.

I eventually started an effort to clean up this part of our code base,
and sent some patches to the maintainer of @(ASDF), who was then Gary King.
Gary seemed open to making such portability-oriented changes,
so we wouldn't have to maintain as much of the mess.
However, things were not going as fast as I wished,
because of fear of backwards incompatibility with many installations
in the wild using a variety of different revisions of @(ASDF);
how could we fix things and not break them?
Eventually, I wrote
@hyperlink["http://fare.livejournal.com/149264.html"]{one too many rant}
on what was wrong about @(ASDF) and
why it was unlikely to be fixed, unless old installations could be upgraded.
That's when Gary King burned out and resigned, and I became maintainer.

@section{ASDF 2}

My initial focus with @(ASDF2) was to make it
more robust, more portable, more extensible, and more usable:
I had to fix the more obvious bugs (obvious to users by their inconvenience);
I had to make sure the very same unmodified version could run everywhere;
I had to add all the missing slots and hooks so that
all previous code overrides would become unnecessary;
and I had to minimize the amount of configuration required,
with the possibility to wholly shield end-users
from having to configure anything at all,
yet allowing the power-users to keep their old habits if they wanted.

I imported into @(ASDF) itself
all the things previously implemented as extensions
whenever leaving them as extensions would otherwise require
special configuration by the end-user.
I notably added some configuration languages into @(ASDF) to locate systems,
and to help specify an implementation-specific cache
in which to move compiled files out of the way.
A portability layer had to be built to use pathnames in the same way
on all implementations.

Challenge: self-upgradability.

ILC 2010 article on @(ASDF2) by Robert Goldman and myself:
@hyperlink["http://common-lisp.net/project/asdf/ilc2010draft.pdf"]
{Evolving ASDF: More Cooperation, Less Coordination}.



@section{TRAVERSE}

As the dust slowly started to settle down or urgent usability issues,
it appeared that several deeper bugs required attention.

Back when we were writing the @(ASDF2) paper in 2010,
Robert Goldman basically forced me
to understand @(ASDF) enough to explain its behavior.
At the same time, he was declaring himself not satisfied with
how the semantics of @(ASDF) was not completely realized in its object model,
but partly hidden in its @cl{traverse} algorithm.
At the time, I didn't fully understand what he meant,
since the @cl{component-depends-on} function seemed to me
to pretty much describe the dependency relationships
between the nodes of the dependency graph.
(Yet already, this function is a misnomer, since the nodes are not components,
but pairs of an operation and a component,
which I have dubbed @emph{actions},
after the way they are called in
@hyperlink["BUILD"]{some old article on Lisp build systems}).

It turns out, he was right and I was wrong. So I owe him a beer or two.
For while I was understanding how @(ASDF) did work at the time,
I didn't understand how @(ASDF) @emph{should} have worked and was failing.
As a result of my recent work on @(ASDF), I finally grokked the issue,
the result of which is a much simplified @(ASDF),
one that is finally turning into something that makes sense,
rather than just a hack.
Which means that rather than just working in the common case,
it can work in all cases, and is easier to extend.

But it also turns out that finally understanding the issue ...

My recent work on fixing some deep bugs from back in the days, ...

The solution means that we can tremendously simplify @(ASDF), ...

As for @(ASDF), I could finally distill the essence of
this piece of software the latest incarnation of which
I had inherited the maintainership.
With this newfound grokking,
I could perfect its implementation in important ways,
thus building on the discoveries of Dan Barlow and previous hackers.


@section{ASDF 3}

The story starts back in April 2012, when Erik Pearson requested a feature
to speed up loading of new code in an image with lots of dependencies,
by not trying to check the up-to-date status of thousands of file
before to start compiling anything new.

It otherwise provided the user with a semantics and surface syntax
similar to those of mk-defsystem,
where a system could be defined as set of components organized in a hierarchy of modules,
with declared dependencies between components at the same hierarchical level.

Excluding some dependencies
In @(ASDF) 2.26.9, after I ...

In @(ASDF) 2.26.14, I introduced a new operation
to force load-op to explicitly depend on loading the parent's dependencies
through an explicit node in the dependency graph
rather than on implicit reliance upon the traversal algorithm.
The need for it was originally made obvious to me while rewriting @(POIU),
that builds a complete

misnamed parent-load-op, because it was being propagated from child to parent
and was loading the parent's dependencies.
What it actually is is an operation that doesn't apply just to parents
but to each and every load or compile operation, and means:
"prepare yourself to load or compile the component by first loading all its dependencies."

