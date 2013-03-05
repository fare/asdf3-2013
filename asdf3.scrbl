#lang scribble/sigplan @nocopyright
@(define (cl x) (tt x))
@(define (latin x) (emph x))
@(define (de_facto) @latin{de facto})
@(define (ASDF) @cl{ASDF})
@(define (ASDF2) (list @cl{ASDF} " 2"))
@(define (ASDF3) (list @cl{ASDF} " 3"))
@(define (Make) @tt{Make})
@(define (DEFSYSTEM) @cl{DEFSYSTEM})
@(define (defsystem) @cl{defsystem})
@(define (mk-defsystem) @cl{mk-defsystem})
@(define (XXX) ())

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


@Section{Live Programming vs Dead Programs}

Since most hackers these days are somewhat familiar with Unix and its tradition,
but mostly ignorant of Lisp and its tradition,
I will start with a small digression
on the general differences of spirit between Lisp and Unix,
which account for the difference in resulting software:
@emph{live programming} vs
@hyperlink["http://wiki.squeak.org/squeak/2950"]{Cult-of-Dead} programming,
as Lisp and Smalltalk programmers often describe the difference.

In mainstream operating systems,
whether Unix, Windows, MacOS, or Android or iOS,
programs are dead things:
The code that runs is not what programmers work on;
it cannot be fixed, improved, extended or configured.
Each run happens in a fragile short-lived session
that will lose all state after it dies;
the user must constantly restart from scratch.
The abundance of programs and libraries written in low-level languages
imply that unstability, unsafety and crashes are common phenomena.
Applications are meant to run not only without programmer interaction,
but without @emph{possibility} of programmer interaction;
any interaction that happens is to be only with end-users,
inferior beings considered to be stupid at best, enemies at worst.
Even then, no user modification, configuration, or interaction will be preserved
unless the application developer painfully made
an explicit provision to that specific effect;
changes must be persisted by writing files on the filesystem
and reading them back in each process at startup.
It is extremely painful to share state between activities,
as data must be marshalled and unmarshalled and its communication must follow
special purpose protocols with a matching pair of implementations
for both side of each communication.

By contrast, in Lisp and Smalltalk operating systems,
programs are live activities:
the code that runs is what the programmer is interacting with;
it is meant to be fixed, improved, extended and configured as it runs.
The current programming session is capable of surviving forever,
carrying its state as long as the programmer cares for it;
should some downtime be scheduled,
the machine can save its complete state to an image file,
from which it can restart as in the state it was left.
Because programs and libraries are written in a high-level language,
stability is a given, errors are usually recoverable,
and complete crashes seldom happen.
Applications are designed to be run with programmer interaction;
it is possible to explicitly restrict interaction
for delivery in hostile environment, but that's not the default.
Users are friends, and need to be educated at worst;
interactive tools are available for that,
the interface is learnable;
it is discoverable for its elements include self-description.
Every modification, configuration or interaction is preserved by default,
without any programmer having to do anything special;
where transactionality matters,
activities can transparently access records of a persistent object database.
Most activities can directly exchange and access objects in a common image;
where such activities they really need to run on separate machines
or to be isolated in separate images,
general-purpose communication protocols are available,
developers can compose co-developed implementations
of both encoding and decoding of each layer of their protocol,
written in the same high-level language, testable in the same image,
often extracted together from the same specification.

Of course, these are all a matter
of a starting points and an attitude,
rather than limitations on the process or the final product.
We know that in a possibly roundabout way,
all these systems are ultimately equivalent and can express each other.
Unix operating systems have implementations of Lisp and Smalltalk
in which you can run a live environment,
whereas Lisp machines used to run activities originally written in C for Unix
(notably the X server)
isolated from each other by their own thread-local special-variable bindings.
Yet, starting points and attitude matter.

For instance, in traditional operating systems,
it is possible to offer end-users the capability to
extend applications with their own programs,
but this comes at the cost of programmers having
to reimplement their own extension language
on top of the inextensible base language;
they may start from scratch
Greenspunning

Lisp by and large still sucks at whole-process virtualization;
but it's getting there, with such things as Places in Racket.


the only long-lived artefacts are the files on the filesystem.
The intent is for most programs
to be simple short-lived computations,
with some input and some output.
There may be a few limited-purpose "daemons" running in the background
to run various system services, but they too are prepared to die at any moment,
and do not carry any state beyond what is persisted in files.



Unix, having started as a virtualization
of the bad old days of batch processingn
on huge expensive yet slow systems with little memory,
had an initial emphasis on small special-purpose utilities
that are combined together in various ways,
at least for @emph{system administrators},
those users advanced enough to see the system.
But once you go a layer above that to programs interacting with end-users,
or use other mainstream systems
such as the more consumer-oriented Windows or MacOS
(which these days has adopted Unix underneath),
you find that the emphasis on either small or composable have disappeared.
Instead, you have huge behemoths of fancy graphical applications,
each its own mostly isolated process,
that cannot interact well if at all with other applications.

Lisp and Smalltalk programmers often deride this computing paradigm as
the
a paradigm where programs are dead things,
to which they contrast with pride their


http://martinfowler.com/bliki/InternalReprogrammability.html








@section{Programming in the Small}

Indeed, most changes to simple macros
that affect how code is generated also require
change in clients depending on the modified code;
and this only more so if the programmers follow discipline such as the
@hyperlink["XXX"]{@cl{CALL-WITH-} style} whereby
syntactic transformations at the meta-level are segregated from
semantic specification at the base-level —
only the former requires recompilation
when some incompatible syntax transformation is introduced,
at which point old clients will often (but not always) fail in obvious ways,
prompting the programmer to regenerate from clean.

It's not like @tt{make clean ; make} is unknown in the world of C.
Whenever some configuration change causes incompatible changes
in the code generation strategy by whatever tools in the build,
a @tt{make clean} is necessary.
It's just that macros are a natural part of Lisp,
themselves written using the full power of Lisp,
whereas code generation in C is customized with
a hodge-podge combination of ad-hoc languages:
C compiler flags, C preprocessor,
various domain-specific languages such as @tt{lex} and @tt{yacc},
sometimes ad-hoc preprocessing with the likes of @tt{sed} or @tt{m4}, etc.


the recompilation of the client code will thus happen
because of its own modification,
without having to be triggered by the change in the dependency.
And most people write small enough programs
(with respect to the speed of their compilers)
that if things behave weirdly, recompiling everything from scratch is an option,
which resets any bogus compilation state,
and this counter measure becomes as natural as "saving a file"
for fear of losing patiently written state to a crash.
Thus, for most Lispers in most cases, the bug might never manifested
except in transient occurrences that naturally disappear
as part of their normal programming workflow.


use of CALL-WITH- style reduces the friction.


Kalman Reti informed me that the Symbolics @cl{SCT} (System Construction Tool),
a successor to said original, had this bug fixed decades ago,
and indeed it provides a @cl{:definitions} dependency that does the right thing,
as opposed to the backward-compatible @cl{:serial} dependency that doesn't.
The native defsystem of Franz's Allegro also has that very same fix.
The native LispWorks defsystem will let you write a @cl{:rule} that
expresses this dependency and correctly propagate timestamps
of modified source files (though, and that's a bug, not of modified fasls).

One could argue that the practical disappearance of a feature once available
on the most popular Lisp platform is another case
of Lisp Lore lost to the "AI winter",
and an illustration of how proprietary software is all too mortal
and you shouldn't rely upon as a long-term repository of knowledge.
On the other hand, surviving proprietary Lisps
have each perpetuated a fix to the bug.
But these fixes, too, will eventually die with them,
and the Common Lisp community as a whole has in practice and for most of it
lost this and all the many other software features
that were tied to this and other dead platforms.

This is also an illustration that Lispers working "in the large",
combining tens of systems developed by tens of different people,
have been noticing and fixing the issue,
whereas those working "in the small" on a single system haven't:
when you have a lot of systems, the odds that one will have
a non-trivial change in meta-level state
that impacts code generation of its clients,
thus triggering the bug, increases significantly,
while at the same time, your willingness
to always recompile everything from scratch decreases.
Therefore, the fixing of this bug is also a symptom of how
the free software Lisp community has belatedly graduated
from programming in the small to programming in the large,
with quicklisp and cl-build making it easy at long last
to write and share libraries.
Or in a proximate way, a symptom of how a large company
has been hiring a guy who thinks in terms of systems
to build code using a Free Software Common Lisp infrastructure
— namely, me.

That said, the ability to survive so long while being "in the small"
can be seen as a credit to the language as much as
actually doing it can be accounted negatively to the Lisp community.
And this ability is directly related to how Lisp, as opposed to Unix,
emphasizes live programming systems as opposed to dead programs.
Our next chapter will thus be a reminder of the principles of live programming.
I will tell you the story of ASDF,
and how I came to several interesting insights while fixing this bug,
not just about ASDF itself, but also about Lisp, Lisp programmers,
and software in general.

You may freely skip the historical and/or technical sections
that do not interest you, and jump straight to the conclusion.
Or you may stay for a ride along the history and guts of ASDF.


MENTION the recompile and retry restart

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
@itemize[
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

When
when building a program may involve creating intermediate programs
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
That's when he wrote ASDF, and on top of it ASDF-Install,
as his attempt and contribution to making Lisp libraries
easier to write and distribute.

@section{ASDF}

When Dan Barlow initially wrote ASDF in 2002,
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
Kent Pitman's @ref[:url "http://www.nhplace.com/kent/Papers/Large-Systems.html"]
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
all of it under the control of ASDF's @cl{defsystem}.

Dan Barlow's final release was only 919 lines of heavily commented code.
That's much, much less than any equivalent implementation of @tt{Make} in C
(or even than @cl{mk-defsystem}, for the matter, though
@cl{mk-defsystem} did provide portability to a lot of old platforms
that ASDF didn't care to support).
Being so much shorter than Make is possible because
ASDF doesn't have to reinvent its own limited-purpose programming language
on top of an existing low-level programming language;
it can just bathe in the Lisp environment.
Yet, within those few puny lines of code,
although his primary goal had always been to write it
for his own use on top of SBCL,
he nevertheless supported seven different Lisp implementations.
ASDF may have had bugs and several features begging to be redone better,
for the main part it worked,
and it satisfied hundreds of Lisp hackers around the world
who quickly adopted it, and it soon deservedly became
the @(de_facto) standard for building and exchanging Lisp libraries.

@section{ASDF Issues}

By 2004, Dan Barlow had ceased being involved as much
in programming in Common Lisp, and eventually stopped completely.
Many hackers stepped in to help support this practical piece of infrastructure.
Christophe Rhodes notably took charge of ASDF, then after him Gary King;
but both eventually burned out, and neither
was trying to make any deep change to ASDF or to address its design issues.

Meanwhile, victim of its own success, ASDF had attracted more users,
who found that while ASDF fulfilled their basic needs,
it also had bugs and quirks that were not being addressed,
while sorely missing a few features;
as a result, using libraries in Common Lisp was harder than in other languages,
and this making the barrier to adoption of Common Lisp accordingly higher.

For instance, although ASDF made it much easier than before
for developers to install and use other developers' libraries,
it provided no good story for deploying software to end-users
who wouldn't know how to edit a @code{~/.sbclrc}.
Because of the lack of Common Lisp standardization,
specifying component pathnames
for anything more complex than a flat directory of files
was surprisingly hard to do in a completely portable way,
and would notably break in horrible ways on Windows.
Portability of ASDF itself was an issue,
and many different versions of ASDF were circulated;
each implementation that provided a precompiled copy to its users
was using its own subtly different, somewhat incompatible variant.
While the same code could often be run on multiple Lisp implementations,
they might each fill directories with plenty of their own compiled files,
or worse, step on each other's compiled files.
Even cosmetically, ASDF had almost but not quite the hooks
that would allow one to display nice progress report during a large build.
And then there were bugs in how dependencies
were failing to be propagated correctly across module or system boundaries.

These were all issues that I had to deal with at ITA,
where we had accumulated many bits of code to workaround these limitations,
or even to override builtin ASDF functions with fixed ones.
An extreme piece of such modification to ASDF was
our use of Andreas Fuchs' POIU.
POIU was compiling code in parallel,
using as many background processes as you'd allow it,
and loading the files in serial
into the current Lisp image as they were available.
Since our build time was an issue, we tried POIU to compile faster.
It too required poking inside ASDF, even redefining a class or two
to add a new slot or a hook function;
but it did so without changing the source code of ASDF itself,
which we did not control or want to control at the time,
and indeed varied from one implementation to the next;
it was simply taking advantage of Lisp's existing language mechanisms
and its hot-patching abilities,
without any need for cheating the system
for going underneath the language
or for resorting to hidden internals of the implementation.
(Whatever implementation-specific bits are included in POIU
are because there is no standard Common Lisp interface to Unix system calls.)
This would have been impossible with any static programming language,
yet relatively easy in Lisp.

I eventually started an effort to clean up this part of our code base,
and sent some patches to the maintainer of ASDF, who was then Gary King.
Gary seemed open to making such portability-oriented changes,
so we wouldn't have to maintain as much of the mess.
However, things were not going as fast as I wished,
because of fear of backwards incompatibility with many installations
in the wild using a variety of different revisions of ASDF;
how could we fix things and not break them?
Eventually, I wrote
@hyperlink["http://fare.livejournal.org/XXX.html"]{one too many rant}
on what was wrong about ASDF and
why it was unlikely to be fixed, unless old installations could be upgraded.
That's when Gary King burned out and resigned, and I became maintainer.

@section{ASDF 2}

My initial focus with ASDF 2 was to make it
more robust, more portable, more extensible, and more usable:
I had to fix the more obvious bugs (obvious to users by their inconvenience);
I had to make sure the very same unmodified version could run everywhere;
I had to add all the missing slots and hooks so that
all previous code overrides would become unnecessary;
and I had to minimize the amount of configuration required,
with the possibility to wholly shield end-users
from having to configure anything at all,
yet allowing the power-users to keep their old habits if they wanted.

I imported into ASDF itself
all the things previously implemented as extensions
whenever leaving them as extensions would otherwise require
special configuration by the end-user.
I notably added some configuration languages into ASDF to locate systems,
and to help specify an implementation-specific cache
in which to move compiled files out of the way.
A portability layer had to be built to use pathnames in the same way
on all implementations.

Challenge: self-upgradability.

2010 Report on by Robert Goldman and myself:
ASDF2 :More Cooperation, Less Coordination.


@section{TRAVERSE}

As the dust slowly started to settle down or urgent usability issues,
it appeared that several deeper bugs required attention.

Back when we were writing the ASDF 2 paper in 2010,
Robert Goldman basically forced me
to understand ASDF enough to explain its behavior.
At the same time, he was declaring himself not satisfied with
how the semantics of ASDF was not completely realized in its object model,
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
For while I was understanding how ASDF did work at the time,
I didn't understand how ASDF @emph{should} have worked and was failing.
As a result of my recent work on ASDF, I finally grokked the issue,
the result of which is a much simplified ASDF,
one that is finally turning into something that makes sense,
rather than just a hack.
Which means that rather than just working in the common case,
it can work in all cases, and is easier to extend.

But it also turns out that finally understanding the issue
the fix is turning

My recent work on fixing some deep bugs from back in the days,

the solution means that we can tremendously simplify ASDF,

As for ASDF, I could finally distill the essence of
this piece of software the latest incarnation of which
I had inherited the maintainership.
With this newfound grokking,
I could perfect its implementation in important ways,
thus building on the discoveries of Dan Barlow and previous hackers.


@section{ASDF 3}



@section{Not Quite the Right Thing}

Such a long-lasting bug as I fixed belies the claim
that the Lisp guys were doing the Right Thing
whereas the C guys were epitomizing the Worse is better attitude.

Unix had already clearly won,
and what he was hoping for was to make Lisp play nice with Unix.
Maybe the Lisp @cl{DEFSYSTEM}s once pioneered great innovative ideas,
they were not working very well in practice,
and he wanted to experiment with his own take on it.

The downside to a modern programmer is that unlike Unix then and now,
Lisp did not have virtualization at the time
(and is still lacking a good story on that),
and so all the computations happen in the current image.

Interactivity is reserved to special-purpose applications,
that are not meant to be programmable, just to do one simple thing.
It is nearly impossible to combine data from one application to the other.
there may be less emphasis on small simple programs,
and more on huge
but otherwise all the above design principles hold.

running live systems,
that may stay on or otherwise persist state for years, potentially forever,
with their code evolving, often in radical ways, as time goes by,
yet preserving any interactive state that the user cares about.
All activities are part of the same interaction, and
users can write programs that seamlessly combine data from all these activities,
using the same programming language sharing all these objects
within the same heap.
things

In live programming, the user starts a @emph{world} from an @emph{image},
keeps using it, then @emph{saves the world} or @emph{dumps the image}
when it has reached an interesting state.
Certainly, interaction state including source code
may be persisted in files or in a database,
for persistence, communication, garbage collection
(rebuilding a clean image without any pollution by unwanted state), etc.
But ideally, the system should be stable enough to stay on forever,

Actually, it's more a diff of POV and granularity.
C is not Turing Equiv.

Of course, in Unix as in other systems,
all really successful applications eventually
grow their own badly designed user progrmaming language.
, out of necessity,
never intended as such, even less well-designed or curated,
but rather grown from badly needed feature
to other badly implemented feature;

The only general purpose interactive tool is the shell,
which is a programming language by necessity,
it is slow as hell, its "scripts" (programs that are ashamed of themselves)
are anything but robust and cannot scale yet the way they
allow for composition of independently developed programs
is the one (and only) thing that Unix creators may claim with pride
with regard to system design.

but that is considered alright because anything important
is to be done in "real" programming languages.


Yet, the one positive contribution of Unix to system design
may be that this interactive shell unintentionally created a culture
of composing programs from other programs through
As a vast improvement on the bad old days,
Unix allows user programs to communicate with each other in useful ways,
and develops a style of
and gives birth to composability of programs.

may be interactive, but that's for end-users,
who don't use real programming languages, though they but various undesigned
Real
and
Interactive programs are all special-purposed


While over the years and after an impressive amount of engineering,
Unix systems have been polished into robust working pieces of software,

------>8------>8------>8------>8------>8------>8------>8------>8------>8------

Programmers vary a lot in personality.
Their approach to software may contain more or less of the
Scientist who sees it as pre-existing mysteries to be unraveled, understood and admired;
the Engineer who sees it as man-made structures to be built and maintained;
the Artist who sees it as an expression or man's tastes and values;
or the Manager who sees it as a project of people to coordinate.
Common Lisp programmers tend to distinguish themselves in
more than other programmers
seeing @emph{existing} software as uncreated discoveries
that must be studied by exploration to discover more,
rather than fully designed artefacts
that must be either learned from authority,
and accepted or rejected to create more.
Software, like Mathematics, is not of Man, but of God —
that is, it is not designed, it @hyperlink["evolutionism"]{evolves}.

------>8------>8------>8------>8------>8------>8------>8------>8------>8------


It worked because if develop an application the Lisp way,
loading and updating your one target application's @cl{system}
in your current image, the failure to propagate timestamps won't matter.
It's only if you're developing many different applications and libraries at the same time,
from several (or restarted) Lisp images that you will sometime hit the bug.


I could also pinpoint many ways in which Lisp hackers
behave differently from other programmers,
with interesting implications on what they can either
teach other programmers or learn from them.

you program in a live system, the error doesn't appear

that make it properly process timestamps.

such that it is finally becoming what it ought to have been,

between @emph{actions},
the (operation,component) pairs that

(misnamed)



@title{Some ASDF History}

@section{Background}

It otherwise provided the user with a semantics and surface syntax
similar to those of mk-defsystem,
where a system could be defined as set of components organized in a hierarchy of modules,
with declared dependencies between components at the same hierarchical level.

The story starts back in last April, when Erik Pearson requested a feature
to speed up loading of new code in an image with lots of dependencies,
by not trying to check the up-to-date status of thousands of file before to start compiling anything new.

excluding some dependencies
In asdf 2.26.9, after I

In ASDF 2.26.14, I introduced a new operation to force load-op to explicitly depend on loading the parent's dependencies through an explicit node in the dependency graph rather than on implicit reliance upon the traversal algorithm. The need for it was originally made obvious to me while rewriting POIU, that builds a complete

 misnamed parent-load-op, because it was being propagated from child to parent and was loading the parent's dependencies. What it actually is is an operation that doesn't apply just to parents but to each and every load or compile operation, and means: " prepare yourself to load or compile the component by first loading all its dependencies."




DOMAINES WEB

/ zCD|E*c//bCD*bgBC|D*G//xfe*c//xD|E*c//bCD*bgBC|D*G//xfe*//xG|
A*e//caF*c//af|e*G+E*dc*a//EG|A*e//caF*c//af|eEdzcba*//
