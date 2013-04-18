#lang scribble/sigplan @nocopyright

@(require scribble/base
          "utils.rkt")

@title{Live Programming vs Dead Programs}

Since most hackers these days are somewhat familiar with the Unix tradition,
but mostly ignorant of Lisp Lore,
I will start with a small digression
on the general difference in spirit between Lisp and Unix,
from which springs the difference in resulting software:
@emph{live programming} vs
@hyperlink["http://wiki.squeak.org/squeak/2950"]{Cult-of-Dead} programming,
as Lisp and Smalltalk programmers often describe the difference.

You may skip this section if you are already familiar with the concept.


@section{Live Programming}

A live programming system is designed with
the user as an active part of the software development feedback loop.
Programs are live activities, undergoing change
as the user not only @emph{can} modify the code of these programs as they run,
but @emph{is supposed to} do so, as
this is the normal way, indeed the one and only way,
of interacting with the system:
shaping it to serve the user's purposes,
themselves refined from the interaction.
@emph{The difference between a programmer and a user, is that the programmer
knows there is no difference between using and programming.}
Fixing, improving, extending or configuring the system
are all but incremental forms of software development,
indistinguishable and unsegregated from other programming activities.

For an effective user experience,
the programming system is made interactively available
in a dynamic homoiconic high-level language,
in which code and data persist in the open.
@itemlist[
  @item{
    High-level means that inasmuch as technology allows,
    the user can manipulate and refine the concepts that matter to him
    without having to delve into irrelevant details
    at a lower level of details than he cares for,
    such as allocating and staying within address ranges,
    marshalling and unmarshalling data,
    compilation representation strategies,
    filling and flushing a hierarchy of caches,
    ensuring multiple changes are done coherently, etc.
  }
  @item{
    Persistence means that the data he manipulates remains valid
    from session to session and system modification to system modification,
    without having to care about saving and restoring, exporting and importing,
    transactional failures and accidental corruptions,
    scheduled and unscheduled down times,
    dead batteries and hard disk failures,
    incompetent or bankrupt service providers,
    computer theft and gestapo crackdowns, etc.,
    or simply crashing the system and losing data the slightest of mistakes.
  }
  @item{
    Dynamicity means that programming doesn't require from the user
    to either know in advance the structure of the data for aeons to come
    or have to explicitly write and use converters for entire systems
    every time that any structure changes the slightest bit,
    under pains of either being unable to use the data,
    or having the system crash, or worse, having subtly wrong answers,
    or worst of all, experiencing data corruption.
  }
  @item{
    Homoiconicity means that code itself is represented as data
    that can be easily expressed and manipulated by the programming language,
    such that the user can use the programming power of the system
    to extend and further shape the system,
    as opposed to having to either do things painfully by hand
    or having to reimplement large swaths of the system from scratch.
  }
]
These concepts can be viewed as aspect of a same principle:
best adapt to the evolving purposes of the user-programmer,
while shielding him from issues he isn't interested in addressing.

Software is designed to be run with programmer interaction;
it is possible to explicitly restrict interaction
for delivery in hostile environment,
or to otherwise extract an "application" from the development system,
but that's not the default.
Users are friends, who need to be educated and guided at worst;
the interface is learnable,
it is discoverable for its elements include self-description;
the scope of the interaction may be narrowed or widened
to fit the user's current focus.

In other words, live programming systems is designed with the user at the center,
as a way to extend the reach of the user,
and from which new functionality grows bottom up.


@section{Dead Programs}

A cult of dead software system, by contrast,
is centered around the delivery of automated applications
that run with limited user interaction if any.
Applications are meant to run not only without programmer interaction,
but without @emph{possibility} of programmer interaction;
any interaction that happens is to be only with end-users,
inferior beings considered to be stupid at best, enemies at worst.
Though such a system by necessity needs possess ways to develop software,
it maintains a strict separation between development and use of software.
By the time they are actually used, programs are dead things,
the code of which is cast in stone, sealed in a coffin,
and impossible to see much less modify, fix or extend.

Users modify data, which is dead information
that only has some semblance of life inasmuch as
it is animated by the dead programs
according to a finite set of behaviors
decided in advance by application providers.
Administrators manage configuration, which is very a sea of settings,
each very narrow, to glue the system together
and parameterize just how restricted each user is
who was granted, top-down, an access privilege to the system.

Mainstream operating systems such as Unix, Windows, MacOS, Android or iOS
follow this cult of dead approach in part because they were designed
to sell proprietary "devices" and "applications".
By making the code hard to see and modify,
it is easier to defend "IP" monopolies,
and make it hard technically and legally
for those who would like to copy, modify and redistribute code.
By artificially segregating users from programmers,
it becomes easier to keep the masses ignorant and passive
and extract resources from them.

The system is organized in layers atop the hardware,
with each layer tuned towards
making it easy for the below layers to implement it efficiently.
Software is thus written in low-level languages each centered
around the implementation details of each below layer,
rather than around abstract and composable human concerns.
Therefore unstability, unsafety and crashes are common phenomena.

Each time an application is run, it runs in
a fragile short-lived session that will lose all state after it dies;
programs must constantly restart from scratch,
and the user must adapt to this perpetual loss of data.
No user modification, configuration, or interaction will be preserved
unless the application developer painfully made
an explicit provision to that specific effect;
changes must be persisted by writing files on the filesystem
and reading them back in each process at startup.

Since the goal is a dead program that doesn't evolve,
programming languages are static:
they offer no possibility for change in a running program,
but instead take advantage of static analysis of immutable code
to cram a bit more performance,
often (though not always) at the expense of safety,
always at the expense of evolvability.

Finally, strict phase separation segregates
development-time computations
from build-time computations from runtime computations.
This doesn't just make development of a given program harder,
as developers find they must encode in advance things they
[vs RTTI]

It is extremely painful to share state between activities,
as data must be marshalled and unmarshalled and its communication must follow
special purpose protocols with a matching pair of implementations
for both side of each communication.

In other words, such systems are designed around the machine, not the human.

@bold{XXX XXX TO BE CONTINUED XXX XXX}

@section{Opposite View}

@section{Meet me in the middle}

Live programming... with how little you need initially,
that you can grow from.

Dead programs... with how much preset features are provided in advance,
that can only be narrowed from, never added to.


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

Recreating features by piling on layers of complexity.

