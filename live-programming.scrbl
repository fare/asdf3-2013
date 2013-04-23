#lang scribble/sigplan @nocopyright

@(require scribble/base "utils.rkt")

@title{Live Programming vs Dead Programs}

Most hackers these days are somewhat familiar with the Unix tradition,
but mostly ignorant of Lisp Lore.
p@(if (multiple-sections)
@list{This section of the document, as a short digression,}
@list{This short essay})
tries to describe the general difference in spirit
between Lisp and Unix,
from which springs the difference in resulting software:
@emph{live computing} systems vs
@hyperlink["http://wiki.squeak.org/squeak/2950"]{Cult-of-Dead} computer systems,
as Lisp and Smalltalk programmers often describe the difference.

I will be deliberately exaggerating the picture and painting with a big brush,
to make the big picture more obvious.

@(if (multiple-sections)
@list{You may skip this section if you are already familiar with the concept.}
@list{This essay was originally started as part of a tentative to explain
@(ASDF) by contrasting it with @(Make), but took a life of its own.})


@section{Live Programming}

A live computing system is designed with
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
in which code and data persist transparently.
@itemlist[
  @item{
    @emph{High-level} means that inasmuch as technology allows,
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
    @emph{Persistence} means that the data he manipulates remains valid
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
    @emph{Dynamicity} means that programming doesn't require from the user
    to either know in advance the structure of the data for aeons to come
    or have to explicitly write and use converters for entire systems
    every time that any structure changes the slightest bit,
    under pains of either being unable to use the data,
    or having the system crash, or worse, having subtly wrong answers,
    or worst of all, experiencing data corruption.
  }
  @item{
    @emph{Homoiconicity} means that code itself is represented as data
    that can be easily expressed and manipulated by the programming language,
    such that the user can use the programming power of the system
    to extend and further shape the system,
    as opposed to having to either do things painfully by hand
    or having to reimplement large swaths of the system from scratch.
  }
  @item{
    @emph{Transparence} means the entire system is designed
    to be run in interaction with the user and is
    @hyperlink["http://martinfowler.com/bliki/InternalReprogrammability.html"]
        {Internally Reprogrammable}.
    though the user doesn't have to deal
    with any particular details he doesn't care for,
    whatever aspect of the system the user does decide to care for
    is readily accessible for him to inspect or modify.
    Nothing is hidden, everything is done in the open;
    the user can adjust the scope of the interaction,
    by narrowing or widening it to fit his current focus.
    The user is a friend, who needs to be educated and guided at worst;
    to that end, the interface is learnable,
    it is discoverable for its elements include self-description
    and documentation including any source code.
  }
]
These concepts can be viewed as aspects of a same principle:
to best adapt to the evolving purposes of the user-programmer,
while shielding him from issues he isn't interested in addressing.
In other words, a live computing system is designed
with the user at the center,
as a way to extend the reach of the user,
and from which new functionality grows bottom up.


@section{Dead Programs}

A cult-of-dead computer system is organized all along
according to opposite principles.
The machine, not the human,
is the central concern from which radiates the architecture of the system.
Indeed, end-users are considered as inferior beings,
stupid at best, enemies at worst,
not to be empowered, but exploited and defended against.
Users consume or provide data, which is dead information
that only has some semblance of life inasmuch as
it is animated by the dead programs
according to a finite set of behaviors
decided in advance by application providers.
Administrators manage configuration, which is very a sea of settings,
each very narrow, to glue the system together
and parameterize just how restricted each user;
an access privilege to the system is granted top-down according to rigid rules.
A forest of bureaucratic forms are the normal way of interacting with the system,
except that when consumers have to be seduced out of their money,
some dumbing down and gamification is added on top to make it all more attractive.

To preserve it against its user, the cult-of-dead computer system
is programmed in a dualist paradigm using static low-level language,
where code and data are both ephemeral and opaque.

@itemlist[
  @item{
    @emph{Opacity} means that the user is not authorized to see
    what happens in the system;
    any details about the system are the domain
    of a separate caste of humans, the programmers;
    even programmers are controlled by system administrators
    who configure and manage the system,
    ultimately enforcing the will of corporate and bureaucratic policy makers.
    Thanks to this opacity, computer systems can be neatly packaged
    into "devices", "applications" and "content" that can be sold
    to ignorant masses for passive consumption.
    Technical opacity and legal protection by "IP" monopolies
    make it hard to copy, modify and redistribute code,
    and thus keep the masses in check to extract resources from them
    while excluding small innovators and new competitors.
  }
  @item{
    @emph{Dualism} means there is a strict segregation between "code" and "data".
    "code" is whatever programmers manipulate, to be delivered into applications;
    "data" is whatever applications manipulate, with or without user interaction;
    never the twain shall meet.
    In particular, never shall the user interact with the code;
    (even their own data they can only access
    but through the mediation of the application
    (assuming their license is up to date)).
    Programming happens using special tools unavailable to normal users.
    By the time they are actually used, programs are dead things,
    the code of which is cast in stone, sealed in a coffin,
    and impossible to see much less modify, fix or extend.
    This hierarchy may happen on several levels;
    programmers themselves are to use the languages provided by their own masters,
    and any automatic program-manipulation is to done by a different caste of programmers.
  }
  @item{
    A @emph{Static} approach is typical of cult-of-dead computer systems:
    since the goal is a dead program that must not evolve,
    programming languages use static types to structure their data,
    whereby the structure is assumed to never change.
    This approach makes it easy to gain extra performance in the simple cases,
    though often at the expense of safety,
    and always comes at the expense of any possibility to evolve the running program,
    while also making it extremely hard to evolve the data.
    It is extremely painful to share state between activities,
    as data must be marshalled and unmarshalled and its communication must follow
    special purpose protocols with a matching pair of implementations
    for both side of each communication.
  }
  @item{
    @emph{Impermanence} is the norm:
    applications are each time run in a fragile short-lived session
    that will lose all state when it dies;
    programs must constantly restart from scratch,
    and the user must adapt to this perpetual loss of data.
    No user modification, configuration, or interaction will be preserved
    unless the application developer painfully made in advance
    an explicit provision to that specific effect;
    changes must be persisted by writing files on the filesystem
    and reading them back in each process at startup.
    Saving data is essentially unreliable,
    even more so when data is to be shared between applications on the same machine,
    even more so between multiple machines.
    Code also is ephemeral, and it is usually impossible
    to run the same program after a few years,
    often making old data useless even if preserved.
  }
  @item{
    @emph{Low-level} languages are the nail on the coffin
    of cult-of-dead computer systems:
    the system is organized in successive layers atop the hardware;
    each layer only provides higher-level functionality
    by exposing details and limitations
    of the static implementation in which it is rooted,
    rather than providing abstract and composable human concerns.
    The approach makes the simple case efficient
    while requiring extreme complexity to handle the general case,
    including dealing with the quirks of all the successive layers below.
    Unstability, unsafety, crashes and security issues
    are consequently pervasive phenomena.
  }
]

In other words, such systems are designed around the machine, not the human,
who is but a cog in the bureaucracy.
The underlying principle is that of authority from above,
from which flows all meaning and all value,
to be enforced against stray subordinates.


@section{IF U SO GOOD, Y U NO WIN?}

Of course, this unflattering view of mainstream computer systems
is purposefully one-sided and exaggerated.
That's the whole point:
to make it painfully obvious
what the proponents of live computing systems want,
and what they reject about mainstream computer systems.
But the point won't be complete
unless and until we find the non-evil justifications
behind the current state of affairs:
we need to acknowledge the strength and the achievements of mainstream computer systems
despite their total or partial embrace of the cult-of-dead computer paradigm,
and we need to account for the weaknesses and failures of live computing systems.
Then we may see how both approaches do or may cope with their respective limitations,
and what they can learn from the other approach.

An excuse that many proponents of Live Computing will use to explain
their relative failure on the market is that
the historic success of cult-of-dead computer systems is largely due to extrinsic factors
such as massive support from government and big corporations,
that propagate their bureaucratic structure of top-down control to the software they fund.
Now, these factors will persist as long as the cause persists,
which means the foreseeable future.
Yet, if as we allege live computing systems provide an edge in productivity,
a strong secondary market in such systems may thrive,
generating technological progress that gets incorporated into mainline cult-of-dead computer systems.
Live programmers will argue that this has already been happening,
and that mainstream cult-of-dead computer systems have thusly adopted many of
the formerly distinctive features of live computing systems,
even though they won't and indeed can't adopt it fundamental paradigm:
object orientation and integrated development environments in the 1980s,
type safety and garbage collection in the 1990s,
and since then the slow percolation of higher-order functions,
metaprogramming techniques, reflection, and dynamic features.

But that excuse is also a cop out, unless we examine the reasons
why these external factors play
in favor of cult-of-dead computer systems rather than live computing systems.
Indeed, if live programming makes for vastly increased productivity,
wouldn't it be advantageous to corporations and governments that adopt it
and reap the productivity benefits?
One immediate advantage that the dead program approach has been having so far is that
by focusing on optimizing for the easy case,
cult-of-dead computer systems have been pick the low-hanging fruits,
and generating more economic value now than more elaborate systems
that better solve the general case.
In other word, cult-of-dead computer systems are indeed directed at harvesting the resources
that matter to the society at large,
when live computing systems optimize the use of the programmer at hand;
and too bad if this relatively starves out more ambitious endeavours to reach higher.
But we can backtrack further: why can't or won't the programmer, once in charge,
make the economically efficient or socially empowering choices?
Because the skills and interests it takes to be a successful businessman or powermonger
are not those it takes to be a good programmer;
whatever comparative advantage you have in one direction,
by definition you don't have in the other;
and so the experts, once in charge, do what suits them,
and fail to efficiently address other people's first concerns,
whereas managers, once in charge, do more of what suits these immediate concerns
even at the cost of technically inferior solutions
warped to suit their ways of thinking.
At root therefore is a psychological tension co-substantial with division of labor itself.

Now, whatever their technical shortcomings and the non-technical factors
that went into making them dominant on the technical scene,
it is a fact that cult-of-dead computer systems have captured
the larger part of the programmer mindshare.
With this mindshare comes a the large value associated to positive network effects.
This alone constitutes a huge barrier to entry for any rival system or technical paradigm.
Actually, some have argued that the vastly increased productivity from live computing systems
is a @hyperlink["http://www.winestockwebdesign.com/Essays/Lisp_Curse.html"]{Curse},
in that it attracts the kind of people who can and will do everything they need by themselves,
and therefore not participate strongly in a community that builds larger software;
live computing systems thus lead to plenty of quickly risen software that is only used and usable
by one person or one small team of people, and dies off
(its very source code often being destroyed or made unavailable)
when that person dies, retires, or changes interest, or
when that group is dissolved for whatever combination of corporate or personal reasons.
Meanwhile, developers of cult-of-dead computer systems by necessity have had to invest early on
in social infrastructure to work together, because their systems are so bad
that they couldn't build anything of interest alone, anyway.
So we see again a psychological tension,
not just between programmers and managers, but among programmers between each other programmer.

Finally, another, related, technical edge
that cult-of-dead computer systems have gained and preserved
is in most topics related to auditability of the code:
dead programs, precisely because they don't change
and can have their meaning not depend on changing context,
can more easily be isolated, duplicated, tested, analyzed, quality-controlled,
sandboxed, reasoned about, proven to satisfy some safety property,
their bugs reproduced, etc.
A live computing system, by contrast, can easily become "autistic",
the meaning of the evolving body code crucially depending on changes to the environment,
or worse, tangled with implicit behavior that has been accumulated
in the "bootstrapped" image of the live computing system across its development history;
such "autistic" systems can be very hard for anyone but the historical authors to understand,
and very hard even for the historical authors to untangle.
This class of problems certainly do arise even with cult-of-dead computer systems;
but the cult-of-dead program tradition has developed a lot of tools to deal with these problems,
with which live programmers are playing catchup, when they are not in neglect or denial:
formal specifications, automated test suites, version control, static types,
proof systems, virtualization, bug tracking, etc.
The one edge that live computing systems hereby possess over cult-of-dead computer systems
in this respect is the ready availability of source code,
and the ease of writing code-manipulation tools;
but open-source solves the former issue for those who adopt it,
and standardization combined with massive resources from the large mindshare
address the latter issue.

In conclusion, even assuming the live computing paradigm provides
an intrinsic advantage in terms of both productivity and how complex problems it can help solve,
both at the scale of the immediate needs of a programmer and in the long-term for everyone,
it cannot succeed unless it can cross the chasm by competing
on the economic needs of users at large:
pick the low-hanging fruits as well as the higher-reaching ones,
care for low-level optimizations and the common use-case,
speak the language of the other programmers, the end-users and the managers,
address the issues of auditability of code.
A paradigm cannot be considered superior if it is incapable of surviving and taking off;
at best it is a paradigm whose time hasn't come yet,
that will take off later and survive in the mean time.
As the problems that people try to solve with software get more elaborate,
programmers of live computing systems will hopefully start
investing more energy in collaboration skills,
whereas programmers of cult-of-dead computer systems will hopefully
adopt more of the productive paradigm of live programming.
But there's a lot of room for progress either way so far.


@section{Meet me in the middle}

Live programming and dead programs are paradigms, points of view.
Actual software fits somewhere in between,
and many systems the design and history of which are rooted in live programming
have adopted innovations from cult-of-dead computer systems, and the other way around.
Let's reexamine the opposition between those two paradigms in this light.

@bold{XXX XXX TO BE CONTINUED XXX XXX}

@XXX{
Regarding Transparence and Opacity,
it is a fact most people, most of the time, have no desire to tune their engine;
indeed, not only are they not proficient enough to do it,
they are only likely to break things and harm themselves if they modify anything.
However, what that justifies is not keeping the engine inaccessible,
it's just having the hood closed by default, yet having an easy way to open it.
Indeed, the engine may have to be serviced eventually;
wholly disabling access makes an otherwise fixable system an expensive piece of junk,
whereas restricting access only to technicians member of a monopoly
will mean much more expensive and bad quality service than users would otherwise get;
and anything that reduces the value to consumers eventually reduces
what they are ready to pay to producers.


and least you lose your mindshare

And indeed, since the 1990s, even the most reluctant vendors of computer systems
have learned that for their system to remain relevant,
they needed to acquire and keep the mindshare of programmers,
and had to provide free development tools and cultivate a healthy community.

Even programmers only care about a narrow aspect of the system at any given time,
and may only be proficient to modify but a small subset of it.

That's a good reason to keep the engine under a closed hood that is locked by default.
A readily available

free software: source, but not at runtime

That might not be reason to make the tuning wholly forbidden;
and indeed, if no one but patented technicians could open the hood,
then not only would repairs be much more expensive than they are now for the average user,
but there wouldn't be new generations of passionate people
developing their taste for the art to make good technicians
and invent the engines of the future.


Live computing... with how little you need initially,
that you can grow from.

Dead programs... with how much preset features are provided in advance,
that can only be narrowed from, never added to.

it is possible to explicitly restrict interaction
for delivery in hostile environment,
or to otherwise extract an "application" from the development system,
but that's not the default.


Of course, these are all a matter
of a starting points and an attitude,
rather than limitations on the process or the final product.
We know that in a possibly roundabout way,
all these systems are ultimately equivalent and can express each other.
Unix operating systems have implementations of Lisp and Smalltalk
in which you can run a live computing environment,
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
of the bad old days of batch processing
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

Recreating features by piling on layers of complexity.
}