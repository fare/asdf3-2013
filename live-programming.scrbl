#lang scribble/sigplan @nocopyright
@(require scribble/base "utils.rkt")

@title{Live Computing vs Dead Software, or LAMBDA, the Ultimate Culture War}

@authorinfo["François-René Rideau" "Google" "tunes@google.com"]

@conferenceinfo["Onward!" "October 20--24, Portland, Oregon."]
@copyrightyear{2014}
@;http://splashcon.org/2014/onward.html

@abstract{
  The world of computing has been torn by war for many decades.
  A culture war, of many dimensions, of which we'll try to describe one.
  On the one side we have live programmers opposing cult-of-dead zombies.
  On the other side we have formal programmers opposing brutish bitdiddlers.
  The opposition is not a matter of various features present or absent
  from programming systems, although it does manifest itself through
  a preference for some features over others.
  The opposition is one of approach, of paradigm, of sense of life.
  We'll try to speak for both sides, one at a time,
  taking extreme stances to make the points more visible;
  yet we'll offer a way for the two sides to make peace,
  and reconcile their visions into a richer paradigm.
}

  Dynamic typing vs static typing.
  Garbage collection vs static memory management.
  Incremental code and data upgrade vs starting fresh processes everytime.
  Interactive REPL vs batch processing.
  Metaprogramming vs design patterns.
  Evolution vs Intelligent Design

  And while many of its once distinctive traits
  are now commonplace features accepted in this mainstream,
  ...

  It proudly touts dynamic typing where
  serious programming language experts
  Most hackers these days are somewhat familiar with the Unix tradition,
  but mostly ignorant of Lisp Lore.
  This essay tries to describe the general difference in spirit
  between Lisp and Unix,
  from which springs the difference not just in resulting software
  but in pervasive culture:
  @emph{live computing} systems vs @emph{Cult-of-Dead} computer systems,
  as Lisp and Smalltalk programmers often describe the difference.

  I will be deliberately exaggerating the picture and painting with a big brush,
  to make the big picture more obvious.
  The gentle reader will easily figure the shades of grey
  to those black and white outlines.
  Hopefully, she can then proceed to make her own black on white drawings,
  white on black chalkings, and paintings using a full color palette.
}
@; This essay was originally started as part of a tentative to explain @(ASDF)
@; by contrasting it with @(make), but took a life of its own.


@section{Computing vs Computers}

@emph{Computer Science is no more about computers than astronomy is about telescopes.}
— E. W. Dijkstra.


@subsection{Live Computing}

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
indistinguishable and unsegregated from other computing activities.

For an effective user experience,
the computing system is made interactively available
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
    such that the user can use the computing power of the system
    to extend and further shape the system,
    as opposed to having to either do things painfully by hand
    or having to reimplement large swaths of the system from scratch.
  }
  @item{
    @emph{Transparence} means the entire system is designed
    to be run in interaction with the user and is
    @hyperlink["http://martinfowler.com/bliki/InternalReprogrammability.html"
       ]{Internally Reprogrammable}.
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


@subsection{Dead Programs}

A @bydef{cult-of-dead} computer system@~cite[Cult-of-Dead-mail],
is organized all along according to opposite principles.
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
    even programmers are controlled by a caste of system administrators
    who configure and manage the system,
    ultimately enforcing the will of corporate and bureaucratic policy makers.
    Thanks to this opacity, computer systems can be neatly packaged
    into "devices", "applications" and "content" that can be sold
    to ignorant masses for passive consumption.
    Technical opacity as well as legal protection by "IP" monopolies
    make it hard to copy, modify and redistribute code,
    and thus keep the masses in check to extract resources from them
    while excluding small innovators and new competitors.
  }
  @item{
    @emph{Dualism} means there is a strict segregation between "code" and "data".
    "code" is whatever programmers manipulate, to be delivered into applications;
    "data" is whatever applications manipulate, with or without user interaction;
    never the twain shall meet.
    In particular, never shall the user interact with the code
    (even their own data they can only access
    but through the mediation of controled applications
    (assuming their license is up to date)).
    Programming happens using special tools unavailable to normal users.
    By the time they are actually used, programs are dead things,
    the code of which is cast in stone, sealed in a coffin,
    and impossible to see, much less modify, fix or extend.
    This hierarchy may happen on several levels;
    programmers themselves are to use the languages provided by their own masters,
    and any automatic program-manipulation is to done by a different caste of programmers.
  }
  @item{
    A @emph{Static} approach is typical of cult-of-dead computer systems:
    since the goal is a dead program that must not evolve,
    programming languages use static types to structure their data,
    whereby the structure is assumed to never change.
    This approach makes it easy to gain extra performance in the simple cases;
    however, this gain is made at the expense of safety, expressiveness, or both,
    in a way to precludes any possibility to evolve the running program,
    while also making it extremely hard to evolve the data.
    It is extremely painful to share state between activities,
    as data must be marshalled and unmarshalled and its communication must follow
    special purpose protocols with a matching pair of implementations
    for both sides of each communication.
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
    yet more so between multiple machines.
    Code also is ephemeral, and it is usually impossible
    to run the same program after a few years,
    often making old data useless even if painfully preserved.
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


@subsection{IF U SO GOOD, Y U NO WIN?}

Of course, this unflattering view of mainstream computer systems
is purposefully one-sided and exaggerated.
That's the whole point:
to make it painfully obvious
what the proponents of live computing systems want,
and what they reject about mainstream computer systems.
On the other hand, the point won't be complete
unless and until we find the good justifications
behind the current state of affairs:
we need to acknowledge the strength and the achievements of mainstream computer systems
despite their total or partial embrace of the cult-of-dead computer paradigm,
and because of this embrace.
We need to account for the weaknesses and failures of live computing systems.
Then we may see how both approaches do or may cope with their respective limitations,
and what they can learn from the other approach.

An excuse that many proponents of live computing will use to explain
their relative failure on the market is that
the historic success of cult-of-dead computer systems is largely due to extrinsic factors
such as massive support from government and big corporations,
that propagate their bureaucratic structure of top-down control to the software they fund.
Now, these factors will persist as long as the cause persists,
which means the foreseeable future.
Yet, if as we allege live computing systems provide an edge in productivity,
a strong secondary market in such systems may thrive,
generating technological progress
that gets incorporated into mainline cult-of-dead computer systems.
Live programmers will argue that this has already been happening,
and that mainstream cult-of-dead computer systems have thusly adopted many of
the formerly distinctive features of live computing systems,
even though they won't and indeed can't adopt it fundamental paradigm:
object orientation and integrated development environments in the 1980s,
type safety and garbage collection in the 1990s,
and the slow percolation of higher-order functions,
metaprogramming techniques, reflection, and dynamic features.

But that excuse is also a cop out, unless we examine the reasons
why these external factors play
in favor of cult-of-dead computer systems rather than live computing systems.
Indeed, if live computing makes for vastly increased productivity,
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

Now, dead programs also have an important property,
that makes them significantly more amenable to some kinds of analyses than live programs:
they are dead.
That is, their behavior is cast in stone in ways that it isn't by definition in a live program.
Therefore, whichever user or meta-program analyses a dead program
can reach answers that are not going to be randomly invalidated
by some future action by the user or input from the environment.
This is quite practical especially in cases
where the program is supposed to run without supervision,
whether on the computer of an "end-user" unable or unwilling to program,
or without human interaction at all.
Oh, certainly, a "super-user" might modify the software anyway;
or the hardware itself may experience failure, may be tampered with,
or be otherwise decommissioned and replaced.
So any prediction made based on analyzing the dead program
is conditional on the program being maintained running in a suitable environment.
Still, predictions under reasonable assumptions
can be much better than "anything goes at the slightest change",
and the rare actions that invalidate those assumptions
can be specially detected, logged and manually audited.

Whatever their technical shortcomings and the non-technical factors
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
in social infrastructure to work together, because their systems are so unproductive
that they couldn't build anything of interest alone, anyway.
So we see again a psychological tension,
not just between programmers and managers, but between programmers and other programmers.

Finally, another, related, technical edge
that cult-of-dead computer systems have gained and preserved
is in most topics related to auditability of the code:
dead programs, precisely because they don't change
and can have their meaning not depend on changing context,
can more easily be isolated, duplicated, tested, analyzed, quality-controlled,
sandboxed, virtualized, compiled, reasoned about,
proven to satisfy some safety property, their bugs reproduced, etc.
Discrepancy from expected standards can be detected early and automatically.
A live computing system, by contrast, can easily become "autistic",
the meaning of the evolving body code crucially relying
on unspecified assumptions about the environment
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
adopt more of the productive paradigm of live computing.
But there's a lot of room for progress either way so far.

@section{Meet me in the middle}

Live computing and dead programs are paradigms, points of view.
Actual software fits somewhere in between,
and many systems the design and history of which are rooted in live computing
have adopted innovations from cult-of-dead computer systems, and the other way around.
Or rather, software development occupies a continuum
Homoiconicity and dualism are therefore two complementary points of view on programming,
and an advanced enough programming system will allow users to seamlessly shift point of view.

Let's reexamine the opposition between those two paradigms in this light.

@subsection{Transparence and Opacity}

No system can be afford to be completely opaque.
Indeed, every engine has to have been designed to begin with,
and may have to be serviced eventually;
wholly disabling access makes an otherwise fixable system an expensive piece of junk,
whereas restricting access only to patented technicians, members of a monopoly,
will mean much more expensive and bad quality service than users would otherwise get;
and anything that reduces the value to consumers eventually reduces
what they are ready to pay to producers.
Moreover, preventing the technically proficient from poking at the insides of the system
means you lose mindshare amongst the proficient,
who won't be available to service your customers,
who'll get familiar with other systems instead,
will invent enhancements to these other systems, and
when they get management positions will adopt these other systems with which they are familiar.

Since the 1990s, even the most reluctant vendors of computer systems
have learned that for their system to remain relevant,
they needed to acquire and keep the mindshare of programmers,
and had to provide free development tools and cultivate a healthy community.
Power users have largely adopted Open Source Software for most of their infrastructure,
whereby the source code is available and can be copied, modified and redistributed.
This legal right to redistribute is more than many live computing systems of yore
used to provide, and without strings attached,
which lowers the economic cost of contributing modifications,
and leads to more third-party modifications
despite the increased difficulty of modifying dead computer systems.
At the same time, the modifications made possible by Open Source
do not generally apply to the running live system, and therefore
do nothing to improve the immediate development feedback loop of programmers
or empower the marginally proficient user.

Conversely, it is a fact most people, most of the time,
have no desire to tune their engine;
indeed, not only are they not proficient enough to do it,
they are only likely to break things and harm themselves if they modify anything.
Having innards of the system easily visible, accessible and modifiable
while not caring about them is not just an ugly aesthetic distraction:
it's an attractive nuisance;
it's inviting trouble without bringing any actual value.
Alan Perlis said that "A programming language is low level
when its programs require attention to the irrelevant".
If the programming system constantly (or randomly) requires
attention to things the user doesn't care about,
rather than abstract them away, then it is actually low-level,
and not high-level.
"Any time you're asking the user to make a choice they don't care about,
you have failed the user" — Jeff Atwood

The solution is thus to keep the hood closed by default,
and provide users with easy ways both
to abstract over details they don't care about,
and to open up the innards of the engine when needed.
Of course, since many people are each interested in small, different, parts
of a big complex system, this mechanism should allow not for a fixed option
but for arbitrary points of view, themselves programmable by the user.
The challenge, as compared to existing systems live or dead,
is thus to provide a such "reflective" mechanism
for the user to interactively explore and shape the semantics of the system.

@subsection{Homoiconicity and Dualism}

Regarding homoiconicity or dualism,
we find that actual live computing systems,
to achieve decent performance, always rely on "optimizations" such as dynamic compilation
that take advantage of an enforced segregation of code and data,
within contexts that are invalidated when the code is modified,
which is hopefully relatively infrequent.
Much of the advantages of dead systems can similarly be achieved
by creating such contexts inside which relevant fragments of code are dead, immutable,
or rather immuted, with any attempt to mutate the code,
if found to be itself valid, invalidating the relevant context before to create a new one,
at the relatively small cost of possibly frequent but simple validity checks
and possibly expensive but rare context switch.

Conversely, even in a computer system that most strictly enforces dualism,
some homoiconicity is inevitable at some point in building the system.
Indeed, the system requires a meta-system
to build it, debug it, install it, configure it, upgrade it, etc.
In this meta-system, the system's code is data manipulated by development tools.
This meta-system itself must be programmable, and so needs a meta-meta-system, and so on.
Because the regression cannot be infinite, for bootstrap purposes,
one of these meta-systems needs allow the mixing code and data.
The code has to read as data from storage files before its pages can be marked as "code"
and loaded into a separate code cache;
and the files had to be written as data by a compiler at some point.
So these systems have a loop that allows to see code as data and data as code.

Nevertheless, dead computer systems can make the development loop extremely long
some bits being manipulated as data and the same bits being executed as code;
at such times, it becomes generally too inefficient to use that loop directly
for interactive development and instead any dynamic interaction will be based
on implementing a virtual machine on top of the static code substrate,
and only providing homoiconic interaction on top of that virtual machine.
Besides, this very phenomenon is why we program microprocessors to begin with,
rather than configure FPGAs or wait for ASICs to be produced:
we are indeed ready to sacrifice a lot of performance
to achieve acceptable latency in the code mutation loop;
for only through code mutation may we navigate as we desire
through the space of possible computer behaviors,
a space too large, indeed beyond astronomically large,
for more than a tiny fraction of it to be statically encoded in any dead computer system.
Therefore even the deadest of systems possesses ways to work around its lack of homoiconicity.
And these workarounds soon enough become favorite techniques for all decent programmers.


@subsection{Dynamic and Static Approaches}

Thus the success of "scripting" languages that can bring life to otherwise dead systems:
even in the clumsiest of static programming systems that doesn't make it easy
to interactively manipulate high-level data structures,
one can write an interpreter for such a scripting language,
using strings of characters as a data representation for code,
and from there on your system has life.
Indeed, much of the success of Unix,
besides the legal and technical ease of porting it,
can be attributed to the interactive expressive power of its shell,
as compared to the batch command processors of contemporary rival systems:
the unix shell was the first massively successful "scripting language",
despite it being a haphazard pile of ill-conceived features
that brings horror to the heart of any programming language designer.
It is common practice nowadays that a large enough system
written in a static programming language will have
some kind of scripting language to extend it;
the practice is so common that these days,
there is no need to reinvent such languages from scratch,
for there now exist many somewhat efficient implementations
of somewhat reasonably designed dynamic language
that can be easily "embedded" in such applications.
Thus, dead programming systems may have a static language at their heart (such as C for Unix),
programmers have learned to program them mostly with dynamic "scripting" language,
for only with such dynamic tools can developers automate the drudgery
of writing, building, testing and debugging
both the application engine and higher-level programs that use this engine.

Conversely, even the most dynamic programming languages
quickly grow a "foreign function interface" to access functions and data structures
written in low-level static languages available on the same underlying software architecture.
Indeed, it is the ability to easily access functionality available in C libraries
that made the success of Tcl, Perl, Python and Ruby as dynamic programming languages.
The ability to call into a static language where performance or compatibility matters
is an important feature for any dynamic language,
whether it be inline assembly or call to C code written in separate files,
and live programming systems will dynamically call a C compiler
and link the resulting code in the current image if needed.
There again, clumsy as they might be, many infrastructures exist to facilitate
the development of such interfaces.
Moreover, many dynamic programming languages also include the ability to write code
in a special static programming dialect, with static types and other restrictions,
which allows for extra performance and compatibility with low-level programs,
at the expense of some combination of dynamicity, expressiveness, conciseness and safety.


@bold{XXX XXX TO BE CONTINUED XXX XXX}


If no computing, just result, not program.
Something changes.

@subsection{Persistence and Impermanence}

@subsection{High-Level and Low-Level}

@section{Reconciling the Live and the Dead}

The same general argument applies to all attributes
that characterize live and dead computing systems,
which can be reduced to Alan Perlis's famous epigram,
"One man's constant is another man's variable".
Live and dead are two opposite points of view
that each are useful in their respective context:
where the code is variable versus where it is constant.

The underlying reality behind both points of view is that
the interaction with humans proficient enough to create useful software
is itself the ultimate scarce resource in software engineering.
At one extreme, this means we need to support Live Computing
as a way to leverage this scarcest of resources at development time.
At the other extreme, this means that human interaction doesn't scale
and we need to support Dead Computers as the common case at runtime.

A better system would allow to smoothly adjust from one to the other,
so a programmer can have a live interaction with the parts he's interested in
while the rest of the system can run predictably as a dead system.

@XXX{
Even programmers only care about a narrow aspect of the system at any given time,
and may only be proficient to modify but a small subset of it.

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


Recreating features by piling on layers of complexity.

}

In Lisp, you don't write a specification of the program,
you write a specification of how to build the program.

Hayekian evolution vs "intelligent" design.


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
