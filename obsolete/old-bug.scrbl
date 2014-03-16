#lang scribble/sigplan @nocopyright

@(require scribble/base
          "utils.rkt")

@title{The Old Bug That Did(n't) Matter}

@section{Programming in the Small}

Indeed, most changes to simple macros
that affect how code is generated also require
change in clients depending on the modified code;
and this only more so if the programmers follow discipline such as the
@hyperlink["http://random-state.net/log/3390120648.html"]{@cl{CALL-WITH-} style} whereby
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
I will tell you the story of @(ASDF),
and how I came to several interesting insights while fixing this bug,
not just about @(ASDF) itself, but also about Lisp, Lisp programmers,
and software in general.

You may freely skip the historical and/or technical sections
that do not interest you, and jump straight to the conclusion.
Or you may stay for a ride along the history and guts of @(ASDF).

MENTION the recompile and retry restart

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
