ASDF 3, or Why Lisp is Now an Acceptable Scripting Language
===========================================================

And by Lisp, I mean Common Lisp.


Viewing the article
-------------------

HTML and PDF versions of the extended version of the article (26 pages)
can be found here:

  * http://fare.tunes.org/files/asdf3/asdf3-2014.html
  * http://fare.tunes.org/files/asdf3/asdf3-2014.pdf


HTML and PDF versions of the short version of the article (8 pages),
as submitted to [ELS 2014](http://www.european-lisp-symposium.org/),
can be found here:

  * http://fare.tunes.org/files/asdf3/asdf3-els2014.html
  * http://fare.tunes.org/files/asdf3/asdf3-els2014.pdf


Notes
-----

The article can be compiled using PLT Racket's Scribble, from
[asdf3-2014.scrbl](https://github.com/fare/asdf3-2013/blob/master/asdf3-2014.scrbl).
See the various [Makefile](https://github.com/fare/asdf3-2013/blob/master/Makefile) targets.

This is an article I wanted to write in 2013, but failed.
In 2013, I only produced the following slides (using org-mode),
for an ASDF "tutorial" at ELS 2013:
[els-slides.org](https://github.com/fare/asdf3-2013/blob/master/els-slides.org)

In 2014, I [presented at ELS 2014](http://medias.ircam.fr/x8fb915) the slides in
[scripting-slides.rkt](https://github.com/fare/asdf3-2013/blob/master/scripting-slides.rkt),
on the topic of using CL as a scripting language,
including deploying executables and image life-cycle hooks.



TODO
----

 * do something with the aborted tutorial material in obsolete/ or remove it.
 * complete the essay on Live Programming vs Cult of Dead programming, in
   [ltu_culture_war.scrbl](https://github.com/fare/asdf3-2013/blob/master/ltu_culture_war.scrbl),
   then write a presentation about it.
 * complete the presentation in
   [traverse-slides.rkt](https://github.com/fare/asdf3-2013/blob/master/traverse-slides.rkt)
   on the story of bug that begat ASDF 3,
   exploring subtleties in the ASDF dependency model,
   and a surprising conclusion. (My article's Appendix F)
 * Maybe have a presentation on a laundry list of new features in ASDF 3.1 since ASDF 2 ? Meh.
