#lang scribble/sigplan @nocopyright

@(require scribble/base
          "utils.rkt")

@;TODO: Parameterize whether or not to include notes about ASDF1 and ASDF2
@;TODO: Have CSS output hide it or not, a la Google 

@title{@(ASDF) Tutorial}

So, how do we use @(ASDF) in practice?

Let's start with the easiest examples...

<Xach> I would like more scenario-driven examples,
e.g. "If you have a piece that has to be compiled via a separate process,
here's how the system file can help you do it."
Not ASDF3-specific, I'd like to see more things like that in general.
"If you want your FASL files for project/ to be stored in project/fasls/ rather than globally,
here's how to do it."
I suppose that's more manual than tutorial

