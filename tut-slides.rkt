#lang at-exp racket ;;-*- Scheme -*-
(require slideshow
	 slideshow/code
	 scheme/gui/base
	 "utils.rkt")

(define ~ @t{ })
(define *blue* (make-object color% "blue"))
(define *red* (make-object color% "red"))
(define *grey* (make-object color% 200 200 200))
(define (url x) (colorize (tt x) *blue*))
(define (red x) (colorize x *red*))
(define (grey x) (colorize x *grey*))

(define (emph x) (red x))

(define (title x) (text x (cons 'bold 'default) 38))

(define slides
  (make-keyword-procedure
   (lambda (kws kvs repeats . lines)
     (for ([i repeats])
       (let ((m (λ (xs) (map (λ (x) (x i)) xs))))
	 (keyword-apply
	  slide kws (m kvs) (m lines)))))))

(define (always x) (lambda (i) x))
(define (repeat-fun test n iftesttrue [iftestfalse ~] [ifnorepeat ~])
  (lambda (i)
    (cond
     ((not i) ifnorepeat)
     ((test i n) iftesttrue)
     (#t iftestfalse))))
(define (if= n x [y ~] [z ~]) (repeat-fun = n x y z))
(define (if<= n x [y ~] [z ~]) (lambda (i) (if (<= i n) x y)))
(define (if>= n x [y ~] [z ~]) (lambda (i) (if (>= i n) x y)))

(define (?highlight n m object [shaded grey] [highlit red] [normal identity])
  (if n
      (if (eqv? n m)
	  (highlit object)
	  (shaded object))
      (normal object)))

(define (tASDF3)
  (title "ASDF 3 Tutorial"))

(define (tslide _title . body)
  (keyword-apply slide '(#:title) (list (title _title)) body))

(tslide "ASDF 3"
  @t{Another System Definition Facility, version 3}
  ~
  @t{What, How, and Why}
  ~
  @t{François-René Rideau <tunes@"@"google.com>})


(tslide "Plan"
  @t{What}
  ~
  @t{How to use it, extend it}
  ~
  @t{Why this design})

(tslide "What"
  @t{@[CL] build system}
  ~
  @t{@emph{de facto} standard}
  ~
  @t{Recent successor to ASDF})

(tslide "What"
  @t{In a file foo.asd:}
  ~
  @clcode{
   (defsystem foo
     :depends-on (bar baz)
     :components
     ((:file "file1")
      (:file "file2" :depends-on ("file1"))
      (:file "file3" :depends-on ("file1"))
      (:file "file4" :depends-on ("file2" "file3"))))})

(tslide "When"
  @t{2002-2004 Dan Barlow et al. ASDF 1.85}
  @t{2004-2006 Christophe Rhodes et al. ASDF 1.97}
  @t{2006-2009 Gary King et al. ASDF 1.369}
  @t{2009-2012 François-René Rideau et al. ASDF 2.26}
  @t{2012-2013 François-René Rideau et al. ASDF 3.0.1})

(tslide "Before"
  @t{197X DEFSYSTEM on MIT Lisp Machine} ;; rule-based, described Chine Nual
  @t{198X Build, Pitman article} ;; research
  @t{198X SCT on Symbolics Lisp Machine} ;; components and rules
  @t{1990-1995 mk-defsystem} ;; component-based
  @t{199X defsystem in Allegro, LispWorks})

(tslide "After"
  @t{ASDF 4?}
  @t{Quick-build}
  @t{XCVB}
  ~
  @t{Racket?})

(tslide "What ASDF does"
  @t{Build in current image}
  ~
  @t{Compile and load Lisp code}
  ~
  @t{Provide extensible object model})

(tslide "What ASDF does not"
  @t{Download code (see Quicklisp)}
  ~
  @t{Solve version hell (check only)}
  ~
  @t{Build non-Lisp stuff (awkward)})

(tslide "New in ASDF2"
  @t{Robustness: pathname abstractions}
  @t{Portability: 15 implementations, 4 OSes}
  @t{Configurability: configuration files}
  @t{Usability: plenty of features!}
  @t{Hot-Upgradability: solve the social issue}
  @t{Less Slow: don't use lists})

(tslide "Some ASDF2 features"
  @t{load-system, test-system}
  @t{defsystem-depends-on}
  @t{force, force-not, require-system}
  @t{encodings}
  @t{around-compile, compile-check})

(tslide "New in ASDF3"
  @t{Fixed object model}
  @t{Portability Layer: UIOP}
  @t{More Features!}
  @t{Auto-Upgrade, no downgrade}
  @t{Faster: more caching})

(tslide "Some ASDF3 features"
  @t{program-op, image-restore-hook}
  @t{uninteresting-warning, deferred-warnings}
  @t{fasl-op, concatenate-source-op}
  @t{fallback systems, pre-built systems}
  @t{plan can be queried}
  @t{POIU})

(tslide "Some UIOP features"
  @t{subpathname, parse-unix-namestring, ensure-pathname}
  @t{chdir, with-temporary-file, directory-files}
  @t{run-program, slurp-input-stream, getenv}
  @t{dump-image, restore-image, hooks, command-line-arguments}
  @t{compile-file*, with-muffled-compiler-conditions}
  @t{define-package, with-upgradability})

(tslide "How"
  @t{How to use it}
  @t{How to configure it}
  @t{How to define a new system}
  @t{How to use extensions}
  @t{How the object model works}
  @t{How to extend ASDF})

(tslide "How to use ASDF")

(tslide "How to use ASDF"
  @t{(asdf:load-system :inferior-shell)}
  @t{(in-package :inferior-shell)}
  @t{(run `(pipe (echo ,(* 90 137)) (tr "1032" "HOLE")))}
  @t{(run `(grep Mem "/proc/meminfo") :output :lines)})

(tslide "Before to use it"
  @t{(require "asdf")}) ;; CLISP: not :asdf
  @t{#-asdf2 (error "You lose")}
  @t{(asdf:load-system :asdf)}

(tslide "Before to use it, the hard way"
  @t{see slime/contrib/swank-asdf.lisp}
  ~
  @t{see lisp/setup.lisp from quux})

(tslide "Good Style"
  @t{No in-package}
  @t{Only defsystem forms for foo, foo/bar}
  @t{Any classes, methods from defsystem-depends-on}
  @t{No other methods, no side-effect, no pushing features})

(tslide "Simple file calling ASDF"
  @t{(require :asdf)}
  @t{(asdf:upgrade-asdf)}
  @t{(in-package :asdf)}
  @t{(load-system :some-dependencies)}
  @t{(load (subpathname (load-pathname) "foo.lisp"))})

(tslide "More complex build file"
  @t{require asdf, load an upgrade}
  @t{configure asdf, twice!})

(tslide "Using CL-Launch"
  #|
  @verbatim{
#!/bin/sh
":" ; DIR="$(cd $(basename "$0");pwd)" ; exec cl-launch -l ccl -S "$DIR//:" -i "$0" -- "$@"
(some lisp code)
}|#)


(tslide "Before ASDF"
  @t{Figure paths for each and every library}
  @t{Each library configured differently}
  @t{No incremental build}
  @t{Build not extensible})

(tslide "How to configure ASDF")

(tslide "How to configure ASDF"
  @t{Source Registry}
  @t{Output Translations}
  @t{Optimization, Verbosity, etc.})

(tslide "Default Installation Paths"
  @t{No need to configure}
  @t{~/.local/share/common-lisp/source/}
  @t{/usr/local/share/common-lisp/source/}
  @t{/usr/share/common-lisp/source/}
  @t{FASLs under ~/.cache/common-lisp/})

(tslide "Source Registry, via config file"
  @t{~/.config/common-lisp/source-registry.conf}
  @clcode{(:source-registry
            (:directory "/myapp/src")
            (:tree "/home/tunes/cl")
          :inherit-configuration)}) ;; Unlike ASDF 1, forgiving of no final /

(tslide "Source Registry, via modular config file"
  @t{~/.config/common-lisp/source-registry.conf.d/myapp.conf}
  @clcode{(:directory "/myapp/src")})

(tslide "Source Registry, via environment"
  @t{export CL_SOURCE_REGISTRY=/myapp/src/:/home/tunes/cl//:})

(tslide "Source Registry, via Lisp evaluation"
  @clcode{(asdf:initialize-source-registry
      `(:source-registry
          (:directory ,appdir)
          (:tree ,librootdir)
          :inherit-configuration))})

(tslide "Old Style central registry"
  @t{(pushnew #p"/myapp/src/" asdf:*central-registry* :test 'equal)}
  ~
  @t{ASDF 1 was unforgiving if you forget the trailing /}
  ~
  @t{ASDF 2 has asdf::getenv, now uiop:getenv})

(tslide "Output Translations"
  @t{/myapp/src/foo.fasl}
  @t{=> /home/tunes/.cache/common-lisp/acl-9.0-linux-x86/myapp/src/foo.fasl}
  @t{=> /home/tunes/.cache/common-lisp/sbcl-1.1.0-linux-x64/myapp/src/foo.fasl})

(tslide "Output Translations, via config file"
  @t{~/.config/common-lisp/asdf-output-translations.conf}
  @clcode{
    (:output-translations
       (t (,cache-root :implementation))
       :ignore-inherited-configuration)})

(tslide "Output Translations, via modular config file"
  @t{~/.config/common-lisp/asdf-output-translations.conf.d/foo.conf}
  @clcode{
    ("/myapp/src/" ("/var/clcache" :implementation "myapp/src"))
  })

(tslide "Output Translations"
  @t{export ASDF_OUTPUT_TRANSLATIONS=/:/some/cache/dir/:}
  @clcode{(asdf:initialize-output-translations
      `(:output-translations
          (t (,cache-root :implementation))
          :ignore-inherited-configuration))})

(tslide "Output Translations, current_dir/sbcl-1.2-x86/foo.fasl"
  @clcode{(asdf:initialize-output-translations
      `(:output-translations
          (t (:root :**/ :implementation :*.*.*))
          :ignore-inherited-configuration))})

(tslide "Using Quicklisp and clbuild"
  @t{(load "quicklisp/setup.lisp") does it all}
  ~
  @t{I'm not sure about clbuild — use the source-registry})

(tslide "How do I find a library?"
  @t{Just use quicklisp}
  ~
  @t{Google it, search Cliki, cl-user.net}
  ~
  @t{Ask the community, e.g. irc.freenode.net #lisp})

(tslide "Where do I download it?"
  @t{Just use quicklisp}
  ~
  @t{To some place in your source-registry}
  ~
  @t{zero conf: ~/.local/share/common-lisp/source/})

(tslide "Other configuration"
  @t{Optimization, Verbosity, etc.}
  ~
  @t{setf in a build file}
  ~
  @t{sbcl --load build.lisp}
  ~
  @t{For portability, use cl-launch})


(tslide "Creating Basic ASDF Systems")

(tslide "Creating Basic ASDF Systems"
  @t{foo.asd}
  @clcode{(asdf:defsystem foo 
            :components
            ((:file "foo")))})

(tslide "Depending on other systems"
  @clcode{(asdf:defsystem foo
            :depends-on (:alexandria :cl-ppcre)
            :components
            ((:file "foo")))})

(tslide "Multiple files"
  @clcode{(asdf:defsystem foo ...
            :components
            (:file "pkgdcl")
            (:file "foo" :depends-on ("pkgdcl"))
            (:file "bar" :depends-on ("pkgdcl")))})

(tslide "Typical small system"
  @clcode{(asdf:defsystem foo ...
            :components
            ((:file "pkgdcl")
             (:file "specials" :depends-on ("pkgdcl"))
             (:file "macros" :depends-on ("pkgdcl"))
             (:file "utils" :depends-on ("macros"))
             (:file "runtime" :depends-on ("specials" "macros"))
             (:file "main" :depends-on ("specials" "macros"))))})

(tslide "Bigger system: divided in modules"
  @clcode{(asdf:defsystem foo ...
            :components
            ((:module "base"
                :components ...)
             (:module "runtime"
                :depends-on ("base")
                :components ...)
             ...))})

(tslide "Logical Modules, same directory"
  @clcode{(asdf:defsystem foo ...
            :components
            ((:module "base"
                :pathname ""
                :components ...)
             ...))})

(tslide "Pathname override"
  @clcode{(:file "foo/bar")}
  @clcode{(:file "foo" :pathname "../sibling-dir/foo")}
  @clcode{(:file "foo" :pathname #p"../sibling-dir/foo.LiSP")})

(tslide "Sibling directories"
  @clcode{(:file "../sibling-dir/foo")}
  @clcode{(:module "../sibling-dir/foo")}
  @clcode{(:file "foo" :pathname "../sibling-dir/foo")}
  @clcode{(:file "foo" :pathname #p"../sibling-dir/foo.LiSP")})

(tslide "Punting on fine-grained dependencies"
  @clcode{(asdf:defsystem foo
            :serial t
            :components
            ((:file "pkgdcl")
             ...
             (:file "main")))})

(tslide "Serial Dependencies"
  @t{Scope of :serial t is the current module or system}
  @t{not its submodules or systems.}
  @t{You can easily nest serial / parallel dependencies})

(tslide "Explicit Dependencies"
  @t{:depends-on ("foo" "bar/baz" "quux")})

(tslide "Other files in a project"
  @t{README}
  @t{LICENSE}
  @t{TODO}
  @t{.git})

(tslide "Using Quickprojects"
  @t{Automatically create the skeleton})

(tslide "How to map packages and systems")

(tslide "Strategy 1: one package per system"
  @t{The traditional way}
  @t{system foo, package foo}
  @t{system cl-foo, package foo (yuck)}
  @t{system cl-foo, package cl-foo}
  @t{file pkgdcl.lisp or package.lisp})

(tslide "Strategy 1b: one package per subsystem"
  @t{Whether you subsystem is a second system or a module}
  @t{system foo, system foo/bar}
  @t{see asdf itself, iolib})

(tslide "Strategy 2: interface vs implementation package"
  @t{package foo, package foo-impl}
  @t{same system foo, or}
  @t{two systems foo/interface and foo/implementation}
  @t{See cl-protobufs})

(tslide "Strategy 3: one package per file"
  @t{More discipline, reduces mess}
  @t{dependencies implicit from defpackage}
  @t{See ASDF 3}
  @t{As in faslpath, quick-build})

(tslide "uiop:define-package vs defpackage"
  @t{Works well with hot-upgrade}
  @t{(:mix "foo" "bar")})

(tslide "Advanced ASDF Systems")

(tslide "Character Encodings")

(tslide "Finalizers")

(tslide "Using Extensions: CFFI Grovel")

(tslide "Load-only class"
  @t{Beware: defeats executable creation!}
  @t{Maybe instead you want run-time evaluation}
  @t{(foo '(some data))}
  @t{or even}
  @t{(eval '(some expression))})

(tslide "The ASDF object model")

(tslide "Components, Operations, Actions"
  @t{Components describe how your source code is organized.}
  @t{Operations describe processes or stages of processing of a component.}
  @t{Actions are pairs of an operation and a component on which to effect the operation.}
  @t{@emph{The dependency graph is a graph of actions, not of components.}})

(tslide "Lisp Components"
  @t{system, module}
  @t{cl-source-file}
  @t{(also cl-source-file.lsp, cl-source-file.cl)})

(tslide "Lisp Operations"
  @t{load-op, compile-op. Propagate downward.}
  @t{prepare-op. Propagate upward; Also sideway load-op of dependencies.})

(tslide "Static Plan then Act"
  @t{Traverse.}
  @t{sequential-plan.}
  @t{POIU.})

(tslide "Troubleshooting")

(tslide "Backtrace")

;;Size: what is the biggest solution? the smallest?

(tslide "Trace"
 @t{What should I trace?}
 @t{perform-plan, perform}
 @t{input-files, output-files})

(tslide "Error")

(tslide "Why"
  @t{Design constraints}
  @t{Spirit of Live Computing}
  @t{No auto dependency})

(tslide "Dependency generation?"
  @t{asdf-dependency-grovel})

(tslide "Components of type SYSTEM ?"
  @t{Yes: that's what ASDF:DEFSYSTEM does!}
  ~
  @t{No: mk-defsystem idiom, not supported})

(tslide "Cool feature ?"
  @t{program-op}
  @t{asdf-finalizers}
  @t{defsystem foo/bar}
  @t{poiu as an add-on})

(tslide "Support other languages?"
  @t{Can they be loaded in-image?}
  @t{Yes: CL becomes a platform}
  @t{No: second class citizens})

(tslide "Horror .asd file?"
  @t{mcclim.asd before ASDF 3 refactoring}
  ~
  @t{gbbopen.asd is still pretty complex}
  ~
  @t{Really, any .asd file with non-defsystem forms.})
