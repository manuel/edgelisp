CyberLisp
=========

CyberLisp aims to be a well-designed, compact, yet expressive Lisp,
for in-browser as well as desktop and server-side scripting.

CyberLisp is mostly an amalgamation of Common Lisp, Dylan, Goo, and
Smalltalk.  My motto is that *Lisp ain't broke*, so I don't try to fix
it.

The following "plumbing" is implemented, but most of it is still
missing the right "porcelain" and docs:

* Lists, dictionaries, strings, numbers, booleans, null
* Classes with single inheritance, integrated with the JS types
* Single-dispatching generic functions
* Separate function and variable namespaces (Lisp-2)
* Optional, keyword, and rest parameters
* Runtime type-checked function parameters
* Restartable exceptions; first-class nonlocal exits; stack unwind protection
* `defmacro` with quasiquotation
* Slot access through (overridable) generic functions
* Generalized references (places)
* Inline JavaScript, with escaping back into Lisp, (and back into JS...)

CyberLisp will soon get these additional features:

* Optional dynamic scoping
* User-extensible pretty-printing

CyberLisp probably will get these features in the medium-term:

* Convenient object literals and destructuring
* Everything-is-an-object, including null
* No-applicable-method trap
* Before-, around-, and after-advice for methods
* Object-specific methods
* Stack inspection; programmable debugger
* `CyberSlime` as well as an in-browser REPL
* Module system (?)
* Hygienic macro system (?)
* Coroutines (?)

Run
---

Requires SpiderMonkey at the moment (but only for `readline()` and
`print()`, the rest is "standard" JavaScript).

Use `./cyber` to start a REPL.

*Manuel Simoni, 2008-11-10*
