Hello world (and more)
======================

CyberLisp's Hello world:

    (print "Hello world")

But let's look at some more interesting examples...

each
----

`each` applies a function to all elements of a collection for effect:

    (defun each ((fun <function>) coll)
      (let ((iter (iter coll)))
        (while (has-next iter)
          (funcall fun (now iter))
          (next iter))))

    (each #'print (list 1 2 3))
    1
    2
    3

Here, we can see:

* Types use the `<type-name>` convention.
* Function parameters can be typed; types are checked at runtime.
* All collection utilities use a generic iteration protocol: `iter`
  returns an iterator for a collection; `has-next` checks whether
  there's still an element available (misnomer); `now` returns that
  element; and `next` steps the iterator to the next element.
* CyberLisp is a Lisp-2, so use `funcall` or `apply`.

map
---

`map` is like `each`, but collects the results of applying the
function:

    (defun map ((fun <function>) coll &key (into (list)))
      (let ((iter (iter coll)))
        (while (has-next iter)
          (add into (funcall fun (now iter)))
          (next iter)))
      into)
    
    (defvar list (list 5 6 7))
    (defvar results (map (lambda (x) (* x 2)) list))
    => [10,12,14]
    (map (lambda (x) (* x 10)) list into: results)
    => [10,12,14,50,60,70]
    
* CyberLisp has optional, keyword and rest parameters. Optionals and
  keywords can have default values.
* `into` is a keyword parameter, by default bound to a fresh, empty
  list.  `into` can be used to specify an existing collection.

every
-----

`every` returns true iff a predicate holds for all elements of a
collection:

    (defun every ((pred <function>) coll)
      (block exit
        (let ((iter (iter coll)))
          (while (has-next iter)
            (unless (funcall pred (now iter))
              (return-from exit false))
            (next iter)))
        true))

* `block` binds the name `exit` (or any other name) to the current
  continuation.  `return-from` invokes the continuation with a value,
  thereby leaving the `while` loop, and returning the value (`false`)
  from the block.  (In contrast to Common Lisp's `block`, CyberLisp's
  exits are first class objects.)

map*
----

`map*` applies a function to the elements of multiple collections in
parallel, and stops as soon as one of them is exhausted:

    (defun map* ((fun <function>) &rest colls &key (into (list)))
      (let ((iters (map #'iter colls)))
        (while (every #'has-next iters)
          (add into (apply fun (map #'now iters)))
          (each #'next iters)))
      into)
    
    (map* #'+ (list 1 2 3) (list 1 2 3 4))
    => [2,4,6]
