Hello world (and more)
---

CyberLisp's Hello world:

 (print "Hello world")

But let's look at some more interesting examples...

 (defun each ((fun <function>) coll)
   "Applies a function to each element of a collection for effect."
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
* All collection utilities use a generic iteration protocol (Dylan's).
* CyberLisp is a Lisp-2, so use `funcall` or `apply`.

Another one:

 (defun map ((fun <function>) coll &key (into (<list>)))
   "Applies a function to each element of a collection and returns a
 collection with the results of each application.  The `into' keyword
 argument can be used to supply a different collection to hold the
 results."
   (let ((iter (iter coll)))
     (while (has-next iter)
       (add into (funcall fun (now iter)))
       (next iter)))
   into)

* CyberLisp has optional, keyword and rest parameters. Optionals and
  keywords can have default values.

Another one:

 (defun every ((pred <function>) coll)
   "Returns true iff every element of a collection satisfies the predicate."
   (block exit
     (let ((iter (iter coll)))
       (while (has-next iter)
         (unless (funcall pred (now iter))
           (return-from exit false))
         (next iter)))
     true))

* `block` binds the name `exit` (or any other name) to the current
  continuation.  `return-from` invokes the continuation with a value,
  thereby leaving the `while` loop, and returning `false` from the
  block.  (In contrast to Common Lisp's `block`, CyberLisp's exits are
  first class objects.)

And, because it shows how nice Lisp is, a multi-collection `map`:

 (defun map* ((fun <function>) &rest colls &key (into (<list>)))
   "Applies a function to the elements of N collections and returns a
 collection with the results of each application.  The function is
 called with N positional arguments, each taken from the collections
 from left to right.  The shortest collection determines how many times
 the function is called.  The `into' keyword argument can be used to
 supply a different collection to hold the results."
   (let ((iters (map #'iter colls)))
     (while (every #'has-next iters)
       (add into (apply fun (map #'now iters)))
       (each #'next iters)))
   into)
 
 (map* #'+ (list 1 2 3) (list 1 2 3 4))
 [2,4,6]
