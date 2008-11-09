/* CyberLisp: A Lisp that compiles to JavaScript 1.5.
   
   Copyright (C) 2008 by Manuel Simoni.
   
   CyberLisp is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.
   
   CyberLisp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA. */
   
/* Lisp runtime: this file should contain all functions needed to run
   compiled Lisp code.  At the moment, it does have some dependencies
   on functions in `lisp.js', though.

   Lisp code that does use `eval' will always need to include the
   compiler, `lisp.js', too. */

/*** Functions ***/

function lisp_arity_min(length, min)
{
    if (length < min)
        throw Error("Too few arguments ");
}

function lisp_arity_min_max(length, min, max)
{
    lisp_arity_min(length, min);
    if (length > max)
        throw Error("Too many arguments ");
}

/* Returns the sequence of arguments to which the rest parameter is
   bound; called with a function's arguments and the count of
   positional (required and optional) parameters of the function. */
function lisp_rest_param(_arguments, max) {
    var args = [];
    var offset = 1 + max; // Skip calling convention argument.
    var len = _arguments.length;
    for (var i = offset; i < len; i++) {
        args[i - offset] = _arguments[i];
    }
    return args;
}

/*** Truth ***/

function lisp_is_true(obj)
{
    return (obj !== false) && (obj !== null);
}

/*** Exceptions ***/

/* CyberLisp uses the same exception system as Dylan and Goo (which is
   basically Common Lisp's but with exception handlers and restarts
   unified into a single concept).
   
   What's noteworthy about this system is that throwing an exception
   does not unwind the stack -- an exception handler runs as a
   subroutine of the thrower, and thus may advise the thrower on
   different ways to handle an exceptional situation.  Unwinding the
   stack, if desired, has to be done manually through the use of a
   non-local exit.
   
   Together with `unwind-protect' ("finally"), this system is strictly
   more powerful than the exception systems of languages like Java and
   Python.  It should also be noted that non-unwinding, restartable
   exceptions pose no algorithmic or implementational complexity over
   ordinary, automatically unwinding exceptions. */

/* CyberLisp maintains a stack of exception handler frames, because it
   cannot use JavaScript's try/catch construct.  The handler stack
   grows downward, so that the most recently established exception
   handlers reside in the bottom-most, deepest frame.
   
   An exception handler frame is an object:
   
   { handlers: <list>, 
     parent_frame: <handler_frame> }
   
   handlers: a list of handler objects;
   parent_frame: the parent of the current frame or null if it is the
   top-most frame.

   A handler object:
   
   [ <type>, <fun> ]
   
   type: type of exception handled by this handler;
   fun: handler function.

   A handler function is called with two arguments: an exception and a
   next-handler function.  It has three possibilities: (1) return a
   value -- this will be the result of the `throw' that invoked the
   handler; (2) take a non-local exit, aborting execution of the
   thrower; (3) decline handling the exception by calling the
   next-handler function (without arguments), which will continue the
   search for an applicable handler stack-upwards. */

var lisp_handler_frame = null; // bottom-most frame or null

function lisp_bif_catch(_key_, handlers, fun)
{
    try {
        var orig_frame = lisp_handler_frame;
        lisp_handler_frame = { handlers: handlers, 
                               parent_frame: orig_frame };
        return fun(null);
    } finally {
        lisp_handler_frame = orig_frame;
    }
}

function lisp_bif_throw(_key_, exception)
{
    function handler_type(handler) { return handler[0]; }
    function handler_fun(handler) { return handler[1]; }

    function find_handler(exception, handler_frame)
    {
        if (!handler_frame) return null;
        var handlers = handler_frame.handlers;
        var type = lisp_type_of(exception);
        for (var i in handlers) {
            var handler = handlers[i];
            if (lisp_subtypep(type, handler_type(handler))) {
                return [handler, handler_frame];
            }
        }
        return find_handler(exception, handler_frame.parent_frame);
    }
    
    function do_throw(exception, handler_frame)
    {
        var handler_and_frame = find_handler(exception, handler_frame);
        if (handler_and_frame) {
            var handler = handler_and_frame[0];
            var frame = handler_and_frame[1];
            function next_handler(_key_)
            {
                return do_throw(exception, frame.parent_frame);
            }
            return (handler_fun(handler))(null, exception, next_handler);
        } else {
            lisp_error("No applicable handler", exception);
        }
    }

    return do_throw(exception, lisp_handler_frame);
}

/*** Escape functions ***/

function lisp_bif_call_with_escape_function(_key_, fun) {
    var token = {};
    var escape_function = function(_key_, result) {
        token.result = result;
        throw token;
    };
    try {
        return fun(null, escape_function);
    } catch(obj) {
        if (obj == token) {
            return token.result;
        } else {
            throw obj;
        }
    }
}

/*** Unwind protect ***/

function lisp_bif_call_unwind_protected(_key_, protected_fun, cleanup_fun)
{
    try {
        return protected_fun(null);
    } finally {
        cleanup_fun(null);
    }
}

/*** Other built-ins ***/

function lisp_bif_macroexpand_1(_key_, form)
{
    var macro = lisp_macro_function(form.elts[0].name);
    return macro(null, form);
}

function lisp_bif_print(_key_, object)
{
    print(lisp_show(object));
}

function lisp_bif_eq(_key_, a, b)
{
    return a == b;
}

/*** Types ***/

function lisp_type_of(obj)
{
    return obj.__proto__;
}

function lisp_bif_type_of(_key_, obj) 
{
    return lisp_type_of(obj);
}

/* Returns true iff type1 is a general subtype of type2, meaning
   either equal to type2, or a subtype of type2. */
function lisp_subtypep(type1, type2)
{
    if (type1 == type2) 
        return true;

    var supertype = type1.__proto__;
    if (supertype)
        return lisp_subtypep(supertype, type2);
    
    return false;
}

function lisp_bif_subtypep(_key_, type1, type2)
{
    return lisp_subtypep(type1, type2);
}

function lisp_check_type(obj, type)
{
    if (!lisp_subtypep(lisp_type_of(obj), type))
        lisp_error("Type error", obj);
}

/*** Classes and objects ***/

function lisp_bif_make_class(_key_)
{
    return {};
}

function lisp_bif_set_super_class(_key_, clsA, clsB)
{
    clsA.__proto__ = clsB;
}

function lisp_bif_make(_key_, cls)
{
    var obj = {};
    obj.__proto__ = cls;
    return obj;
}

function lisp_bif_slot(_key_, obj, name)
{
    lisp_assert_string(name);
    return obj[lisp_mangle_slot(name)] || null;
}

function lisp_bif_set_slot(_key_, obj, name, value)
{
    lisp_assert_string(name);
    return obj[lisp_mangle_slot(name)] = value;
}

function lisp_bif_has_slot(_key_, obj, name)
{
    lisp_assert_string(name);
    return obj.hasOwnProperty(lisp_mangle_slot(name));
}

function lisp_bif_get_method(_key_, obj, name)
{
    return obj[lisp_mangle_method(name)];
}

function lisp_bif_set_method(_key_, obj, name, fun)
{
    return obj[lisp_mangle_method(name)] = fun;
}

/*** More form manipulation functions ***/

function lisp_bif_symbol_name(_key_, symbol)
{
    lisp_assert_symbol_form(symbol);
    return symbol.name;
}

function lisp_bif_symbolp(_key_, form)
{
    return form.formt == "symbol";
}

function lisp_bif_compoundp(_key_, form)
{
    return form.formt == "compound";
}

/*** Lists ***/

function lisp_bif_list(_key_)
{
    var elts = [];
    for (var i = 1; i < arguments.length; i++) {
        elts.push(arguments[i]);
    }
    return elts;
}

function lisp_bif_list_elt(_key_, list, i)
{
    return list[i];
}

function lisp_bif_list_len(_key_, list, i)
{
    return list.length;
}

/*** Strings ***/

function lisp_bif_string_concat(_key_, s1, s2)
{
    return s1.concat(s2);
}

function lisp_bif_string_to_syntax(_key_, string)
{
    return { formt: "string", s: string };
}

/*** Misc ***/

function lisp_bif_is_typename(_key_, string)
{
    return lisp_is_type_name(string);
}

function lisp_bif_apply(_key_, fun, args, keys)
{
    return fun.apply(null, [ keys ].concat(args));
}

lisp_set("true", "true");
lisp_set("false", "false");
lisp_set("null", "null");

lisp_set_function("%%apply", "lisp_bif_apply");
lisp_set_function("%%call-unwind-protected", "lisp_bif_call_unwind_protected");
lisp_set_function("%%call-with-escape-function", "lisp_bif_call_with_escape_function");
lisp_set_function("%%catch", "lisp_bif_catch");
lisp_set_function("%%compoundp", "lisp_bif_compoundp");
lisp_set_function("%%eq", "lisp_bif_eq");
lisp_set_function("%%get-method", "lisp_bif_get_method");
lisp_set_function("%%list", "lisp_bif_list");
lisp_set_function("%%list-elt", "lisp_bif_list_elt");
lisp_set_function("%%list-len", "lisp_bif_list_len");
lisp_set_function("%%macroexpand-1", "lisp_bif_macroexpand_1");
lisp_set_function("%%make", "lisp_bif_make");
lisp_set_function("%%make-class", "lisp_bif_make_class");
lisp_set_function("%%print", "lisp_bif_print");
lisp_set_function("%%set-method", "lisp_bif_set_method");
lisp_set_function("%%set-super-class", "lisp_bif_set_super_class");
lisp_set_function("%%string-concat", "lisp_bif_string_concat");
lisp_set_function("%%string-to-syntax", "lisp_bif_string_to_syntax");
lisp_set_function("%%subtypep", "lisp_bif_subtypep");
lisp_set_function("%%symbol-name", "lisp_bif_symbol_name");
lisp_set_function("%%symbolp", "lisp_bif_symbolp");
lisp_set_function("%%throw", "lisp_bif_throw");
lisp_set_function("%%type-name-p", "lisp_is_type_name");
lisp_set_function("%%type-of", "lisp_bif_type_of");
lisp_set_function("has-slot", "lisp_bif_has_slot");
lisp_set_function("set-slot", "lisp_bif_set_slot");
lisp_set_function("slot", "lisp_bif_slot");
