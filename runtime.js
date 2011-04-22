/* CyberLisp: A Lisp that compiles to JavaScript 1.5.
   
   Copyright (C) 2008, 2011 by Manuel Simoni.
   
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
   compiled Lisp code.  At the moment, it does have dependencies on
   name mangling functions in `compiler.js', though.

   Lisp code that does use `eval' will always need to include the
   compiler too. */


/*** Strings ***/

function Lisp_string(s)
{
    this.s = s;
}

/*** Numbers ***/

function lisp_number(numrepr)
{
    return jsnums.fromString(numrepr);
}


/*** Functions ***/

function lisp_bif_fast_apply(_key_, fun, _arguments)
{
    return fun.apply(null, _arguments);
}


/*** Forms ***/

function Lisp_number_form(sign, integral_digits, fractional_digits)
{
    this.formt = "number";
    this.sign = sign;
    this.integral_digits = integral_digits;
    this.fractional_digits = fractional_digits;
}

function Lisp_string_form(s)
{
    this.formt = "string";
    this.s = s;
}

function Lisp_symbol_form(name)
{
    this.formt = "symbol";
    this.name = name;
}

function Lisp_compound_form(elts)
{
    this.formt = "compound";
    this.elts = elts;
}

/* This whole comments-as-forms business is silly, but atm I don't
   know another way to have comments ignored. */
function Lisp_comment_form(contents)
{
    this.formt = "comment";
    this.contents = contents;
}


/*** Utilities for generated code. ***/

function lisp_undefined_identifier(name, namespace)
{
    return lisp_error("Undefined " + namespace, name);
}

/* Used inside lambdas. */
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

/* Used inside lambdas for checking arguments against their types, if any. */
function lisp_check_type(obj, type)
{
    if (!lisp_subtypep(lisp_type_of(obj), type))
        lisp_error("Type error", obj);
}

function lisp_show(obj)
{
    // show is defined as a generic function in boot.lisp
    if (typeof(_lisp_function_show) !== "undefined")
        return _lisp_function_show(null, obj);
    else
        return JSON.stringify(obj);
}

function lisp_array_contains(array, elt)
{
    for (var i in array) {
        if (array[i] === elt) return true;
    }
    return false;
}

function lisp_iter_dict(dict, fun)
{
    for (var k in dict) {
        if (dict.hasOwnProperty(k))
            fun(k);
    }
}

function lisp_lists_equal(list1, list2)
{
    if (list1.length != list2.length) return false;
    for (var i = 0, len = list1.length; i < len; i++) {
        if (list1[i] !== list2[i]) return false;
    }
    return true;
}

// fixme: should SIGNAL error
function lisp_error(message, arg)
{
    throw Error(message + ": " + JSON.stringify(arg));
}

function lisp_assert(value, message, arg)
{
    if (!value) lisp_error(message, arg);
    return value;
}

function lisp_assert_not_null(value, message, arg)
{
    lisp_assert(value != null, message, arg);
    return value;
}

function lisp_assert_number(value, message, arg)
{
    // fixme
    lisp_assert(typeof value === "number", message, arg);
    return value;
}

function lisp_assert_string(value, message, arg)
{
    lisp_assert(typeof value === "string", message, arg);
    return value;
}

function lisp_assert_nonempty_string(value, message, arg)
{
    lisp_assert_string(value, message, arg);
    lisp_assert(value.length > 0, message, arg);
    return value;
}

function lisp_assert_function(value, message, arg)
{
    lisp_assert(typeof value === "function", message, arg);
    return value;
}

function lisp_assert_symbol_form(value, message, arg)
{
    lisp_assert(typeof value === "object", message, arg);
    lisp_assert(value.formt === "symbol", message, arg);
    lisp_assert_nonempty_string(value.name, message, arg);
    return value;
}

function lisp_assert_compound_form(value, message, arg)
{
    lisp_assert(typeof value === "object", message, arg);
    lisp_assert(value.formt === "compound", message, arg);
    lisp_assert_not_null(value.elts, message, arg);
    lisp_assert_not_null(value.elts.length, message, arg);
    return value;
}


/*** Built-in form manipulation functions ***/

/* Applies a Lisp function to the elements of a compound form, a
   simple form of destructuring.  The form's elements are simply
   supplied as the function's positional arguments. */
function lisp_bif_compound_apply(_key_, fun, form)
{
    var _key_ = null;
    var args = [ _key_ ].concat(form.elts);
    var thisArg = null;
    return fun.apply(thisArg, args);
}

/* Creates a compound form from all positional arguments, which must
   be forms. */
function lisp_bif_make_compound(_key_)
{
    var elts = [];
    for (var i = 1; i < arguments.length; i++) {
        var elt = arguments[i];
        lisp_assert(elt && elt.formt, "make-compound", elt);
        elts = elts.concat(elt);
    }
    return new Lisp_compound_form(elts);
}

/* Creates a compound form by appending all positional arguments,
   which must be compound forms or lists.  This fuzzyness in accepting
   both compound forms and lists enables the splicing in of forms
   supplied via the rest parameter. */
function lisp_bif_append_compounds(_key_)
{
    var elts = [];
    for (var i = 1; i < arguments.length; i++) {
        var elt = arguments[i];
        if (elt.formt === "compound") {
            elts = elts.concat(elt.elts);
        } else {
            lisp_assert(elt.length != null, "append-compounds", elt);
            elts = elts.concat(elt);
        }
    }
    return new Lisp_compound_form(elts);
}

function lisp_bif_compound_map(_key_, fun, compound)
{
    function js_fun(elt) {
        return fun(null, elt);
    }
    lisp_assert_not_null(fun);
    lisp_assert_compound_form(compound);
    return new Lisp_compound_form(compound.elts.map(js_fun));
}

function lisp_bif_compound_elt(_key_, compound, i)
{
    lisp_assert_compound_form(compound, "compound-elt", compound);
    var elt = compound.elts[i];
    return elt;
}

function lisp_bif_compound_len(_key_, compound)
{
    lisp_assert_compound_form(compound, "compound-len", compound);
    return compound.elts.length;
}

function lisp_bif_compound_elts(_key_, compound)
{
    lisp_assert_compound_form(compound, "compound-elts", compound);
    return compound.elts;
}

function lisp_bif_compound_slice(_key_, compound, start)
{
    lisp_assert_compound_form(compound, "compound-slice", compound);
    return new Lisp_compound_form(compound.elts.slice(start));
}

function lisp_bif_compound_emptyp(_key_, compound)
{
    lisp_assert_compound_form(compound, "compound-empty?", compound);
    return compound.elts.length === 0;
}

/*** Built-in string dictionaries ***/

/* This is the type of dictionaries used to hold keyword arguments,
   i.e. the one a function with an `&all-keys' signature keyword
   receives.  They're called string dictionaries because their keys
   can only be strings.
   
   By convention, all keys are prefixed with "%" (but not transformed
   like variables), so we never get in conflict with JS's special
   names, such as "prototype". */

function lisp_mangle_string_dict_key(name)
{
    lisp_assert_string(name);
    return "%" + name;
}

function lisp_is_string_dict_key(k)
{
    return k[0] === "%";
}

function Lisp_string_dict()
{
}

/* Turns an ordinary dictionary into a string dictionary.  May only be
   called if the caller has ensured that all keys are properly
   mangled, or that the dictionary is empty. */
function lisp_fast_string_dict(js_dict)
{
    js_dict.__proto__ = Lisp_string_dict.prototype;
    return js_dict;
}

function lisp_bif_string_dict_get(_key_, dict, key)
{
    return dict[lisp_mangle_string_dict_key(key)];
}

function lisp_bif_string_dict_put(_key_, dict, key, value)
{
    dict[lisp_mangle_string_dict_key(key)] = value;
}

function lisp_bif_string_dict_has_key(_key_, dict, key)
{
    return lisp_mangle_string_dict_key(key) in dict;
}


/*** Control Flow and Conditions ***/

/* CyberLisp uses the same exception system as Dylan and Goo (which is
   basically Common Lisp's but with exception handlers and restarts
   unified into a single concept).
   
   What's noteworthy about this system is that signaling an exception
   does not unwind the stack -- an exception handler runs as a
   subroutine of the signaler, and thus may advise the signaler on
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
   value -- this will be the result of the `signal' that invoked the
   handler; (2) take a non-local exit, aborting execution of the
   signaler; (3) decline handling the exception by calling the
   next-handler function (without arguments), which will continue the
   search for an applicable handler stack-upwards. */

var lisp_handler_frame = null; // bottom-most frame or null

function lisp_bif_bind_handlers(_key_, handlers, fun)
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

function lisp_bif_signal(_key_, exception)
{
    function handler_type(handler) { return handler[0]; }
    function handler_fun(handler) { return handler[1]; }

    function find_handler(exception, handler_frame)
    {
        if (!handler_frame) return null;
        var handlers = handler_frame.handlers;
        var type = lisp_type_of(exception);
        for (var i = 0, len = handlers.length; i < len; i++) {
            var handler = handlers[i];
            if (lisp_subtypep(type, handler_type(handler))) {
                return [handler, handler_frame];
            }
        }
        return find_handler(exception, handler_frame.parent_frame);
    }
    
    function do_signal(exception, handler_frame)
    {
        var handler_and_frame = find_handler(exception, handler_frame);
        if (handler_and_frame) {
            var handler = handler_and_frame[0];
            var frame = handler_and_frame[1];
            function next_handler(_key_)
            {
                return do_signal(exception, frame.parent_frame);
            }
            return (handler_fun(handler))(null, exception, next_handler);
        } else {
            lisp_error("No applicable handler", exception);
        }
    }

    return do_signal(exception, lisp_handler_frame);
}

function lisp_bif_call_with_escape_function(_key_, fun) {
    var token = {};
    var escape_function = function(_key_, result) {
        token.result = result;
        throw token;
    };
    try {
        return fun(null, escape_function);
    } catch(obj) {
        if (obj === token) {
            return token.result;
        } else {
            throw obj;
        }
    }
}

function lisp_bif_call_unwind_protected(_key_, protected_fun, cleanup_fun)
{
    try {
        return protected_fun(null);
    } finally {
        cleanup_fun(null);
    }
}

function lisp_bif_call_while(_key_, test_fun, body_fun)
{
    while(test_fun(null)) {
        body_fun(null);
    }
    return null;
}


/*** Multiple Dispatch ***/

function Lisp_generic()
{
    this.method_entries = [];
}

function Lisp_method_entry(method, specializers)
{
    this.method = method;
    this.specializers = specializers;
}

function lisp_bif_make_generic(_key_)
{
    return new Lisp_generic();
}

function lisp_make_method_entry(method, specializers)
{
    return new Lisp_method_entry(method, specializers);
}

function lisp_bif_put_method(_key_, generic, specializers, method)
{
    for (var i = 0, len = generic.method_entries; i < len; i++) {
        var me = generic.method_entries[i];
        if (lisp_lists_equal(me.specializers, specializers)) {
            me.method = method;
            return;
        }
    }
    generic.method_entries.push(lisp_make_method_entry(method, specializers));
}

function lisp_bif_params_specializers(_key_, params)
{
    var specializers = [];
    var sig = lisp_compile_sig(params.elts);
    for (var i = 0, len = sig.req_params.length; i < len; i++) {
        var param = sig.req_params[i];
        var specializer = param.specializer ? param.specializer : "object";
        var specs = [ new Lisp_symbol_form("%%identifier"),
                      new Lisp_symbol_form(specializer),
                      new Lisp_symbol_form("class") ];
        specializers.push(new Lisp_compound_form(specs));
    }
    return new Lisp_compound_form(specializers);
}

function lisp_bif_find_method(_key_, generic, arguments)
{
    var applicable_mes =
        lisp_find_applicable_method_entries(generic, arguments);
    if (applicable_mes.length === 0)
        return lisp_no_applicable_method(generic, arguments);
    var me = lisp_most_specific_method_entry(generic, applicable_mes);
    if (me)
        return me.method;
    else
        return lisp_no_most_specific_method(generic, arguments, applicable_mes);
}

function lisp_find_applicable_method_entries(generic, arguments)
{
    var actual_specializers = [];
    // start at 1 to skip over calling convention argument
    for (var i = 1, len = arguments.length; i < len; i++)
        actual_specializers.push(lisp_type_of(arguments[i]));
    var applicable_mes = [];
    var mes = generic.method_entries;
    for (var i = 0, len = mes.length; i < len; i++) {
        if (lisp_specializers_lists_agree(actual_specializers,
                                          mes[i].specializers)) {
            applicable_mes.push(mes[i]);
        }
    }
    return applicable_mes;
}

function lisp_specializers_lists_agree(actuals, formals)
{
    if (actuals.length != formals.length) return false;
    for (var i = 0, len = actuals.length; i < len; i++)
        if (!lisp_subtypep(actuals[i], formals[i]))
            return false;
    return true;
}

function lisp_most_specific_method_entry(generic, applicable_mes)
{
    if (applicable_mes.length === 1)
        return applicable_mes[0];
    for (var i = 0, len = applicable_mes.length; i < len; i++) 
        if (lisp_least_method_entry(applicable_mes[i], applicable_mes))
            return applicable_mes[i];
    return null;
}

function lisp_least_method_entry(me, mes)
{
    for (var i = 0, len = mes.length; i < len; i++) {
        if (me === mes[i])
            continue;
        if (!lisp_smaller_method_entry(me, mes[i]))
            return false;
    }
    return true;
}

function lisp_smaller_method_entry(me1, me2)
{
    if (me1.specializers.length != me2.specializers.length)
        return false;
    for (var i = 0, len = me1.specializers.length; i < len; i++)
        if ((!lisp_classes_comparable(me1.specializers[i],
                                      me2.specializers[i])) ||
            (!lisp_subtypep(me1.specializers[i],
                            me2.specializers[i])))
            return false;
    return true;
}

function lisp_classes_comparable(class1, class2)
{
    return ((lisp_subtypep(class1, class2)) ||
            (lisp_subtypep(class2, class1)))
}

function lisp_no_applicable_method(generic, arguments)
{
    lisp_error("No applicable method", generic);
}

function lisp_no_most_specific_method(generic, arguments, applicable_mes)
{
    lisp_error("No most specific method", generic);
}


/*** Classes ***/

function lisp_type_of(obj)
{
    if (obj === null)
        return lisp_nil_class;
    else
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
    if (type1 === type2) 
        return true;

    var supertype = type1.lisp_superclass;
    if (supertype)
        return lisp_subtypep(supertype, type2);
    
    return false;
}

function lisp_bif_subtypep(_key_, type1, type2)
{
    return lisp_subtypep(type1, type2);
}

function lisp_bif_make_class(_key_)
{
    return {};
}

function lisp_bif_set_superclass(_key_, clsA, clsB)
{
    clsA.lisp_superclass = clsB;
}

function lisp_bif_make_instance(_key_, cls)
{
    var obj = {};
    obj.__proto__ = cls;
    return obj;
}

function lisp_bif_is_typename(_key_, string)
{
    return lisp_is_type_name(string);
}


/*** Slots ***/

function lisp_bif_slot(_key_, obj, name)
{
    lisp_assert_string(name);
    return obj[lisp_mangle_slot(name)];
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


/*** Utilities ***/

function lisp_bif_macroexpand_1(_key_, form)
{
    var macro = lisp_macro_function(form.elts[0].name);
    if (macro)
        return macro(null, form);
    else
        return form;
}

function lisp_bif_print(_key_, object)
{
    lisp_print(object); // defined in REPL
}

function lisp_bif_eq(_key_, a, b)
{
    return a === b;
}

function lisp_bif_symbol_name(_key_, symbol)
{
    lisp_assert_symbol_form(symbol);
    return symbol.name;
}

function lisp_bif_symbolp(_key_, form)
{
    return form.formt === "symbol";
}

function lisp_bif_compoundp(_key_, form)
{
    return form.formt === "compound";
}

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

function lisp_bif_list_add(_key_, list, elt)
{
    list.push(elt);
    return list;
}

function lisp_bif_string_concat(_key_, s1, s2)
{
    return s1.concat(s2);
}

function lisp_bif_string_to_form(_key_, string)
{
    return new Lisp_string_form(string);
}

function lisp_bif_string_to_symbol(_key_, string)
{
    return new Lisp_symbol_form(string);
}

function lisp_bif_apply(_key_, fun, args, keys)
{
    return fun.apply(null, [ keys ].concat(args));
}

function lisp_is_true(obj) // T
{
    return (obj !== false) && (obj !== null);
}


/*** Export to Lisp ***/

lisp_set("#t", "true");
lisp_set("#f", "false");

lisp_set_class("object", "Object.prototype");
lisp_set_class("<boolean>", "Boolean.prototype");
lisp_set_class("<function>", "Function.prototype");
lisp_set_class("<compound-form>", "Lisp_compound_form.prototype");
lisp_set_class("<list>", "Array.prototype");
lisp_set_class("<number-form>", "Lisp_number_form.prototype");
lisp_set_class("<string-dict>", "Lisp_string_dict.prototype");
lisp_set_class("<string-form>", "Lisp_string_form.prototype");
lisp_set_class("<string>", "String.prototype");
lisp_set_class("<symbol-form>", "Lisp_symbol_form.prototype");

var lisp_nil_class = lisp_bif_make_class(null);
lisp_set_class("nil", "lisp_nil_class");
lisp_set("nil", "null");

// Numbers
lisp_set_class("small-integer", "Number.prototype");
lisp_set_class("big-integer", "jsnums.BigInteger.prototype")
lisp_set_class("real", "jsnums.FloatPoint.prototype");
lisp_set_class("rational", "jsnums.Rational.prototype");

lisp_set_function("append-compounds", "lisp_bif_append_compounds");
lisp_set_function("apply", "lisp_bif_apply");
lisp_set_function("bind-handlers", "lisp_bif_bind_handlers");
lisp_set_function("call-unwind-protected", "lisp_bif_call_unwind_protected");
lisp_set_function("call-while", "lisp_bif_call_while");
lisp_set_function("call-with-escape-function", "lisp_bif_call_with_escape_function");
lisp_set_function("compound-apply", "lisp_bif_compound_apply");
lisp_set_function("compound-elt", "lisp_bif_compound_elt");
lisp_set_function("compound-elts", "lisp_bif_compound_elts");
lisp_set_function("compound-empty?", "lisp_bif_compound_emptyp");
lisp_set_function("compound-len", "lisp_bif_compound_len");
lisp_set_function("compound-map", "lisp_bif_compound_map");
lisp_set_function("compound-slice", "lisp_bif_compound_slice");
lisp_set_function("compound?", "lisp_bif_compoundp");
lisp_set_function("eq", "lisp_bif_eq");
lisp_set_function("fast-apply", "lisp_bif_fast_apply");
lisp_set_function("find-method", "lisp_bif_find_method");
lisp_set_function("has-slot", "lisp_bif_has_slot");
lisp_set_function("list", "lisp_bif_list");
lisp_set_function("list-add", "lisp_bif_list_add");
lisp_set_function("list-elt", "lisp_bif_list_elt");
lisp_set_function("list-len", "lisp_bif_list_len");
lisp_set_function("macroexpand-1", "lisp_bif_macroexpand_1");
lisp_set_function("make-class", "lisp_bif_make_class");
lisp_set_function("make-compound", "lisp_bif_make_compound");
lisp_set_function("make-generic", "lisp_bif_make_generic");
lisp_set_function("make-instance", "lisp_bif_make_instance");
lisp_set_function("params-specializers", "lisp_bif_params_specializers");
lisp_set_function("print", "lisp_bif_print");
lisp_set_function("put-method", "lisp_bif_put_method");
lisp_set_function("set-slot", "lisp_bif_set_slot");
lisp_set_function("set-superclass", "lisp_bif_set_superclass");
lisp_set_function("signal", "lisp_bif_signal");
lisp_set_function("slot", "lisp_bif_slot");
lisp_set_function("string-concat", "lisp_bif_string_concat");
lisp_set_function("string-dict-get", "lisp_bif_string_dict_get");
lisp_set_function("string-dict-has-key", "lisp_bif_string_dict_has_key");
lisp_set_function("string-dict-put", "lisp_bif_string_dict_put");
lisp_set_function("string-to-form", "lisp_bif_string_to_form");
lisp_set_function("string-to-symbol", "lisp_bif_string_to_symbol");
lisp_set_function("subtype?", "lisp_bif_subtypep");
lisp_set_function("symbol-name", "lisp_bif_symbol_name");
lisp_set_function("symbol?", "lisp_bif_symbolp");
lisp_set_function("type-of", "lisp_bif_type_of");


/*** Utilities ***/

function lisp_set(lisp_name, js_object)
{
    eval(lisp_mangle_var(lisp_name) + " = " + js_object);
}

function lisp_set_function(lisp_name, js_function)
{
    eval(lisp_mangle_function(lisp_name) + " = " + js_function);
}

function lisp_set_class(lisp_name, js_class)
{
    eval(lisp_mangle_class(lisp_name) + " = " + js_class);
}
