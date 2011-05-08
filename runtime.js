/*  EdgeLisp: A Lisp that compiles to JavaScript.
    Copyright (C) 2008-2011 by Manuel Simoni.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, version 3.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Lisp runtime: this file contains all types and functions needed to
   run compiled Lisp code.

   Lisp code that uses `eval' will need to include the compiler, too. */

/* Nonstandard stuff of which I'd like to get rid of:
   __proto__, <function>.caller */

// Fast-loadable, contains compiled JS code for a Lisp unit.
function Lisp_fasl(times)
{
    // Maps time names ("execute", "compile") to compiled JS.
    this.times = times;
}

/* A compiler identifier (CID) is the fully explicit form of
   identifier used inside the compiler.

   The name is a string.

   The namespace is "variable", "function", "class" or another of the
   Lisp-Omega namespaces.
   
   The hygiene-context is null for user-entered identifiers, and a
   UUID string for macro-generated identifiers. */ 
function Lisp_cid(name, namespace, hygiene_context)
{
    lisp_assert_string(name, "Bad cid name", name);
    lisp_assert_string(namespace, "Bad cid namespace", namespace);
    this.name = name;
    this.namespace = namespace;
    this.hygiene_context = hygiene_context;
}

function lisp_identifier_to_cid(form, namespace)
{
    lisp_assert_identifier_form(form, "bad variable identifier", form);
    lisp_assert_string(namespace);
    return new Lisp_cid(form.name, namespace, form.hygiene_context);
}

/* Turns a plain identifier like #'foo, or a compound identifier like
   #'(%%identifier foo variable), or a macro like #'(function foo)
   into a CID. */
function lisp_generalized_identifier_to_cid(form, default_namespace)
{
    var ns = default_namespace ? default_namespace : "variable";
    switch(form.formt) {
    case "identifier":
        return new Lisp_cid(form.name, ns, form.hygiene_context);
    case "compound":
        var exp = lisp_macroexpand(form);
        lisp_assert_identifier_form(exp.elts[0]);
        lisp_assert(exp.elts[0].name, "%%identifier");
        var name = lisp_assert_identifier_form(exp.elts[1]);
        var namespace = lisp_assert_identifier_form(exp.elts[2]);
        return new Lisp_cid(name.name, namespace.name, name.hygiene_context);
    }
    lisp_error("Bad generalized identifier form", form);
}

function lisp_mangle_cid(cid)
{
    return lisp_mangle(cid.name, cid.namespace, cid.hygiene_context);
}

/* A set mapping mangled CIDs to CIDs. */
var lisp_globals = {};

function lisp_define_global(cid)
{
    lisp_globals[lisp_mangle_cid(cid)] = cid;
}

function lisp_global_defined(cid)
{
    return lisp_globals[lisp_mangle_cid(cid)] !== undefined;
}

function lisp_apropos(str)
{
    var results = [];
    lisp_iter_dict(lisp_globals, function(k) {
            var cid = lisp_globals[k];
            if (cid.name.indexOf(str) !== -1)
                results.push(cid.name);
        });
    return results;
}

function lisp_bif_apropos(_key_, str)
{
    return lisp_apropos(str);
}

/*** Literal ***/

function Lisp_literal()
{
}

/*** Nil ***/

function Lisp_nil()
{
}

/*** Numbers ***/

function lisp_number(numrepr)
{
    return jsnums.fromString(numrepr);
}

function lisp_bif_string_to_number(_key_, s)
{
    return lisp_number(s);
}

function Lisp_number()
{
}

function Lisp_integer()
{
}

/*** Functions ***/

function lisp_bif_fast_apply(_key_, fun, _arguments)
{
    return fun.apply(null, _arguments);
}


/*** Forms ***/

function Lisp_form()
{
}

function Lisp_number_form(sign, integral_digits, fractional_digits)
{
    this.formt = "number";
    this.sign = sign;
    this.integral_digits = integral_digits;
    this.fractional_digits = fractional_digits;
}

Lisp_number_form.prototype.toString = function() {
    var sign_str = this.sign === "+" ? "" : "-";
    return sign_str + this.integral_digits + this.fractional_digits;
}

function Lisp_string_form(s)
{
    this.formt = "string";
    this.s = s;
}

Lisp_string_form.prototype.toString = function() {
    return "\"" + this.s + "\"";
}

function Lisp_identifier_form(name, hygiene_context)
{
    this.formt = "identifier";
    this.name = name;
    this.hygiene_context = hygiene_context;
}

Lisp_identifier_form.prototype.toString = function() {
    return this.name + (this.hygiene_context ? "\\" + this.hygiene_context : "");
}

function Lisp_compound_form(elts)
{
    this.formt = "compound";
    this.elts = elts;
}

Lisp_compound_form.prototype.toString = function() {
    return "(" + this.elts.map(function(elt){return elt.toString();}).join(" ") + ")";
}

/* This whole comments-as-forms business is silly, but atm I don't
   know another way to have comments ignored. */
function Lisp_comment_form(contents)
{
    this.formt = "comment";
    this.contents = contents;
}


/*** Utilities for generated code. ***/

function lisp_undefined_identifier(name, namespace, hygiene_context)
{
    return _lisp_function_undefined_identifier(null, name, namespace,
                                               (hygiene_context ? hygiene_context : null));
}

function _lisp_function_undefined_identifier(_key_, name, namespace, hygiene_context)
{
    return lisp_error("Undefined " + namespace, name +
                      (hygiene_context ? ("\\"+hygiene_context) : ""));
}

/* Used inside lambdas. */
function lisp_arity_min(length, min)
{
    if (length < min)
        return lisp_error("Too few arguments", lisp_arity_min.caller);
}

function lisp_arity_min_max(length, min, max)
{
    if (length < min)
        return lisp_error("Too few arguments", lisp_arity_min_max.caller);
    if (length > max)
        return lisp_error("Too many arguments", lisp_arity_min_max.caller);
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
        return lisp_error("Type error", [obj, lisp_show(type)]);
}

/* This later replaced with generic function in boot.lisp. */
function lisp_show(obj)
{
    return _lisp_function_show(null, obj);
}

function _lisp_function_show(_key_, obj)
{
    // Needs to be safe
    //    return String(JSON.stringify(obj));
    return String(obj);
}

function lisp_array_contains(array, elt)
{
    for (var i = 0; i < array.length; i++) {
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

function Lisp_error()
{
}

function Lisp_runtime_error(message, arg)
{
    this.message = message;
    this.arg = arg;
}

function lisp_bif_runtime_error_message(_key_, runtime_error)
{
    return runtime_error.message;
}

function lisp_bif_runtime_error_arg(_key_, runtime_error)
{
    return runtime_error.arg;
}

function lisp_error(message, arg)
{
    return lisp_signal(new Lisp_runtime_error(message, arg));
}

function lisp_signal(condition)
{
    // careful, maybe Lisp SIGNAL isn't defined yet
    if (typeof _lisp_function_signal !== "undefined")
        return _lisp_function_signal(null, condition);
    else
        throw condition;
}

function lisp_assert(value, message, arg)
{
    if (!value) return lisp_error(message, arg);
    return value;
}

function lisp_assert_not_null(value, message, arg)
{
    lisp_assert(value != null, message, arg);
    return value;
}

function lisp_assert_number(value, message, arg)
{
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

function lisp_assert_list(value, message, arg)
{
    lisp_assert(value.length !== undefined, message, arg);
    return value;
}

function lisp_assert_identifier_form(value, message, arg)
{
    lisp_assert(typeof value === "object", message, arg);
    lisp_assert(value.formt === "identifier", message, arg);
    lisp_assert_nonempty_string(value.name, message, arg);
    return value;
}

function lisp_assert_string_form(value, message, arg)
{
    lisp_assert(typeof value === "object", message, arg);
    lisp_assert(value.formt === "string", message, arg);
    lisp_assert_string(value.s, message, arg);
    return value;
}

function lisp_assert_number_form(value, message, arg)
{
    lisp_assert(typeof value === "object", message, arg);
    lisp_assert(value.formt === "number", message, arg);
    lisp_assert_string(value.sign, message, arg);
    lisp_assert_string(value.integral_digits, message, arg);
    lisp_assert_string(value.fractional_digits, message, arg);
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

/* Adds form at end of compound form. */
function lisp_bif_compound_add(_key_, compound, form)
{
    compound.elts.push(form);
    return compound;
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
    lisp_assert(i < compound.elts.length, "index too large", [compound, i]);
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

function lisp_bif_compound_slice(_key_, compound, start, end)
{
    lisp_assert_compound_form(compound, "compound-slice", compound);
    return new Lisp_compound_form(compound.elts.slice(start, end));
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
    lisp_assert_string(name, "bad string dict key", name);
    return "%" + name;
}

function lisp_unmangle_string_dict_key(name)
{
    return name.slice(1);
}

function lisp_is_string_dict_key(k)
{
    return k[0] === "%";
}

function lisp_bif_string_dict_map(_key_, fun, dict)
{
    lisp_iter_dict(dict, function(k) {
            fun(null, lisp_unmangle_string_dict_key(k));
        });
    return null;
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
    var val = dict[lisp_mangle_string_dict_key(key)];
    return val !== undefined ? val : "null";
}

function lisp_bif_string_dict_put(_key_, dict, key, value)
{
    dict[lisp_mangle_string_dict_key(key)] = value;
    return value;
}

function lisp_bif_string_dict_has_key(_key_, dict, key)
{
    return lisp_mangle_string_dict_key(key) in dict;
}


/*** Control flow ***/

function lisp_bif_call_with_catch_tag(_key_, tag, body_fun)
{
    try {
        return body_fun(null);
    } catch(obj) {
        if (obj && (obj.lisp_tag === tag))
            return obj.lisp_value;
        else
            throw obj;
    }
}

function lisp_bif_throw(_key_, tag, value)
{
    throw { lisp_tag: tag, lisp_value: (value !== undefined ? value : null) };
}

function lisp_bif_call_unwind_protected(_key_, protected_fun, cleanup_fun)
{
    try {
        return protected_fun(null);
    } finally {
        cleanup_fun(null);
    }
}

function lisp_bif_call_forever(_key_, body_fun)
{
    while(true)
        body_fun(null);
}


/*** Multiple Dispatch ***/

function Lisp_generic(name)
{
    this.method_entries = [];
    this.name = name;
}

function lisp_generic_name(generic)
{
    return generic.name ? generic.name : "anonymous generic";
}

function lisp_bif_generic_name(_key_, generic)
{
    return lisp_generic_name(generic);
}

function Lisp_method_entry(method, specializers)
{
    this.method = method;
    this.specializers = specializers;
}

function lisp_bif_make_generic(_key_, name)
{
    return new Lisp_generic(name);
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
            return null;
        }
    }
    generic.method_entries.push(lisp_make_method_entry(method, specializers));
    return null;
}

function lisp_bif_params_specializers(_key_, params)
{
    if (!lisp_compile_sig)
        lisp_error("Compiler not loaded");

    var specializers = [];
    // This empty compilation state is a HACK, but it doesn't matter,
    // as we're only interested in the specializers, which are a
    // purely syntactical matter.
    var st = new Lisp_compilation_state();
    var sig = lisp_compile_sig(st, params.elts);
    for (var i = 0, len = sig.req_params.length; i < len; i++) {
        var param = sig.req_params[i];
        var specializer = param.specializer ? param.specializer : "object";
        var specs = [ new Lisp_identifier_form("%%identifier"),
                      new Lisp_identifier_form(specializer, param.hygiene_context),
                      new Lisp_identifier_form("class") ];
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
        if (lisp_specializer_lists_agree(actual_specializers,
                                         mes[i].specializers)) {
            applicable_mes.push(mes[i]);
        }
    }
    return applicable_mes;
}

function lisp_specializer_lists_agree(actuals, formals)
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
    return _lisp_function_no_applicable_method(null, generic, arguments);
}

function _lisp_function_no_applicable_method(_key_, generic, arguments)
{
    return lisp_error("No applicable method", lisp_generic_name(generic));
}

function lisp_no_most_specific_method(generic, arguments, applicable_mes)
{
    return _lisp_function_no_most_specific_method(null, generic, arguments);
}

function _lisp_function_no_most_specific_method(_key_, generic, arguments)
{
    return lisp_error("No most specific method", lisp_generic_name(generic));
}

/*** Classes ***/

function lisp_bif_class_name(_key_, klass)
{
    return klass.lisp_name ? klass.lisp_name : "anonymous class";
}

function lisp_bif_set_class_name(_key_, klass, name)
{
    klass.lisp_name = name;
    return name;
}

function Lisp_native()
{
}

Lisp_native.prototype.toString = function()
{
    return "JS object";
}

function lisp_type_of(obj)
{
    if (obj === undefined)
        return lisp_error("segfault");
    else if (obj === null)
        return Lisp_nil.prototype;
    else if (obj.hasOwnProperty("lisp_is_class"))
        return Lisp_class.prototype;
    else {
        var proto = obj.__proto__;
        if (proto.hasOwnProperty("lisp_is_class"))
            return proto;
        else
            return Lisp_native.prototype;
    }
}

function lisp_bif_type_of(_key_, obj) 
{
    return lisp_type_of(obj);
}

function lisp_bif_the(_key_, klass, object)
{
    if (lisp_subtypep(lisp_type_of(object), klass)) {
        return object;
    } else {
        lisp_error(lisp_show(object) + " is not of expected type", lisp_show(klass));
    }
}

function lisp_show_type_sensibly(type)
{
    if (type === null) {
        return "#![null-type (error)]";
    } else if (type.lisp_name) {
        return "#![" + type.lisp_name + "]";
    } else {
        return JSON.stringify(type);
    }
}

/* Returns true iff type1 is a general subtype of type2, meaning
   either equal to type2, or a subtype of type2. */
function lisp_subtypep(type1, type2)
{
    if ((!type1) || (!type2))
        lisp_error("subtype?", [lisp_show_type_sensibly(type1),
                                lisp_show_type_sensibly(type2)]);

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

function Lisp_class()
{
    /* Classes set this property, which identifies them to TYPE-OF as
       such.  The rationale is that we don't want to go ahead and muck
       with the prototype structure of non-Lisp, built-in classes.
       Lest they completely fall apart. */
    this.lisp_is_class = true;
}

function lisp_bif_make_class(_key_)
{
    return new Lisp_class();
}

function lisp_bif_set_superclass(_key_, clsA, clsB)
{
    return lisp_set_superclass(clsA, clsB);
}

function lisp_bif_superclass(_key_, clsA)
{
    return lisp_superclass(clsA);
}

function lisp_superclass(clsA)
{
    return clsA.lisp_superclass ? clsA.lisp_superclass : lisp_error("no superclass", clsA);
}

function lisp_set_superclass(clsA, clsB)
{
    clsA.lisp_superclass = clsB;
    return clsB;
}

function lisp_bif_make_instance(_key_, cls)
{
    var obj = new Object();
    obj.__proto__ = cls;
    return obj;
}


/*** Slots ***/

function lisp_bif_slot_value(_key_, obj, name)
{
    lisp_assert_string(name);
    var value = obj[lisp_mangle_slot(name)];
    return value !== undefined ? value : null;
}

function lisp_bif_set_slot_value(_key_, obj, name, value)
{
    lisp_assert_string(name);
    obj[lisp_mangle_slot(name)] = value;
    return value;
}

function lisp_bif_has_slot(_key_, obj, name)
{
    lisp_assert_string(name);
    return obj.hasOwnProperty(lisp_mangle_slot(name));
}


/*** Utilities ***/

/* Maps the mangled CIDs of macros to their expander functions.
 
   This is defined, but unused in the runtime, so that users calling
   macroexpand don't get an error. */
var lisp_macros_table = {};

function lisp_macro_function(name)
{
    var name = lisp_assert_nonempty_string(name, "Bad macro name", name);
    var mangled_name = lisp_mangle_function(name);
    return lisp_macros_table[mangled_name];
}

function lisp_set_macro_function(name, expander)
{
    var name = lisp_assert_nonempty_string(name, "Bad macro name", name);
    var mangled_name = lisp_mangle_function(name);    
    lisp_macros_table[mangled_name] = expander;
    return expander;
}

function lisp_bif_set_macro_function(_key_, name, fun)
{
    return lisp_set_macro_function(name, fun);
}

function lisp_macroexpand(form)
{
    var exp = lisp_macroexpand_1(form);
    return exp;
}

function lisp_macroexpand_1(form)
{
    var macro = lisp_macro_function(form.elts[0].name);
    if (macro) {
        return macro(null, form);
    } else {
        return form;
    }
}

function lisp_bif_macroexpand(_key_, form)
{
    return lisp_macroexpand(form);
}

function lisp_bif_macroexpand_1(_key_, form)
{
    return lisp_macroexpand_1(form);
}

function lisp_bif_read_from_string(_key_, string)
{
    return lisp_read_from_string(string);
}

function lisp_bif_eq(_key_, a, b)
{
    return a === b;
}

function lisp_bif_eval(_key_, form)
{
    return lisp_eval(form);
}

function lisp_bif_identifier_name(_key_, identifier)
{
    lisp_assert_identifier_form(identifier);
    return identifier.name;
}

function lisp_bif_identifierp(_key_, form)
{
    return form.formt === "identifier";
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
    lisp_assert(i < list.length, "index too large", [list, i]);
    return list[i];
}

function lisp_bif_list_emptyp(_key_, list)
{
    return list.length === 0;
}

function lisp_bif_list_len(_key_, list, i)
{
    return list.length;
}

function lisp_bif_list_slice(_key_, list, start, end)
{
    return list.slice(start, end);
}

function lisp_bif_list_add(_key_, list, elt)
{
    list.push(elt);
    return list;
}

function lisp_bif_string_concat(_key_)
{
    var s = "";
    for (var i = 1; i < arguments.length; i++)
        s = s.concat(arguments[i]);
    return s;
}

function lisp_bif_string_len(_key_, string)
{
    return string.length;
}

function lisp_bif_string_to_form(_key_, string)
{
    return new Lisp_string_form(string);
}

function lisp_bif_string_to_identifier(_key_, string)
{
    return new Lisp_identifier_form(string);
}

function lisp_bif_apply(_key_, fun, args, keys)
{
    return fun.apply(null, [ keys ].concat(args));
}

function lisp_is_true(obj) // T
{
    return (obj !== false) && (obj !== null);
}



/*** Name Mangling ***/

/* Lisp identifiers may contain additional characters beyond those
   supported by JavaScript names.  These special characters are
   translated to uppercase characters, which are not allowed in
   EdgeLisp identifiers. */

// Needs to be in sync with `lisp_identifier_special_char'.
var lisp_mangle_table =
    [
     ["-", "_"],
     ["&", "A"],
     [":", "C"],
     [".", "D"],
     ["=", "E"],
     [">", "G"],
     ["<", "L"],
     ["%", "N"],
     ["+", "P"],
     ["?", "Q"],
     ["/", "S"],
     ["*", "T"],
     ["#", "O"],
     ];

function lisp_do_mangle(string)
{
    var res = string;
    for (var i = 0, len = lisp_mangle_table.length; i < len; i++) {
        var pair = lisp_mangle_table[i];
        var pattern = new RegExp("\\" + pair[0], "g");
        res = res.replace(pattern, pair[1]);
    }
    return res;
}

function lisp_mangle(name, namespace, hygiene_context)
{
    lisp_assert_nonempty_string(name, "Bad name", name);
    lisp_assert_nonempty_string(namespace, "Bad namespace", namespace);
    return "_lisp_" + lisp_do_mangle(namespace) + "_" + lisp_do_mangle(name) +
        (hygiene_context ? ("_" + lisp_do_mangle(hygiene_context)) : "");
}

/* Additionally, the different namespaces (variable, function, slot,
   method) are prefixed, so they can coexist in their respective
   JavaScript namespace(s). */

// BUG: unhygienic

function lisp_mangle_var(name)
{
    return lisp_mangle(name, "variable");
}

function lisp_mangle_function(name)
{
    return lisp_mangle(name, "function");
}

function lisp_mangle_class(name)
{
    return lisp_mangle(name, "class");
}

function lisp_mangle_slot(name)
{
    return lisp_mangle(name, "slot");
}

function lisp_mangle_method(name)
{
    return lisp_mangle(name, "method");
}

/*** UUID ***/

function lisp_bif_make_uuid(_key_)
{
    return lisp_make_uuid();
}

function lisp_make_uuid()
{
    return uuid();
}

/*** Compilation ***/

function lisp_bif_compile(_key_, form)
{
    return lisp_compile_unit(form);
}

/*** Export to Lisp ***/

lisp_export("#t", "true");
lisp_export("#f", "false");
lisp_export("nil", "null");

lisp_export_class("big-integer", "jsnums.BigInteger.prototype")
lisp_export_class("boolean", "Boolean.prototype");
lisp_export_class("cid", "Lisp_cid.prototype");
lisp_export_class("class", "Lisp_class.prototype");
lisp_export_class("compound-form", "Lisp_compound_form.prototype");
lisp_export_class("error", "Error.prototype");
lisp_export_class("fasl", "Lisp_fasl.prototype");
lisp_export_class("form", "Lisp_form.prototype");
lisp_export_class("function", "Function.prototype");
lisp_export_class("generic", "Lisp_generic.prototype");
lisp_export_class("list", "Array.prototype");
lisp_export_class("literal", "Lisp_literal.prototype");
lisp_export_class("nil", "Lisp_nil.prototype");
lisp_export_class("native", "Lisp_native.prototype");
lisp_export_class("number-form", "Lisp_number_form.prototype");
lisp_export_class("number", "Lisp_number.prototype");
lisp_export_class("integer", "Lisp_integer.prototype");
lisp_export_class("object", "Object.prototype");
lisp_export_class("rational", "jsnums.Rational.prototype");
lisp_export_class("real", "jsnums.FloatPoint.prototype");
lisp_export_class("small-integer", "Number.prototype");
lisp_export_class("runtime-error", "Lisp_runtime_error.prototype");
lisp_export_class("string", "String.prototype");
lisp_export_class("string-dict", "Lisp_string_dict.prototype");
lisp_export_class("string-form", "Lisp_string_form.prototype");
lisp_export_class("identifier-form", "Lisp_identifier_form.prototype");

lisp_export_set_superclass("big-integer", "integer");
lisp_export_set_superclass("boolean", "literal");
lisp_export_set_superclass("cid", "object");
lisp_export_set_superclass("class", "object");
lisp_export_set_superclass("compound-form", "form");
lisp_export_set_superclass("error", "object");
lisp_export_set_superclass("fasl", "object");
lisp_export_set_superclass("form", "object");
lisp_export_set_superclass("function", "object");
lisp_export_set_superclass("generic", "object");
lisp_export_set_superclass("integer", "rational");
lisp_export_set_superclass("list", "object");
lisp_export_set_superclass("literal", "object");
lisp_export_set_superclass("native", "object");
lisp_export_set_superclass("nil", "literal");
lisp_export_set_superclass("number", "literal");
lisp_export_set_superclass("number-form", "form");
lisp_export_set_superclass("rational", "real");
lisp_export_set_superclass("real", "number");
lisp_export_set_superclass("runtime-error", "error");
lisp_export_set_superclass("small-integer", "integer");
lisp_export_set_superclass("string", "literal");
lisp_export_set_superclass("string-dict", "object");
lisp_export_set_superclass("string-form", "form");
lisp_export_set_superclass("identifier-form", "form");

lisp_export_function("%append-compounds", "lisp_bif_append_compounds");
lisp_export_function("%apply", "lisp_bif_apply");
lisp_export_function("%apropos", "lisp_bif_apropos");
lisp_export_function("%call-unwind-protected", "lisp_bif_call_unwind_protected");
lisp_export_function("%call-with-catch-tag", "lisp_bif_call_with_catch_tag");
lisp_export_function("%call-forever", "lisp_bif_call_forever");
lisp_export_function("%class-name", "lisp_bif_class_name");
lisp_export_function("%compile", "lisp_bif_compile");
lisp_export_function("%compound-apply", "lisp_bif_compound_apply");
lisp_export_function("%compound-add", "lisp_bif_compound_add");
lisp_export_function("%compound-elt", "lisp_bif_compound_elt");
lisp_export_function("%compound-elts", "lisp_bif_compound_elts");
lisp_export_function("%compound-empty?", "lisp_bif_compound_emptyp");
lisp_export_function("%compound-len", "lisp_bif_compound_len");
lisp_export_function("%compound-map", "lisp_bif_compound_map");
lisp_export_function("%compound-slice", "lisp_bif_compound_slice");
lisp_export_function("%compound?", "lisp_bif_compoundp");
lisp_export_function("%eq", "lisp_bif_eq");
lisp_export_function("%eval", "lisp_bif_eval");
lisp_export_function("%fast-apply", "lisp_bif_fast_apply");
lisp_export_function("%find-method", "lisp_bif_find_method");
lisp_export_function("%generic-name", "lisp_bif_generic_name");
lisp_export_function("%has-slot", "lisp_bif_has_slot");
lisp_export_function("%list", "lisp_bif_list");
lisp_export_function("%list-add", "lisp_bif_list_add");
lisp_export_function("%list-elt", "lisp_bif_list_elt");
lisp_export_function("%list-empty?", "lisp_bif_list_emptyp");
lisp_export_function("%list-len", "lisp_bif_list_len");
lisp_export_function("%list-slice", "lisp_bif_list_slice");
lisp_export_function("%macroexpand-1", "lisp_bif_macroexpand_1");
lisp_export_function("%macroexpand", "lisp_bif_macroexpand");
lisp_export_function("%make-class", "lisp_bif_make_class");
lisp_export_function("%make-compound", "lisp_bif_make_compound");
lisp_export_function("%make-generic", "lisp_bif_make_generic");
lisp_export_function("%make-instance", "lisp_bif_make_instance");
lisp_export_function("%make-uuid", "lisp_bif_make_uuid");
lisp_export_function("%params-specializers", "lisp_bif_params_specializers");
lisp_export_function("%put-method", "lisp_bif_put_method");
lisp_export_function("%read-from-string", "lisp_bif_read_from_string");
lisp_export_function("%set-class-name", "lisp_bif_set_class_name");
lisp_export_function("%set-macro-function", "lisp_bif_set_macro_function");
lisp_export_function("%set-slot-value", "lisp_bif_set_slot_value");
lisp_export_function("%set-superclass", "lisp_bif_set_superclass");
lisp_export_function("%runtime-error-message", "lisp_bif_runtime_error_message");
lisp_export_function("%runtime-error-arg", "lisp_bif_runtime_error_arg");
lisp_export_function("%slot-value", "lisp_bif_slot_value");
lisp_export_function("%string-concat", "lisp_bif_string_concat");
lisp_export_function("%string-dict-get", "lisp_bif_string_dict_get");
lisp_export_function("%string-dict-has-key", "lisp_bif_string_dict_has_key");
lisp_export_function("%string-dict-map", "lisp_bif_string_dict_map");
lisp_export_function("%string-dict-put", "lisp_bif_string_dict_put");
lisp_export_function("%string-len", "lisp_bif_string_len");
lisp_export_function("%string-to-form", "lisp_bif_string_to_form");
lisp_export_function("%string-to-number", "lisp_bif_string_to_number");
lisp_export_function("%string-to-identifier", "lisp_bif_string_to_identifier");
lisp_export_function("%subtype?", "lisp_bif_subtypep");
lisp_export_function("%superclass", "lisp_bif_superclass");
lisp_export_function("%identifier-name", "lisp_bif_identifier_name");
lisp_export_function("%identifier?", "lisp_bif_identifierp");
lisp_export_function("%the", "lisp_bif_the");
lisp_export_function("%throw", "lisp_bif_throw");
lisp_export_function("%type-of", "lisp_bif_type_of");

// Additional interface functions
// - show 
// - signal
// - no-applicable-method 
// - no-most-specific-method
// - undefined-identifier

function lisp_export(lisp_name, js_object)
{
    lisp_define_global(new Lisp_cid(lisp_name, "variable"));
    eval(lisp_mangle_var(lisp_name) + " = " + js_object);
}

function lisp_export_function(lisp_name, js_function)
{
    lisp_define_global(new Lisp_cid(lisp_name, "function"));
    eval(lisp_mangle_function(lisp_name) + " = " + js_function);
}

function lisp_export_class(lisp_name, js_class)
{
    lisp_define_global(new Lisp_cid(lisp_name, "class"));
    var class_name = lisp_mangle_class(lisp_name);
    eval("(" + class_name + " = " + js_class + ", " +
         class_name + ".lisp_name = \"" + lisp_name + "\", " + 
         class_name + ".lisp_is_class = true)");
}

function lisp_export_set_superclass(class_name, super_name)
{
    var mclass = lisp_mangle_class(class_name);
    var msuper = lisp_mangle_class(super_name);
    eval("lisp_set_superclass(" + mclass + ", " + msuper + ")");
}