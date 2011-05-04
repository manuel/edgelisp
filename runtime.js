/* EdgeLisp: A Lisp that compiles to JavaScript 1.5.
   
   Copyright (C) 2008, 2011 by Manuel Simoni.
   
   EdgeLisp is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.
   
   EdgeLisp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA. */

/* Lisp runtime: this file contains all types and functions needed to
   run compiled Lisp code.

   Lisp code that uses `eval' will need to include the compiler, too. */

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
    return this.sign + this.integral_digits + this.fractional_digits;
}

function Lisp_string_form(s)
{
    this.formt = "string";
    this.s = s;
}

Lisp_string_form.prototype.toString = function() {
    return "\"" + this.s + "\"";
}

function Lisp_symbol_form(name)
{
    this.formt = "symbol";
    this.name = name;
}

Lisp_symbol_form.prototype.toString = function() {
    return this.name;
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

function lisp_undefined_identifier(name, namespace)
{
    return _lisp_function_undefined_identifier(null, name, namespace);
}

function _lisp_function_undefined_identifier(_key_, name, namespace)
{
    return lisp_error("Undefined " + namespace, name);
}

/* Used inside lambdas. */
function lisp_arity_min(length, min)
{
    if (length < min)
        return lisp_error("Too few arguments ");
}

function lisp_arity_min_max(length, min, max)
{
    lisp_arity_min(length, min);
    if (length > max)
        return lisp_error("Too many arguments ");
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

function Lisp_error()
{
}

function Lisp_simple_error(message, arg)
{
    this.message = message;
    this.arg = arg;
}

function lisp_bif_simple_error_message(_key_, simple_error)
{
    return simple_error.message;
}

function lisp_bif_simple_error_arg(_key_, simple_error)
{
    return simple_error.arg;
}

function lisp_error(message, arg)
{
    return lisp_signal(new Lisp_simple_error(message, arg));
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

function lisp_assert_symbol_form(value, message, arg)
{
    lisp_assert(typeof value === "object", message, arg);
    lisp_assert(value.formt === "symbol", message, arg);
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
    throw { lisp_tag: tag, lisp_value: (value ? value : null) };
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
}

function lisp_type_of(obj)
{
    if (obj === undefined)
        return lisp_error("that can never happen");
    else if (obj === null)
        return Lisp_nil.prototype;
    else if (obj.hasOwnProperty("lisp_is_class"))
        return Lisp_class.prototype;
    else
        return obj.__proto__;
}

function lisp_bif_type_of(_key_, obj) 
{
    return lisp_type_of(obj);
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
    return null;
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
    return null;
}

function lisp_bif_eq(_key_, a, b)
{
    return a === b;
}

function lisp_bif_eval(_key_, form)
{
    return lisp_eval(form);
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



/*** Name Mangling ***/

/* Lisp symbols may contain additional characters beyond those
   supported by JavaScript names.  These special characters are
   translated to uppercase characters, which are not allowed in
   EdgeLisp symbols. */

// Needs to be in sync with `lisp_symbol_special_char'.
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

function lisp_mangle(name, namespace)
{
    lisp_assert_nonempty_string(name, "Bad name", name);
    lisp_assert_nonempty_string(namespace, "Bad namespace", namespace);
    for (var i = 0, len = lisp_mangle_table.length; i < len; i++) {
        var pair = lisp_mangle_table[i];
        var pattern = new RegExp("\\" + pair[0], "g");
        name = name.replace(pattern, pair[1]);
    }
    return "_lisp_" + namespace + "_" + name;
}

/* Additionally, the different namespaces (variable, function, slot,
   method) are prefixed, so they can coexist in their respective
   JavaScript namespace(s). */

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

/*** Export to Lisp ***/

lisp_export("#t", "true");
lisp_export("#f", "false");
lisp_export("nil", "null");

lisp_export_class("big-integer", "jsnums.BigInteger.prototype")
lisp_export_class("boolean", "Boolean.prototype");
lisp_export_class("class", "Lisp_class.prototype");
lisp_export_class("compound-form", "Lisp_compound_form.prototype");
lisp_export_class("error", "Error.prototype");
lisp_export_class("form", "Lisp_form.prototype");
lisp_export_class("function", "Function.prototype");
lisp_export_class("generic", "Lisp_generic.prototype");
lisp_export_class("list", "Array.prototype");
lisp_export_class("literal", "Lisp_literal.prototype");
lisp_export_class("nil", "Lisp_nil.prototype");
lisp_export_class("number-form", "Lisp_number_form.prototype");
lisp_export_class("number", "Lisp_number.prototype");
lisp_export_class("integer", "Lisp_integer.prototype");
lisp_export_class("object", "Object.prototype");
lisp_export_class("rational", "jsnums.Rational.prototype");
lisp_export_class("real", "jsnums.FloatPoint.prototype");
lisp_export_class("small-integer", "Number.prototype");
lisp_export_class("simple-error", "Lisp_simple_error.prototype");
lisp_export_class("string", "String.prototype");
lisp_export_class("string-dict", "Lisp_string_dict.prototype");
lisp_export_class("string-form", "Lisp_string_form.prototype");
lisp_export_class("symbol-form", "Lisp_symbol_form.prototype");

lisp_export_function("append-compounds", "lisp_bif_append_compounds");
lisp_export_function("apply", "lisp_bif_apply");
lisp_export_function("call-unwind-protected", "lisp_bif_call_unwind_protected");
lisp_export_function("call-with-catch-tag", "lisp_bif_call_with_catch_tag");
lisp_export_function("call-forever", "lisp_bif_call_forever");
lisp_export_function("class-name", "lisp_bif_class_name");
lisp_export_function("compound-apply", "lisp_bif_compound_apply");
lisp_export_function("compound-add", "lisp_bif_compound_add");
lisp_export_function("compound-elt", "lisp_bif_compound_elt");
lisp_export_function("compound-elts", "lisp_bif_compound_elts");
lisp_export_function("compound-empty?", "lisp_bif_compound_emptyp");
lisp_export_function("compound-len", "lisp_bif_compound_len");
lisp_export_function("compound-map", "lisp_bif_compound_map");
lisp_export_function("compound-slice", "lisp_bif_compound_slice");
lisp_export_function("compound?", "lisp_bif_compoundp");
lisp_export_function("eq", "lisp_bif_eq");
lisp_export_function("eval", "lisp_bif_eval");
lisp_export_function("fast-apply", "lisp_bif_fast_apply");
lisp_export_function("find-method", "lisp_bif_find_method");
lisp_export_function("has-slot", "lisp_bif_has_slot");
lisp_export_function("list", "lisp_bif_list");
lisp_export_function("list-add", "lisp_bif_list_add");
lisp_export_function("list-elt", "lisp_bif_list_elt");
lisp_export_function("list-empty?", "lisp_bif_list_emptyp");
lisp_export_function("list-len", "lisp_bif_list_len");
lisp_export_function("list-slice", "lisp_bif_list_slice");
lisp_export_function("macroexpand-1", "lisp_bif_macroexpand_1");
lisp_export_function("make-class", "lisp_bif_make_class");
lisp_export_function("make-compound", "lisp_bif_make_compound");
lisp_export_function("make-generic", "lisp_bif_make_generic");
lisp_export_function("make-instance", "lisp_bif_make_instance");
lisp_export_function("params-specializers", "lisp_bif_params_specializers");
lisp_export_function("print", "lisp_bif_print");
lisp_export_function("put-method", "lisp_bif_put_method");
lisp_export_function("set-class-name", "lisp_bif_set_class_name");
lisp_export_function("set-slot-value", "lisp_bif_set_slot_value");
lisp_export_function("set-superclass", "lisp_bif_set_superclass");
lisp_export_function("simple-error-message", "lisp_bif_simple_error_message");
lisp_export_function("simple-error-arg", "lisp_bif_simple_error_arg");
lisp_export_function("slot-value", "lisp_bif_slot_value");
lisp_export_function("string-concat", "lisp_bif_string_concat");
lisp_export_function("string-dict-get", "lisp_bif_string_dict_get");
lisp_export_function("string-dict-has-key", "lisp_bif_string_dict_has_key");
lisp_export_function("string-dict-put", "lisp_bif_string_dict_put");
lisp_export_function("string-to-form", "lisp_bif_string_to_form");
lisp_export_function("string-to-number", "lisp_bif_string_to_number");
lisp_export_function("string-to-symbol", "lisp_bif_string_to_symbol");
lisp_export_function("subtype?", "lisp_bif_subtypep");
lisp_export_function("superclass", "lisp_bif_superclass");
lisp_export_function("symbol-name", "lisp_bif_symbol_name");
lisp_export_function("symbol?", "lisp_bif_symbolp");
lisp_export_function("throw", "lisp_bif_throw");
lisp_export_function("type-of", "lisp_bif_type_of");

// Additional functions:
// - signal
// - no-applicable-method
// - no-most-specific-method
// - undefined-identifier

/*** Utilities ***/

function lisp_export(lisp_name, js_object)
{
    eval(lisp_mangle_var(lisp_name) + " = " + js_object);
}

function lisp_export_function(lisp_name, js_function)
{
    eval(lisp_mangle_function(lisp_name) + " = " + js_function);
}

function lisp_export_class(lisp_name, js_class)
{
    var class_name = lisp_mangle_class(lisp_name);
    eval("(" + class_name + " = " + js_class + ", " +
         class_name + ".lisp_name = \"" + lisp_name + "\", " + 
         class_name + ".lisp_is_class = true)");
}
