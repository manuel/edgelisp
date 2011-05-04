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

/*** Compilation and Evaluation ***/

function lisp_eval(form)
{
    return eval(lisp_emit(lisp_compile(form)));
}

/* The usual Lisp evaluation rule: literals evaluate to themselves;
   symbols evaluate to the value of the variable binding they name.  A
   compound form is evaluated differently depending on whether its
   operator (first element) names a special form, a macro, or a
   function: special form calls are evaluated with built-in evaluation
   rules; macro calls are first expanded and then compiled
   recursively; function calls are evaluated by applying the named
   function to the supplied arguments. */

/* A note about identifiers in EdgeLisp:
   
   EdgeLisp is a Lisp-Omega, meaning it supports the principled use of
   an unlimited number of namespaces.

   EdgeLisp is a Lisp-2 insofar as identifiers in the operator
   position of a call form are looked in the function namespace, not
   the ordinary variable namespace.

   EdgeLisp uses additional namespaces for other types of objects,
   such as classes and generics.

   See the `%%identifier' form. */

function lisp_compile(form)
{
    switch(form.formt) {
    case "number":
        return { vopt: "number",
                 sign: form.sign,
                 integral_digits: form.integral_digits,
                 fractional_digits: form.fractional_digits };
    case "string":
        lisp_assert_string(form.s, "Bad .s", form);
        return { vopt: "string", s: form.s };
    case "symbol":
        lisp_assert_symbol_form(form, "Bad symbol form", form);
        return { vopt: "identifier", name: form.name, namespace: "variable" };
    case "compound":
        lisp_assert_compound_form(form, "Bad compound form", form);
        return lisp_compile_compound_form(form);
    }
    lisp_error("Bad form", form);
}

function lisp_compile_compound_form(form)
{
    var op = lisp_assert_symbol_form(form.elts[0], "Bad operator", form);
    var name = lisp_assert_nonempty_string(op.name, "Bad operator name", form);
    var special = lisp_special_function(name);
    if (special) {
        return special(form);
    } else {
        var macro = lisp_macro_function(op.name);
        if (macro) {
            // The macro function is a Lisp function, so the calling
            // convention argument must be supplied.
            return lisp_compile(macro(null, form));
        } else {
            return lisp_compile_function_application(form);
        }
    }
}

function lisp_compile_function_application(form)
{
    var op = lisp_assert_symbol_form(form.elts[0], "Bad function call", form);
    var name = lisp_assert_nonempty_string(op.name, "Bad function name", form);
    var fun = { vopt: "identifier", name: name, namespace: "function" };
    var call_site = lisp_compile_call_site(form.elts.slice(1));
    return { vopt: "funcall", 
             fun: fun, 
             call_site: call_site };
}

/* Maps the mangled names of macros to their expander functions. */
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


/*** Special forms ***/

/* Special forms are forms with built-in evaluation rules
   (e.g. `%%if').  The names of special forms are prefixed with "%%",
   so more comfortable wrappers around them can be defined later
   (e.g. `if' with an optional alternative). */

var lisp_specials_table = {
    "%%defined?": lisp_compile_special_definedp,
    "%%defparameter": lisp_compile_special_defparameter,
    "%%defsyntax": lisp_compile_special_defsyntax,
    "%%eval-when-compile": lisp_compile_special_eval_when_compile,
    "%%funcall": lisp_compile_special_funcall,
    "%%identifier": lisp_compile_special_identifier,
    "%%if": lisp_compile_special_if,
    "%%lambda": lisp_compile_special_lambda,
    "%%native": lisp_compile_special_native,
    "%%native-snippet": lisp_compile_special_native_snippet,
    "%%number-form": lisp_compile_special_number_form,
    "%%progn": lisp_compile_special_progn,
    "%%quasiquote": lisp_compile_special_quasiquote,
    "%%quote": lisp_compile_special_quote,
    "%%setq": lisp_compile_special_setq,
    "%%string-form": lisp_compile_special_string_form,
    "%%symbol-form": lisp_compile_special_symbol_form
};

function lisp_special_function(name)
{
    lisp_assert_nonempty_string(name, "Bad special form name", name);
    return lisp_specials_table[name];
}

/**** List of special forms ****/

/* Accesses the value of a global or local variable.
   Name and namespace are not evaluated.
   (%%identifier name namespace) -> value */
function lisp_compile_special_identifier(form)
{
    var name = lisp_assert_symbol_form(form.elts[1],
                                       "Bad identifier name", form);
    var namespace = lisp_assert_symbol_form(form.elts[2],
                                            "Bad identifier namespace", form);
    return { vopt: "identifier",
             name: name.name,
             namespace: namespace.name };
}

/* Updates the value of a global variable.
   Defines the global variable if it doesn't exist yet.
   (%%defparameter identifier value) -> value */
function lisp_compile_special_defparameter(form)
{
    var name_form = lisp_assert_not_null(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "setq", 
             name: lisp_compile(name_form),
             value: lisp_compile(value_form) };
}

/* Updates the value of a global or local variable.
   (%%setq name value) -> value */
function lisp_compile_special_setq(form)
{
    var name_form = lisp_assert_not_null(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "setq",
             name: lisp_compile(name_form),
             value: lisp_compile(value_form) };
}

/* Returns true if a local or global variable is defined (unlike
   Common Lisp's `boundp', the identifier is not evaluated).
   (%%defined? identifier) -> boolean */
function lisp_compile_special_definedp(form)
{
    var name_form = lisp_assert_not_null(form.elts[1]);
    return { vopt: "defined?",
             name: lisp_compile(name_form) };
}

/* Evaluates a number of forms in sequence and returns the value of the last.
   (%%progn &rest forms) -> value
   (%%progn) -> nil */
function lisp_compile_special_progn(form)
{
    var forms = form.elts.slice(1);
    return { vopt: "progn", 
             vops: forms.map(lisp_compile) };
}

/* In EdgeLisp #f and nil are considered false, all other
   objects (including the number zero) are true.
   (%%if test consequent alternative) -> result */
function lisp_compile_special_if(form)
{
    var test = lisp_assert_not_null(form.elts[1]);
    var consequent = lisp_assert_not_null(form.elts[2]);
    var alternative = lisp_assert_not_null(form.elts[3]);
    return { vopt: "if",
             test: lisp_compile(test),
             consequent: lisp_compile(consequent),
             alternative: lisp_compile(alternative) };
}

/* Creates a function.  See heading ``Functions''.
   (%%lambda sig body) -> function */
function lisp_compile_special_lambda(form)
{
    lisp_assert_compound_form(form.elts[1]);
    var sig = form.elts[1].elts;
    var body = form.elts[2];
    return { vopt: "lambda", 
             sig: lisp_compile_sig(sig),
             body: lisp_compile(body) };
}

/* Calls a function.
   (%%funcall fun &rest args &all-keys keys) -> result */
function lisp_compile_special_funcall(form)
{
    var fun = lisp_assert_not_null(form.elts[1]);
    var call_site = lisp_assert_not_null(form.elts.slice(2), "Bad call-site", form);
    return { vopt: "funcall",
             fun: lisp_compile(fun),
             call_site: lisp_compile_call_site(call_site) };
}

/* Registers a macro expander function.  An expander function takes a
   form as input and returns a form.  The expander-function argument
   is evaluated at compile-time.
   (%%defsyntax name expander-function) -> nil */
function lisp_compile_special_defsyntax(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1], "Bad syntax name", form);
    var expander_form = lisp_assert_not_null(form.elts[2], "Bad syntax expander", form);
    lisp_set_macro_function(name_form.name, lisp_eval(expander_form));
    return { vopt: "identifier", name: "nil", namespace: "variable" };
}

/* Executes body-form at compile-time, not at runtime.
   (%%eval-when-compile body-form) -> nil */
function lisp_compile_special_eval_when_compile(form)
{
    var body_form = lisp_assert_not_null(form.elts[1]);
    lisp_eval(body_form);
    return { vopt: "identifier", name: "nil", namespace: "variable" };
}

/* See heading ``Quasiquotation''.
   (%%quasiquote form) -> form */
function lisp_compile_special_quasiquote(form)
{
    return lisp_compile(lisp_qq(form));
}

/* See heading ``Quasiquotation''.
   (%%quote form) -> form */
function lisp_compile_special_quote(form)
{
    return lisp_compile_special_quasiquote(form);
}

/* Produces  a symbol form.
   (%%symbol-form symbol) -> symbol-form */
function lisp_compile_special_symbol_form(form)
{
    var symbol = lisp_assert_symbol_form(form.elts[1], "Bad symbol");
    return { vopt: "symbol-form",
             name: symbol.name };
}

/* Produces a number form.
   (%%number-form form) -> number-form */
function lisp_compile_special_number_form(form)
{
    var numform = lisp_assert_number_form(form.elts[1]);
    return { vopt: "number-form",
             sign: numform.sign, 
             integral_digits: numform.integral_digits,
             fractional_digits: numform.fractional_digits };
}

/* Produces a string form.
   (%%string-form form) -> string-form */
function lisp_compile_special_string_form(form)
{
    var strform = lisp_assert_string_form(form.elts[1]);
    return { vopt: "string-form",
             s: strform.s };
}

/* Contains a mixture of ordinary forms and NATIVE-SNIPPET forms.
   (%%native &rest forms -> result) */
function lisp_compile_special_native(form)
{
    return { vopt: "native",
             stuff: form.elts.slice(1).map(lisp_compile) };
}

/* A piece of JS text to directly emit.  Must appear inside NATIVE.
   (%%native-snippet js-string) */
function lisp_compile_special_native_snippet(form)
{
    var js_string = lisp_assert_string_form(form.elts[1]);
    return{ vopt: "native-snippet",
            text: js_string.s };
}

/*** Functions ***/

/**** Overview ****/

/* A function can have required, optional, keyword, rest, and all-keys
   parameters.  Required parameters can be typed.  Optional and
   keyword parameters can have init forms (default values).  A rest
   parameter is bound to a sequence of any remaining positional
   arguments.  An all-keys parameter is bound to a dictionary of all
   keyword arguments passed to a function.
   
   Required and optional parameters are called positional, because
   they are bound from left to right. */
   
/**** Binding of parameters to arguments ****/

/* When a function is called, the function's required and optional
   parameters are bound to the positional arguments from left to
   right.  If there are remaining arguments, and the function defines
   a rest parameter, it is bound to a sequence containing the
   remaining arguments.  If the function defines keyword parameters,
   they are bound to the corresponding keyword arguments.  If the
   function defines an all-keys parameter, it is bound to a dictionary
   containing all keyword arguments, even those already bound to
   keyword parameters.

   In contrast to JavaScript, EdgeLisp does not allow a function to
   be called with less positional arguments than there are required
   parameters in its signature.  Likewise, a function can only be
   called with more positional arguments than positional parameters
   (required plus optional) if the function's signature defines a rest
   parameter.

   On the other hand, keyword parameters are always optional.
   Furthermore, EdgeLisp does not constrain the allowable keywords
   supplied to a function: even if a function's signature does not
   contain keyword parameters, the caller may still supply keyword
   arguments to the function, or, in case a function defines keyword
   parameters, the caller may supply different keyword arguments.
   (The function has no means to access these unrequested arguments,
   unless it has an all-keys parameter) */

/**** Signature syntax ****/

/* The different kinds of parameters in a function signature are
   introduced by signature keywords: &optional, &key, &rest,
   &all-keys, and &aux.

   For example, a signature with 2 required, 1 optional, and 1 keyword
   argument may look like this:

   (req1 req2 &optional opt1 &key key1)

   All parameters to the right of a signature keyword, and to the left
   of another signature keyword or the end of the signature, belong to
   that kind of parameter.  For example, all parameters to the right
   of an &optional keyword, and to the left of another keyword or the end
   of the list, are optional.  The initial parameters in a signature,
   before any signature keyword appears, are required.
   
   While it's not very useful, it is possible to repeat signature
   keywords, e.g.:

   (&optional opt1 &key key1 &optional opt2 &key key2)

   If a signature contains multiple &rest and &all-keys parameters,
   the leftmost one is used, e.g. in the signature (&rest a b
   &all-keys c d), the parameter `a' is bound to the sequence of
   remaining positional arguments, and the parameter `c' is bound to
   the dictionary of supplied keyword arguments.

   &aux parameters are not really parameters, but define new variables
   in the function.  For example, (funcall (lambda (&aux (x 1)) x))
   yields 1. */

/**** Typed parameter syntax ****/

/* Required parameters can be typed, meaning they will only accept
   arguments that are general instances of a given type.  For example,
   the following signature has two typed parameters:

   ((s string) (n number)) */

/**** Parameter init forms ****/

/* Optional and keyword parameters can have init forms, that are used
   when the parameter is not supplied with an argument.  An init form
   is evaluated in an environment where all parameters to the left of
   it in the parameter list are bound to their respective arguments
   (or init forms).

   A parameter and its init form are placed into a compound form --
   for example, (&optional (log-file "/tmp/log")) is a signature with a
   optional parameter named `log-file' with the init form "/tmp/log". */

/**** Signature objects ****/

/* Function signatures are represented as objects:

   { req_params: <list>,
     opt_params: <list>,
     key_params: <list>,
     rest_param: <param>,
     all_keys_param: <param>,
     aux_params: <list>,
     fast_param: <param> }

   req_params, opt_params, key_params, aux_params: lists of required,
   optional, keyword, and aux parameters, respectively.
   
   rest_param, all_keys_param: the rest and all-keys parameters, or
   null.

   fast_param: see ``Fast parameter passing''.

   Parameters are also represented as objects:

   { name: <string>,
     init: <vop>,
     specializer: <string> }

   name: name of the parameter;
   
   init: VOP for the value to be used when no argument is supplied to
   the parameter (only significant for optional and keyword
   parameters).

   specializer: name of the parameter's type (only significant for
   required parameters).
 */

/**** Fast parameter passing ****/

/* Generic functions must be able to dispatch to another function (the
   method) as quickly as possible.  One sub-problem of this is that
   the generic needs to forward the arguments it received as-is to the
   method.  So far, the only way to do that has been for the generic
   to define a rest and an all-keys parameter, and supply these when
   `apply'ing the method.

   However, the rest parameter processing has a huge cost: because
   JavaScript's `arguments' object is not an array, it cannot be
   sliced (to chop off the calling convention argument), and so a
   subset of the `arguments' has to be copied into a new array for the
   rest parameter on each call.

   Enter fast parameter passing: a generic function declares a fast
   parameter in its signature with the `&fast' keyword.  This is bound
   to the JavaScript `arguments' list, including the calling
   convention argument.  For ordinary Lisp code, this arguments list
   is useless.  There's only one thing that can be done with it: it
   can be used to `fast-apply' the method.  Fast-applying simply
   `Function.apply's the method with the complete `arguments' list.
   Furthermore, the presence of a fast parameter is a hint to the
   compiler to perform no arity or type-checking of the function
   arguments (which is done by the method anyways), further boosting
   performance. */

var lisp_optional_sig_keyword = "&optional";
var lisp_key_sig_keyword = "&key";
var lisp_rest_sig_keyword = "&rest";
var lisp_all_keys_sig_keyword = "&all-keys";
var lisp_aux_sig_keyword = "&aux";
var lisp_fast_sig_keyword = "&fast";
var lisp_sig_keywords = 
    [lisp_optional_sig_keyword,
     lisp_key_sig_keyword,
     lisp_rest_sig_keyword,
     lisp_all_keys_sig_keyword,
     lisp_aux_sig_keyword,
     lisp_fast_sig_keyword];

function lisp_is_sig_keyword(string)
{
    return lisp_array_contains(lisp_sig_keywords, string);
}

/* Given a list of parameter forms, return a signature. */
function lisp_compile_sig(params)
{
    var req = [], opt = [], key = [], rest = [], all_keys = [], aux = [], fast = [];
    var cur = req;

    function compile_parameter(param)
    {
        if (param.formt === "symbol") {
            // Ordinary parameter (positional or keyword)
            return { name: param.name };
        } else if ((param.formt === "compound") &&
                   (cur === req)) {
            // Typed required parameter
            var name_form = lisp_assert_symbol_form(param.elts[0]);
            var specializer_form = lisp_assert_symbol_form(param.elts[1]);
            return { name: name_form.name,
                     specializer: specializer_form.name };
        } else if ((param.formt === "compound") &&
                   ((cur === opt) ||
                    (cur === key) ||
                    (cur === aux))) {
            // Optional or keyword parameter with init form
            var name_form = lisp_assert_symbol_form(param.elts[0]);
            var init_form = lisp_assert_not_null(param.elts[1]);
            return { name: name_form.name,
                     init: lisp_compile(init_form) };
        } else {
            lisp_error("Bad parameter: " + JSON.stringify(param), params);
        }
    }

    for (var i = 0, len = params.length; i < len; i++) {
        var param = params[i];
        lisp_assert_not_null(param, "Bad param", params);
        if (param.formt === "symbol") {
            if (lisp_is_sig_keyword(param.name)) {
                switch (param.name) {
                case lisp_optional_sig_keyword: 
                    cur = opt; continue;
                case lisp_key_sig_keyword: 
                    cur = key; continue;
                case lisp_rest_sig_keyword: 
                    cur = rest; continue;
                case lisp_all_keys_sig_keyword: 
                    cur = all_keys; continue;
                case lisp_aux_sig_keyword: 
                    cur = aux; continue;
                case lisp_fast_sig_keyword:
                    cur = fast; continue;
                }
                lisp_error("Bad signature keyword", param.name);
            }
        }
        cur.push(compile_parameter(param));
    }
    
    return { req_params: req, 
             opt_params: opt, 
             key_params: key, 
             rest_param: rest[0],
             all_keys_param: all_keys[0],
             aux_params: aux,
             fast_param: fast[0] };
}

function lisp_param_name(param)
{
    return lisp_assert_nonempty_string(param.name);
}

function lisp_mangled_param_name(param)
{
    return lisp_mangle_var(lisp_param_name(param));
}

/**** Function call sites ****/

/* At a function call site, the positional and keyword arguments are
   apparent at compile-time.  (This enables a fast calling convention,
   see below.)  For example, in the call `(foo file: f 12)', there is
   one keyword argument named `file' with the value `f' and one
   positional argument with the value `12'.

   Call sites are represented as objects:

   { pos_args: <list>, 
     key_args: <dict> }

   pos_args: list of VOPs of positional arguments;

   key_args: Dictionary that maps mangled keyword argument names
   (without the trailing ":") to their value VOPs. */

function lisp_is_keyword_arg(string)
{
    if (string.length > 1)
        return string[0] === ":";
    else
        return false;
}

function lisp_clean_keyword_arg(string)
{
    return string.slice(1, string.length);
}

/* Given a list of argument forms, return a call site. */
function lisp_compile_call_site(args)
{
    var pos_args = [];
    var key_args = {};
    
    for (var i = 0, len = args.length; i < len; i++) {
        var arg = args[i];
        if (arg.formt === "symbol") {
            if (lisp_is_keyword_arg(arg.name)) {
                var name = lisp_clean_keyword_arg(arg.name);
                var value = lisp_compile(args[++i]);
                key_args[lisp_mangle_string_dict_key(name)] = value;
                continue;
            }
        }
        pos_args.push(lisp_compile(arg));
    }

    return { pos_args: pos_args,
             key_args: key_args };
}

/**** Calling convention ****/

/* Given that we can statically determine positional and keyword
   arguments at a call site (see above), we can implement a calling
   convention that, for calls with only required arguments, is as fast
   as a normal JavaScript call.
   
   All functions get a hidden calling convention parameter as first
   parameter.  This parameter is a dictionary that maps the names of
   keyword arguments to their value VOPs.  The names of the keyword
   arguments do not contain the trailing ":".  After the keywords
   dictionary, the positional arguments are passed as normal
   JavaScript arguments.

   See `lisp_emit_vop_funcall' and `lisp_emit_vop_lambda', below, for
   the implementation of the caller and callee sides of the calling
   convention, respectively. */

// Name of the calling convention parameter.
var lisp_keywords_dict = "_key_";

/*** Quasiquotation ***/

// These rules are from the Fargo language.
var lisp_qq_rules =
    [
     // Atoms
     [["%%quasiquote", {"?": "symbol", "type":"symbol"}],
      ["%%symbol-form", {"!":"symbol"}]],
     [["%%quasiquote", {"?": "number", "type":"number"}],
      ["%%number-form", {"!":"number"}]],
     [["%%quasiquote", {"?": "string", "type":"string"}],
      ["%%string-form", {"!":"string"}]],
     // Lists
     [["%%quasiquote", ["%%unquote", {"?":"expr"}]],
      {"!":"expr"}],
     [["%%quasiquote", [["%%unquote-splicing", {"?":"first"}], {"?*":"rest"}]],
      ["%append-compounds", {"!":"first"}, ["%%quasiquote", {"!":"rest"}]]],
     [["%%quasiquote", [{"?":"first"}, {"?*":"rest"}]],
      ["%append-compounds", ["%make-compound", ["%%quasiquote", {"!":"first"}]],
       ["%%quasiquote", {"!":"rest"}]]],
     [["%%quasiquote", []],
      ["%make-compound"]]
     ];

function lisp_qq(form)
{
    if (!form) lisp_error("bad qq form");
    for (var i = 0; i < lisp_qq_rules.length; i++) {
        var rule = lisp_qq_rules[i];
        var lhs = rule[0];
        var rhs = rule[1];
        var bindings = qq_match(lhs, form);
        if (bindings !== false) {
            return qq_rewrite(rhs, bindings);
        }
    }
    return lisp_error("Quasiquote bug", form);

    function qq_match(lhs, form) {
        if (!form) lisp_error("bad match form");
        var bindings = {};
        if (typeof lhs === "string") {
            if (form.formt !== "symbol") return false;
            return (lhs === form.name) ? bindings : false;
        } else if (lhs["?"]) {
            if (lhs.type && !(lhs.type === form.formt)) return false;
            bindings[lhs["?"]] = form;
            return bindings;
        } else if (lhs.length !== undefined) {
            if (form.formt !== "compound") return false;
            var hasRestVar = false;
            for (var i = 0; i < lhs.length; i++) {
                if (lhs[i]["?*"]) {
                    bindings[lhs[i]["?*"]] = new Lisp_compound_form(form.elts.slice(i));
                    hasRestVar = true;
                    break;
                }
                if (i >= form.elts.length) return false;
                var nested_bindings = qq_match(lhs[i], form.elts[i]);
                if (!nested_bindings) { return false; }
                for (var name in nested_bindings)
                    if (nested_bindings.hasOwnProperty(name))
                        bindings[name] = nested_bindings[name];
            }
            if ((!hasRestVar) && (lhs.length !== form.elts.length)) return false;
            return bindings;
        } else {
            return lisp_error("bad lhs", lhs);
        }
    }

    function qq_rewrite(rhs, bindings) {
        if (typeof rhs === "string") {
            return new Lisp_symbol_form(rhs);
        } else if (rhs["!"]) {
            return bindings[rhs["!"]];
        } else if (rhs.length !== undefined) {
            return new Lisp_compound_form(rhs.map(function(nested_rhs) {
                        return qq_rewrite(nested_rhs, bindings); 
                    }));
        } else {
            return lisp_error("bad rhs", lhs);
        }
    }
}

/*** Virtual Operations ***/

/* Virtual operations (VOPs) are low-level operations that are emitted
   to JavaScript.  Note that there are no macro- and
   quasiquotation-related VOPs, as these special forms are expanded
   away or rewritten during compilation. */

var lisp_vop_table = {
    "defined?": lisp_emit_vop_definedp,
    "funcall": lisp_emit_vop_funcall,
    "identifier": lisp_emit_vop_identifier,
    "if": lisp_emit_vop_if,
    "lambda": lisp_emit_vop_lambda,
    "native": lisp_emit_vop_native,
    "native-snippet": lisp_emit_vop_native_snippet,
    "number": lisp_emit_vop_number,
    "number-form": lisp_emit_vop_number_form,
    "progn": lisp_emit_vop_progn,
    "setq": lisp_emit_vop_setq,
    "string": lisp_emit_vop_string,
    "string-form": lisp_emit_vop_string_form,
    "symbol-form": lisp_emit_vop_symbol_form
};

// Emits a VOP to JavaScript.
function lisp_emit(vop)
{
    lisp_assert_string(vop.vopt, "Bad .vopt", vop);
    var vop_function = lisp_vop_function(vop.vopt);
    lisp_assert_not_null(vop_function, "No VOP emitter function", vop);
    return vop_function(vop);
}

function lisp_vop_function(vopt)
{
    return lisp_vop_table[vopt];
}

/**** List of VOPs ****/

/* Evaluates a number of VOPs in sequence and returns the value of the last.
   { vopt: "progn", vops: <list> }
   vops: list of VOPs. */
function lisp_emit_vop_progn(vop)
{
    lisp_assert_list(vop.vops, "Bad progn", vop);
    if (vop.vops.length > 0)
        return "(" + vop.vops.map(lisp_emit).join(", ") + ")";
    else
        return "null";
}

/* Variable reference.
   { vopt: "identifier", name: <string>, namespace: <string> }
   name: the name of the variable. */
function lisp_emit_vop_identifier(vop)
{
    lisp_assert_nonempty_string(vop.name, "Bad variable name", vop);
    lisp_assert_nonempty_string(vop.namespace, "Bad variable namespace", vop);
    var mname = lisp_mangle(vop.name, vop.namespace);
    var error_args = JSON.stringify(vop.name) + ", " + JSON.stringify(vop.namespace);
    return "(typeof " + mname + " !== \"undefined\" ? " + mname + " : lisp_undefined_identifier(" + error_args + "))";
}

/* Assigns a value to a variable.
   { vopt: "setq", name: <vop>, value: <vop> }
   name: the "identifier" VOP of the variable;
   value: VOP for the value. */
function lisp_emit_vop_setq(vop)
{
    lisp_assert(vop.name.vopt === "identifier", "Bad place", vop);
    lisp_assert_nonempty_string(vop.name.name, "Bad place name", vop);
    lisp_assert_nonempty_string(vop.name.namespace, "Bad place namespace", vop);
    var mname = lisp_mangle(vop.name.name, vop.name.namespace);
    var value = lisp_emit(vop.value);
    return "(" + mname + " = " + value + ")";
}

/* Checks whether variable is defined.
   { vopt: "defined?", name: <vop> }
   name: the "identifier" VOP of the variable. */
function lisp_emit_vop_definedp(vop)
{
    lisp_assert(vop.name.vopt === "identifier", "Bad place", vop);
    lisp_assert_nonempty_string(vop.name.name, "Bad place name", vop);
    lisp_assert_nonempty_string(vop.name.namespace, "Bad place namespace", vop);
    var mname = lisp_mangle(vop.name.name, vop.name.namespace);
    return "(typeof " + mname + " !== \"undefined\")";
}

/* { vopt: "if", test: <vop>, consequent: <vop>, alternative: <vop> } */
function lisp_emit_vop_if(vop)
{
    var test = lisp_emit(vop.test);
    var consequent = lisp_emit(vop.consequent);
    var alternative = lisp_emit(vop.alternative);
    return "(lisp_is_true(" +test+ ") ? " +consequent+ " : " +alternative+ ")";
}

/* Number literal.
   { vopt: "number",
     sign: <string>,
     integral_digits: <string>,
     fractional_digits: <string> } */
function lisp_emit_vop_number(vop)
{
    lisp_assert_string(vop.sign, "Bad sign", vop);
    lisp_assert_string(vop.integral_digits, "Bad integral digits", vop);
    lisp_assert_string(vop.fractional_digits, "Bad fractional digits", vop);
    var num_repr = vop.sign + vop.integral_digits + vop.fractional_digits;
    return "(lisp_number(\"" + num_repr + "\"))";
}

/* Number form.
   { vopt: "number-form",
     sign: <string>,
     integral_digits: <string>,
     fractional_digits: <string> } */
function lisp_emit_vop_number_form(vop)
{
    lisp_assert_string(vop.sign, "Bad sign", vop);
    lisp_assert_string(vop.integral_digits, "Bad integral digits", vop);
    lisp_assert_string(vop.fractional_digits, "Bad fractional digits", vop);
    return "(new Lisp_number_form("+JSON.stringify(vop.sign)+","+JSON.stringify(vop.integral_digits)+","+JSON.stringify(vop.fractional_digits)+"))";
}

/* String literal.
   { vopt: "string", s: <string> }
   s: the string in JavaScript syntax. */
function lisp_emit_vop_string(vop)
{
    lisp_assert_string(vop.s, "Bad string", vop);
    return JSON.stringify(vop.s);
}

/* String form.
   { vopt: "string-form", s: <string> } */
function lisp_emit_vop_string_form(vop)
{
    lisp_assert_string(vop.s, "Bad string form", vop);
    return "(new Lisp_string_form(" + JSON.stringify(vop.s) + "))";
}

/* Symbol form.
   { vopt: "symbol-form", name: <string> } */
function lisp_emit_vop_symbol_form(vop)
{
    lisp_assert_string(vop.name, "Bad symbol name", vop);
    return "(new Lisp_symbol_form(" + JSON.stringify(vop.name) + "))";
}

/* Creates a lexical closure.
   { vopt: "lambda", sig: <sig>, body: <vop> }
   sig: signature, see above;
   body: VOP for the function's body. */
function lisp_emit_vop_lambda(vop)
{
    var req_params = lisp_assert_not_null(vop.sig.req_params);
    var opt_params = lisp_assert_not_null(vop.sig.opt_params);
    var key_params = lisp_assert_not_null(vop.sig.key_params);
    var rest_param = vop.sig.rest_param;
    var all_keys_param = vop.sig.all_keys_param;
    var aux_params = lisp_assert_not_null(vop.sig.aux_params);
    var fast_param = vop.sig.fast_param;

    // Signature (calling convention keywords dict + positional parameters)
    var param_names = [ lisp_keywords_dict ];
    param_names = param_names.concat(req_params.map(lisp_mangled_param_name));
    param_names = param_names.concat(opt_params.map(lisp_mangled_param_name));
    var sig = param_names.join(", ");

    // Positional arguments arity check
    var min = req_params.length;
    var max = req_params.length + opt_params.length;
    var js_min = 1 + min; // Include calling convention argument
    var js_max = 1 + max; //
    if (rest_param) {
        var check_arity = 
            "lisp_arity_min(arguments.length, " + js_min + "); ";
    } else {
        var check_arity = 
            "lisp_arity_min_max(arguments.length, " +js_min+ ", " +js_max+"); ";
    }

    // Required arguments type checks
    var check_types = "";
    for (var i = 0, len = req_params.length; i < len; i++) {
        var param = req_params[i];
        if (param.specializer) {
            var name = lisp_mangle_var(param.name);
            var type_name = lisp_mangle_class(param.specializer);
            check_types += "lisp_check_type(" + name + ", " + type_name + "); ";
        }
    }

    // Optional parameter init forms
    var init_opt_params = "";
    if (opt_params.length > 0) {
        var s = "";
        for (var i = js_min; i < js_max; i++) {
            var param = opt_params[i - js_min];
            var name = lisp_mangled_param_name(param);
            var value = param.init ? lisp_emit(param.init) : "null";
            s += "if (arguments.length < " +(i+1)+ ") " +name+ " = "+value+"; ";
        }
        init_opt_params = s;
    }

    // Keyword parameter init forms
    var init_key_params = "";
    if (key_params.length > 0) {
        var with_key_dict = ""; // branch used when _key_ is supplied
        var wout_key_dict = ""; // branch used when _key_ is null
        for (var i = 0, len = key_params.length; i < len; i++) {
            var param = key_params[i];
            var name = lisp_mangled_param_name(param);
            var key_name = lisp_mangle_string_dict_key(param.name);
            var init = param.init ? lisp_emit(param.init) : "null";
            with_key_dict += "if (\"" + key_name + "\" in " + lisp_keywords_dict + ") " +
                                 "var " + name + " = " + lisp_keywords_dict + "[\"" + key_name + "\"]; " +
                             "else var " + name + " = " + init + "; ";
            wout_key_dict += "var " + name + " = " + init + "; ";
        }
        init_key_params = "if (" + lisp_keywords_dict + ") { " + 
                              with_key_dict + 
                          "} else { " + 
                              wout_key_dict +
                          "} ";
    }

    // Rest parameter
    var setup_rest_param = "";
    if (rest_param) {
        setup_rest_param =
            "var " + lisp_mangled_param_name(rest_param) + 
            " = lisp_rest_param(arguments, " + max + "); ";
    }

    // All-keys parameter
    var setup_all_keys_param = "";
    if (all_keys_param) {
        setup_all_keys_param = "var " + lisp_mangled_param_name(all_keys_param) + " = " +
            lisp_keywords_dict + " ? " + lisp_keywords_dict + " : lisp_fast_string_dict({}); ";
    }

    // Aux parameters
    var init_aux_params = "";
    if (aux_params.length > 0) {
        var s = "";
        for (var i = 0; i < aux_params.length; i++) {
            var param = aux_params[i];
            var name = lisp_mangled_param_name(param);
            var value = param.init ? lisp_emit(param.init) : "null";
            s += "var " + name + " = " + value + "; ";
        }
        init_aux_params = s;
    }

    // Fast parameter
    var setup_fast_param = "";
    if (fast_param) {
        setup_fast_param =
            "var " + lisp_mangled_param_name(fast_param) + " = arguments; ";
        check_arity = "";
        check_types = "";
    }

    var preamble = 
        check_arity + 
        check_types + 
        init_opt_params + 
        init_key_params +
        setup_rest_param +
        setup_all_keys_param +
        init_aux_params +
        setup_fast_param;

    var body = lisp_emit(vop.body);

    return "(function(" + sig + "){ " + preamble + "return (" + body + "); })";
}

/* Calls a function.
   { vopt: "funcall", fun: <vop>, call_site: <call_site> }
   fun: VOP of the function;
   call_site: positional and keyword argument VOPs (see above). */
function lisp_emit_vop_funcall(vop)
{
    var fun = lisp_assert_not_null(vop.fun, "Bad function", vop);
    var call_site = lisp_assert_not_null(vop.call_site, "Bad call site", vop);
    var key_args = call_site.key_args;

    // Generate dictionary of keyword arguments
    var s = "";
    lisp_iter_dict(key_args, function(k) {
        lisp_assert(lisp_is_string_dict_key(k), "Bad keyword argument", k);
        var v = lisp_assert_not_null(key_args[k]);
        s += "\"" + k + "\": " + lisp_emit(v) + ", ";
    });
    if (s !== "") {
        var keywords_dict = "lisp_fast_string_dict({ " + s + " })";
    } else {
        var keywords_dict = "null";
    }
    
    var pos_args = call_site.pos_args.map(lisp_emit);
    var args = [ keywords_dict ].concat(pos_args).join(", ");

    return "(" + lisp_emit(fun) + "(" + args + "))";
}

/* { vopt: "native", stuff: <vops> }
   stuff: list of VOPs. */
function lisp_emit_vop_native(vop)
{
    lisp_assert_list(vop.stuff, "Bad native", vop);
    // Note that subforms of NATIVE are not separated by commas, as is
    // usually the case.  This whole area is still rather unclear.
    return vop.stuff.map(lisp_emit).join("");
}

/* { vopt: "native-snippet", text: <string> }
   text: JS text */
function lisp_emit_vop_native_snippet(vop)
{
    lisp_assert_string(vop.text, "Bad native-snippet", vop);
    return vop.text;
}
