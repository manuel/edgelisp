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

load("lib/json2.js");
load("lib/jsparse.js");


/*** Syntax ***/

/* CyberLisp does not use a cons-based representation for Lisp source.
   Instead, forms are represented as objects, which makes it possible
   to attach additional information to forms, e.g. file name and line
   number.  CyberLisp also has a separation between in-language
   datatypes (e.g. strings) and their syntactic representations
   (e.g. string forms).  A string form in the Lisp source is not a
   Lisp string: a string in the source (a string form) may have a line
   number or other metadata attached to it, whereas an in-language
   string is simply a string.

   There are multiple types of forms; number forms, string forms,
   symbol forms, and compound forms:
   
   1     --> { formt: "number", n: "1" }
   "foo" --> { formt: "string", s: "foo" }
   foo   --> { formt: "symbol", name: "foo" }
   (foo) --> { formt: "compound", 
               elts: [ { formt: "symbol", name: "foo" } ] } 
*/

function Lisp_number_form(n)
{
    this.formt = "number";
    this.n = n;
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

var lisp_expression_syntax =
    function(input) { return lisp_expression_syntax(input); }; // forward decl.

/**** Numbers ****/

var lisp_digits = 
    join_action(repeat1(range("0", "9")), "");

var lisp_number_syntax =
    action(sequence(optional(choice("+", "-")),
                    lisp_digits,
                    optional(join_action(sequence(".", lisp_digits), ""))),
           lisp_number_syntax_action);

function lisp_number_syntax_action(ast)
{    
    var sign = ast[0] || "";
    var decimal_digits = ast[1];
    var dot_digits = ast[2] || "";
    return new Lisp_number_form(sign + decimal_digits + dot_digits);
}

/**** Strings ****/

var lisp_escape_char =
    choice("\"", "\\");

var lisp_escape_sequence =
    action(sequence("\\", lisp_escape_char),
           lisp_escape_sequence_action);

var lisp_string_char =
    choice(negate(lisp_escape_char), 
           lisp_escape_sequence);

var lisp_string_syntax =
    action(sequence("\"", join_action(repeat0(lisp_string_char), ""), "\""),
           lisp_string_syntax_action);

function lisp_escape_sequence_action(ast)
{
    var escape_char = ast[1];
    return escape_char;
}

function lisp_string_syntax_action(ast)
{
    return new Lisp_string_form(ast[1]);
}

/**** Symbols ****/

var lisp_symbol_special_char =
    // Needs to be in sync with `lisp_mangle_table'.
    choice("-", "&", ":", ".", "=", ">","<", "%", "+", "?", "/", "*");

var lisp_symbol_syntax =
    action(join_action(repeat1(choice(range("a", "z"),
                                      range("0", "9"),
                                      lisp_symbol_special_char)),
                       ""),
           lisp_symbol_syntax_action);

function lisp_symbol_syntax_action(ast)
{
    return new Lisp_symbol_form(ast);
}

/**** Compounds ****/

var lisp_compound_syntax =
    action(wsequence("(", repeat0(lisp_expression_syntax), ")"),
           lisp_compound_syntax_action);

function lisp_compound_syntax_action(ast)
{
    return new Lisp_compound_form(ast[1]);
}

/**** Inline JavaScript ****/

var lisp_alien_escape =
    action(sequence("~", lisp_expression_syntax),
           lisp_alien_escape_action);

var lisp_alien_syntax =
    action(sequence("{%",
                    repeat1(choice(lisp_alien_escape,
                                   negate("%"),
                                   join_action(sequence("%", not("}")), ""))),
                    "%}"),
           lisp_alien_syntax_action);

function lisp_alien_escape_action(ast)
{
    return ast[1];
}

function lisp_alien_syntax_action(ast)
{
    var elts = [ new Lisp_symbol_form("alien") ];
    elts = elts.concat(ast[1]);
    return new Lisp_compound_form(elts);
}

/* I don't know how to do this right yet.  The current form has a
   "layering violation", in that the (native ...) form contains
   (JavaScript snippet) strings and (~-escaped Lisp) forms.  It should
   only contain forms; the snippets should probably be turned into
   their own form like this: (native ... (snippet ...) ...). 

   Then there's the issue that a "~" anywhere in the JavaScript
   triggers a Lisp escape. */

/**** Misc shortcuts ****/

var lisp_quote_syntax =
    action(sequence("'", lisp_expression_syntax),
           lisp_shortcut_syntax_action("quote"));

var lisp_quasiquote_syntax =
    action(sequence("`", lisp_expression_syntax),
           lisp_shortcut_syntax_action("quasiquote"));

var lisp_unquote_syntax =
    action(sequence(",", lisp_expression_syntax),
           lisp_shortcut_syntax_action("unquote"));

var lisp_unquote_splicing_syntax =
    action(sequence(",@", lisp_expression_syntax),
           lisp_shortcut_syntax_action("unquote-splicing"));

var lisp_function_syntax =
    action(sequence("#'", lisp_symbol_syntax),
           lisp_shortcut_syntax_action("function"));

function lisp_shortcut_syntax_action(name)
{
    return function(ast) {
        return new Lisp_compound_form([ new Lisp_symbol_form(name),
                                        ast[1] ]);
    }
}

/**** Programs ****/

var lisp_expression_syntax =
    whitespace(choice(lisp_number_syntax,
                      lisp_string_syntax,
                      lisp_symbol_syntax,
                      lisp_compound_syntax,
                      lisp_alien_syntax,
                      lisp_quote_syntax,
                      lisp_quasiquote_syntax,
                      lisp_unquote_syntax,
                      lisp_unquote_splicing_syntax,
                      lisp_function_syntax));

var lisp_program_syntax =
    whitespace(repeat1(lisp_expression_syntax));

function lisp_parse(string)
{
    return lisp_program_syntax(ps(string)).ast;
}


/*** Compilation and Evaluation ***/

function lisp_eval(string)
{
    var forms = lisp_parse(string);
    var vop = { vopt: "progn", vops: forms.map(lisp_compile) };
    var js = lisp_emit(vop);
    return eval(js);
}

/* The usual Lisp evaluation rule: literals evaluate to themselves;
   symbols evaluate to the value of the binding they name.  A compound
   form is evaluated differently depending on whether its first
   element names a special form, a macro, or a function: special form
   calls are evaluated with special evaluation rules; macro calls are
   first expanded and then compiled recursively; function calls are
   evaluated by applying the named function to the supplied
   arguments. */

function lisp_compile(form)
{
    switch(form.formt) {
    case "number":
        lisp_assert_nonempty_string(form.n, "Bad .n", form);
        return { vopt: "number", n: form.n };
    case "string":
        lisp_assert_string(form.s, "Bad .s", form);
        return { vopt: "string", s: form.s };
    case "symbol":
        lisp_assert_symbol_form(form, "Bad symbol form", form);
        return { vopt: "ref", name: form.name };
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
    var fun = { vopt: "function", name: name };
    var call_site = lisp_compile_call_site(form.elts.slice(1));
    return { vopt: "funcall", 
             fun: fun, 
             call_site: call_site };
}


/*** Special forms ***/

/* Special forms are built-in forms with special evaluation rules
   (e.g. `%%if').  The names of some special forms are prefixed with
   "%%", so more comfortable wrappers around them can be defined later
   (e.g. `if' with an optional alternative). */

function lisp_special_function(name)
{
    lisp_assert_nonempty_string(name, "Bad special form name", name);
    return lisp_specials_table[name];
}

var lisp_specials_table = {
    "alien": lisp_compile_special_alien,
    "bound?": lisp_compile_special_boundp,
    "defparameter": lisp_compile_special_defparameter,
    "eval-and-compile": lisp_compile_special_eval_and_compile,
    "eval-when-compile": lisp_compile_special_eval_when_compile,
    "fbound?": lisp_compile_special_fboundp,
    "funcall": lisp_compile_special_funcall,
    "function": lisp_compile_special_function,
    "%%if": lisp_compile_special_if,
    "%%lambda": lisp_compile_special_lambda,
    "progn": lisp_compile_special_progn,
    "quasiquote": lisp_compile_special_quasiquote,
    "quote": lisp_compile_special_quote,
    "%%set": lisp_compile_special_set,
    "set-expander": lisp_compile_special_set_expander,
    "set-function": lisp_compile_special_set_function,
};

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

/* Maps the mangled names of macros to their expander functions. */
var lisp_macros_table = {};

/**** List of special forms ****/

/* (alien &rest js-strings-n-lisp-forms) */
function lisp_compile_special_alien(form)
{
    var stuff = form.elts.slice(1);
    stuff = stuff.map(function(thing) {
            if (thing.formt) {
                return lisp_compile(thing);
            } else {
                lisp_assert_string(thing);
                return thing;
            }
        });
    return { vopt: "alien",
             stuff: stuff };
}

/* Returns true if `name' is bound (unlike Common Lisp's `boundp',
   name is not evaluated).  
   (bound? name) */
function lisp_compile_special_boundp(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    return { vopt: "bound?", 
             name: name_form.name };
}

/* Assigns the `value' to the global variable named `name'.
   (defparameter name value) */
function lisp_compile_special_defparameter(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "set", 
             name: name_form.name, 
             value: lisp_compile(value_form) };
}

/* Executes forms during both compilation and evaluation.
   (eval-and-compile &rest forms) */
function lisp_compile_special_eval_and_compile(form)
{
    var forms = form.elts.slice(1);
    return { vopt: "eval_when",
             vop: { vopt: "progn", vops: forms.map(lisp_compile) },
             when: [ "eval", "compile" ] };
}

/* Executes forms only during compilation, not during evaluation.
   (eval-when-compile &rest forms) */
function lisp_compile_special_eval_when_compile(form)
{
    var forms = form.elts.slice(1);
    return { vopt: "eval_when",
             vop: { vopt: "progn", vops: forms.map(lisp_compile) },
             when: [ "compile" ] };
}

/* Assigns the `value' to the global function named `name'.
   (set-function name value) */
function lisp_compile_special_set_function(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "set_function", 
             name: name_form.name, 
             value: lisp_compile(value_form) };
}

/* Registers a macro expander function.  An expander function takes a
   form as input and must return a form.
   (set-expander name expander-function) */
function lisp_compile_special_set_expander(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    var expander_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "set_macro", 
             name: name_form.name, 
             expander: lisp_compile(expander_form) };
}

/* Returns true if `name' is bound in the function namespace (unlike
   Common Lisp's `fboundp', name is not evaluated).  
   (fbound? name) */
function lisp_compile_special_fboundp(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    return { vopt: "fbound?", 
             name: name_form.name };
}

/* Calls a function passed as argument.
   (funcall fun &rest args &all-keys keys) => result */
function lisp_compile_special_funcall(form)
{
    var fun = lisp_assert_not_null(form.elts[1]);
    var call_site = lisp_assert_not_null(form.elts.slice(2));
    return { vopt: "funcall",
             fun: lisp_compile(fun),
             call_site: lisp_compile_call_site(call_site) };
}

/* Accesses the functional value of a name.
   (function name) => function */
function lisp_compile_special_function(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    return { vopt: "function", 
             name: name_form.name };
}

/* In CyberLisp `false' and `null' are considered false, all other
   objects (including the number zero) are true.  
   (%%if test consequent alternative) */
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

/* Returns a lexical closure.  See heading ``Functions''.
   (%%lambda sig body) */
function lisp_compile_special_lambda(form)
{
    lisp_assert_compound_form(form.elts[1]);
    var sig = form.elts[1].elts;
    var body = form.elts[2];
    return { vopt: "lambda", 
             sig: lisp_compile_sig(sig),
             body: lisp_compile(body) };
}

/* Evaluates a number of forms in sequence and returns the value of the last.
   (progn &rest forms) */
function lisp_compile_special_progn(form)
{
    var forms = form.elts.slice(1);
    return { vopt: "progn", 
             vops: forms.map(lisp_compile) };
}

/* (%%set name value) */
function lisp_compile_special_set(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "set",
             name: name_form.name,
             value: lisp_compile(value_form) };
}

/* See heading ``Quasiquotation''.
   (quasiquote form) */
function lisp_compile_special_quasiquote(form)
{
    var quasiquoted = lisp_assert_not_null(form.elts[1]);
    return lisp_compile_qq(quasiquoted, 0);
}

/* (quote form) */
function lisp_compile_special_quote(form)
{
    var quoted = lisp_assert_not_null(form.elts[1]);
    return { vopt: "quote", 
             form: quoted };
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

   In contrast to JavaScript, CyberLisp does not allow a function to
   be called with less positional arguments than there are required
   parameters in its signature.  Likewise, a function can only be
   called with more positional arguments than positional parameters
   (required plus optional) if the function's signature defines a rest
   parameter.

   On the other hand, keyword parameters are always optional.
   Furthermore, CyberLisp does not constrain the allowable keywords
   supplied to a function: even if a function's signature does not
   contain keyword parameters, the caller may still supply keyword
   arguments to the function, or, in case a function defines keyword
   parameters, the caller may supply different keyword arguments.
   (The function has no means to access these unrequested arguments,
   unless it has an all-keys parameter) */

/**** Signature syntax ****/

/* The different kinds of parameters in a function signature are
   introduced by signature keywords: &opt, &key, &rest, and &all-keys.

   For example, a signature with 2 required, 1 optional, and 1 keyword
   argument may look like this:

   (req1 req2 &opt opt1 &key key1)

   All parameters to the right of a signature keyword, and to the left
   of another signature keyword or the end of the signature, belong to
   that kind of parameter.  For example, all parameters to the right
   of an &opt keyword, and to the left of another keyword or the end
   of the list, are optional.  The initial parameters in a signature,
   before any signature keyword appears, are required.
   
   While it's not very useful, it is possible to repeat signature
   keywords, e.g.:

   (&opt opt1 &key key1 &opt opt2 &key key2)

   If a signature contains multiple &rest and &all-keys parameters,
   the leftmost one is used, e.g. in the signature (&rest a b
   &all-keys c d), the parameter `a' is bound to the sequence of
   remaining positional arguments, and the parameter `c' is bound to
   the dictionary of supplied keyword arguments. */

/**** Typed parameter syntax ****/

/* Required parameters can be typed, meaning they will only accept
   arguments that are general instances of a given type.  For example,
   the following signature has two typed parameters:

   ((s <string>) (n <number>)) */

/**** Parameter init forms ****/

/* Optional and keyword parameters can have init forms, that are used
   when the parameter is not supplied with an argument.  An init form
   is evaluated in an environment where all parameters to the left of
   it in the parameter list are bound to their respective arguments
   (or init forms).

   A parameter and its init form are placed into a compound form --
   for example, (&opt (log-file "/tmp/log")) is a signature with a
   optional parameter named `log-file' with the init form "/tmp/log". */

/**** Signature objects ****/

/* Function signatures are represented as objects:

   { req_params: <list>,
     opt_params: <list>,
     key_params: <list>,
     rest_param: <param>,
     all_keys_param: <param>,
     fast_param: <param> }

   req_params, opt_params, key_params: lists of required, optional,
   and keyword parameters, respectively.
   
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

var lisp_optional_sig_keyword = "&opt";
var lisp_key_sig_keyword = "&key";
var lisp_rest_sig_keyword = "&rest";
var lisp_all_keys_sig_keyword = "&all-keys";
var lisp_fast_sig_keyword = "&fast";
var lisp_sig_keywords = 
    [lisp_optional_sig_keyword,
     lisp_key_sig_keyword,
     lisp_rest_sig_keyword,
     lisp_all_keys_sig_keyword,
     lisp_fast_sig_keyword];

function lisp_is_sig_keyword(string)
{
    return lisp_array_contains(lisp_sig_keywords, string);
}

function lisp_is_type_name(string)
{
    return ((string.length >= 3) &&
            (string[0] == "<") &&
            (string[string.length - 1] == ">"));
}

function lisp_clean_type_name(string)
{
    return string.slice(1, string.length - 1);
}

/* Given a list of parameter forms, return a signature. */
function lisp_compile_sig(params)
{
    var req = [], opt = [], key = [], rest = [], all_keys = [], fast = [];
    var cur = req;

    function compile_parameter(param)
    {
        if (param.formt == "symbol") {
            // Ordinary parameter (positional or keyword)
            return { name: param.name };
        } else if ((param.formt == "compound") &&
                   (cur == req)) {
            // Typed required parameter
            var name_form = lisp_assert_symbol_form(param.elts[0]);
            var specializer_form = lisp_assert_symbol_form(param.elts[1]);
            return { name: name_form.name,
                     specializer: specializer_form.name };
        } else if ((param.formt == "compound") &&
                   ((cur == opt) || (cur == key))) {
            // Optional or keyword parameter with init form
            var name_form = lisp_assert_symbol_form(param.elts[0]);
            var init_form = lisp_assert_not_null(param.elts[1]);
            return { name: name_form.name,
                     init: lisp_compile(init_form) };
        } else {
            lisp_error("Bad parameter: " + uneval(param), params);
        }
    }

    for (var i = 0, len = params.length; i < len; i++) {
        var param = params[i];
        lisp_assert_not_null(param, "Bad param", params);
        if (param.formt == "symbol") {
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
        return string[string.length - 1] == ":";
    else
        return false;
}

function lisp_clean_keyword_arg(string)
{
    return string.slice(0, string.length - 1);
}

/* Given a list of argument forms, return a call site. */
function lisp_compile_call_site(args)
{
    var pos_args = [];
    var key_args = {};
    
    for (var i = 0, len = args.length; i < len; i++) {
        var arg = args[i];
        if (arg.formt == "symbol") {
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

/* A quasiquote form is compiled into code that, when evaluated,
   produces a form.  No, really.

   Except for multi-argument unquotes, which are not supported, this
   algorithm should produce the same results as the one in Appendix B
   of Alan Bawden's paper "Quasiquotation in LISP", 1999. */

function lisp_compile_qq(x, depth)
{
    if (depth < 0) 
        lisp_error("Negative quasiquotation nesting depth", x);

    switch(x.formt) {
    case "number":
    case "symbol":
    case "string":
        return { vopt: "quote", form: x };
    case "compound":
        return lisp_compile_qq_compound(x, depth);
    }

    lisp_error("Bad quasiquoted form", x);
}

function lisp_compile_qq_compound(x, depth)
{
    var op = x.elts[0];
    if (op) {
        if (is_unquote(op)) {
            if (depth == 0) {
                return lisp_compile(x.elts[1]);
            } else {
                return unquote(x.elts[1], depth - 1);
            }
        } else if (is_quasiquote(op)) {
            return quasiquote(x.elts[1], depth + 1);
        } else {
            return compile_compound(x, depth);
        }
    } else {
        return make_compound([]);
    }

    function compile_compound(x, depth)
    {
        var compounds = [], compound_elts = [];
        for (var i = 0, len = x.elts.length; i < len; i++) {
            var sub = x.elts[i];
            if ((sub.formt == "compound") && is_unquote_splicing(sub.elts[0])) {
                compounds.push(make_compound(compound_elts));
                compound_elts = [];
                if (depth == 0) {
                    compounds.push(lisp_compile(sub.elts[1]));
                } else {
                    compounds.push(unquote_splicing(sub.elts[1], depth - 1));
                }
            } else {
                compound_elts.push(lisp_compile_qq(sub, depth));
            }
        }
        if (compound_elts.length > 0) 
            compounds.push(make_compound(compound_elts));
        return append_compounds(compounds);
    }

    function is_quasiquote(op)
    {
        return op && (op.formt == "symbol") && (op.name == "quasiquote");
    }

    function is_unquote(op)
    {
        return op && (op.formt == "symbol") && (op.name == "unquote");
    }

    function is_unquote_splicing(op)
    {
        return op && (op.formt == "symbol") && (op.name == "unquote-splicing");
    }

    function quasiquote(x, depth)
    {
        return make_compound([quote(symbol("quasiquote")), 
                              lisp_compile_qq(x, depth)]);
    }

    function unquote(x, depth)
    {
        return make_compound([quote(symbol("unquote")), 
                              lisp_compile_qq(x, depth)]);
    }

    function unquote_splicing(x, depth)
    {
        return make_compound([quote(symbol("unquote-splicing")), 
                              lisp_compile_qq(x, depth)]);
    }

    function make_compound(elt_vops)
    {
        return { vopt: "funcall",
                 fun: { vopt: "function", name: "make-compound" },
                 call_site: { pos_args: elt_vops } };
    }

    function append_compounds(elt_vops)
    {
        return { vopt: "funcall",
                 fun: { vopt: "function", name: "append-compounds" },
                 call_site: { pos_args: elt_vops } };
    }

    function symbol(name)
    {
        return new Lisp_symbol_form(name);
    }

    function quote(form)
    {
        return { vopt: "quote", form: form };
    }
}


/*** Virtual Operations ***/

/* Virtual operations (VOPs) are low-level operations that are emitted
   to JavaScript. */

function lisp_emit(vop)
{
    // Emits a VOP to JavaScript.
    lisp_assert_string(vop.vopt, "Bad .vopt", vop);
    var vop_function = lisp_vop_function(vop.vopt);
    lisp_assert_not_null(vop_function, "No VOP emitter function", vop);
    return vop_function(vop);
}

function lisp_vop_function(vopt)
{
    // Returns the VOP emitter function for a VOP, or null.
    return lisp_vop_table[vopt];
}

/**** List of VOPs ****/

var lisp_vop_table = {
    "alien": lisp_emit_vop_alien,
    "bound?": lisp_emit_vop_boundp,
    "eval_when": lisp_emit_vop_eval_when,
    "fbound?": lisp_emit_vop_fboundp,
    "funcall": lisp_emit_vop_funcall,
    "function": lisp_emit_vop_function,
    "if": lisp_emit_vop_if,
    "lambda": lisp_emit_vop_lambda,
    "number": lisp_emit_vop_number,
    "progn": lisp_emit_vop_progn,
    "quote": lisp_emit_vop_quote,
    "ref": lisp_emit_vop_ref,
    "set": lisp_emit_vop_set,
    "set_function": lisp_emit_vop_set_function,
    "set_macro": lisp_emit_vop_set_macro,
    "string": lisp_emit_vop_string,
};

function lisp_should_eval_at_compile_time(vop)
{
    return ((vop.vopt == "set_macro") ||
            ((vop.vopt == "eval_when") &&
             lisp_array_contains(vop.when, "compile")));
}

function lisp_should_eval_at_load_time(vop)
{
    return (!(vop.vopt == "eval_when") ||
            lisp_array_contains(vop.when, "eval"));
}

/* { vopt: "alien", stuff: <stuff> }
   stuff: list of JavaScript strings and VOPs. */
function lisp_emit_vop_alien(vop)
{
    var s = "";
    var stuff = vop.stuff;
    for (var i = 0, len = stuff.length; i < len; i++) {
        var thing = stuff[i];
        if (thing.vopt) {
            s += lisp_emit(thing);
        } else {
            lisp_assert_string(thing);
            s += thing;
        }
    }
    return s;
}

/* { vopt: "bound?", name: <string> } */
function lisp_emit_vop_boundp(vop)
{
    var name = lisp_assert_nonempty_string(vop.name);
    return "(typeof " + lisp_mangle_var(name) + " != \"undefined\")";
}

/* { vopt: "eval_when", when: <situations>, vop: <vop> }
   situations: list of "eval", "compile". */
function lisp_emit_vop_eval_when(vop)
{
    if (lisp_array_contains(vop.when), "eval") {
        return lisp_emit(vop.vop);
    } else {
        return "undefined";
    }
    // when = "compile" needs to be handled by the compiler
}

/* { vopt: "fbound?", name: <string> } */
function lisp_emit_vop_fboundp(vop)
{
    var name = lisp_assert_nonempty_string(vop.name);
    return "(typeof " + lisp_mangle_function(name) + " != \"undefined\")";
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
    if (s != "") {
        var keywords_dict = "lisp_fast_string_dict({ " + s + " })";
    } else {
        var keywords_dict = "null";
    }
    
    var pos_args = call_site.pos_args.map(lisp_emit);
    var args = [ keywords_dict ].concat(pos_args).join(", ");

    return "(" + lisp_emit(fun) + "(" + args + "))";
}

/* Function reference. 
   { vopt: "function", name: <string> }
   name: the name of the function. */
function lisp_emit_vop_function(vop)
{
    var name = lisp_assert_nonempty_string(vop.name, "Bad function", vop);
    return lisp_mangle_function(name);
}

/* { vopt: "if", test: <vop>, consequent: <vop>, alternative: <vop> } */
function lisp_emit_vop_if(vop)
{
    var test = lisp_emit(vop.test);
    var consequent = lisp_emit(vop.consequent);
    var alternative = lisp_emit(vop.alternative);
    return "(lisp_is_true(" + test + ") ? " + consequent + " : " + alternative + ")";
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
            "lisp_arity_min_max(arguments.length, " + js_min + ", " + js_max + "); ";
    }

    // Required arguments type checks
    var check_types = "";
    for (var i = 0, len = req_params.length; i < len; i++) {
        var param = req_params[i];
        if (param.specializer) {
            var name = lisp_mangle_var(param.name);
            var type_name = lisp_mangle_var(param.specializer);
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
            var value = param.init ? lisp_emit(param.init) : "undefined";
            s += "if (arguments.length < " + (i + 1) + ") " + name + " = " + value + "; ";
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
            var init = param.init ? lisp_emit(param.init) : "undefined";
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
        setup_fast_param;

    var body = lisp_emit(vop.body);

    return "(function(" + sig + "){ " + preamble + "return (" + body + "); })";
}

/* Number literal.
   { vopt: "number", n: <string> }
   n: the number in JavaScript syntax. */
function lisp_emit_vop_number(vop)
{
    lisp_assert_nonempty_string(vop.n, "Bad .n", vop);
    lisp_assert_number(eval(vop.n), "Bad number", vop);
    return vop.n;
}

/* Evaluates a number of VOPs in sequence and returns the value of the last.
   { vopt: "progn", vops: <list> } 
   vops: list of VOPs. */
function lisp_emit_vop_progn(vop)
{
    lisp_assert_not_null(vop.vops, "Bad PROGN", vop);
    if (vop.vops.length > 0)
        return "(" + vop.vops.map(lisp_emit).join(", ") + ")";
    else
        return "undefined";
}

/* Evaluates to the form itself.
   { vopt: "quote", form: <form> }
   form: any form. */
function lisp_emit_vop_quote(vop)
{
    return JSON.stringify(vop.form);
}

/* Variable reference.
   { vopt: "ref", name: <string> }
   name: the name of the variable. */
function lisp_emit_vop_ref(vop)
{
    lisp_assert_nonempty_string(vop.name, "Bad variable name", vop);
    return lisp_mangle_var(vop.name);
}

/* Assigns a value to a variable.
   { vopt: "set", name: <string>, value: <vop> }
   name: the name of the variable;
   value: VOP for the value. */
function lisp_emit_vop_set(vop)
{
    var name = lisp_assert_nonempty_string(vop.name, "Bad variable name", vop);
    var value = lisp_emit(vop.value);
    return "(" + lisp_mangle_var(name) + " = " + value + ")";
}

/* Assigns a value to a function.
   { vopt: "set_function", name: <string>, value: <vop> }
   name: the name of the function;
   value: VOP for the value. */
function lisp_emit_vop_set_function(vop)
{
    var name = lisp_assert_nonempty_string(vop.name, "Bad function", vop);
    var value = lisp_assert_not_null(vop.value, "Bad value", vop);
    var assignment = lisp_mangle_function(name) + " = " + lisp_emit(value);
    var name_upper = name.toUpperCase();
    return "(" + assignment + ")";
}

/* { vopt: "set_macro", name: <string>, expander: <vop> }
   name: macro's name;
   expander: VOP for expander function. */
function lisp_emit_vop_set_macro(vop)
{
    var name = lisp_assert_nonempty_string(vop.name);
    var expander = lisp_assert_not_null(vop.expander);
    return "(lisp_set_macro_function(\"" + name + "\", " + lisp_emit(expander) + "))";
}

/* String literal.
   { vopt: "string", s: <string> }
   s: the string in JavaScript syntax. */
function lisp_emit_vop_string(vop)
{
    lisp_assert_string(vop.s, "Bad .s", vop);
    return JSON.stringify(vop.s);
}


/*** Name Mangling ***/

/* Lisp symbols may contain additional characters beyond those
   supported by JavaScript names.  These special characters are
   translated to uppercase characters, which are not allowed in
   CyberLisp symbols. */

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
     ];

function lisp_mangle(name)
{
    lisp_assert_nonempty_string(name, "Bad name", name);
    for (var i = 0, len = lisp_mangle_table.length; i < len; i++) {
        var pair = lisp_mangle_table[i];
        var pattern = new RegExp("\\" + pair[0], "g");
        name = name.replace(pattern, pair[1]);
    }
    return name;
}

/* Additionally, the different namespaces (variable, function, slot,
   method) are prefixed, so they can coexist in their respective
   JavaScript namespace(s). */

function lisp_mangle_var(name)
{
    return "_v_" + lisp_mangle(name);
}

function lisp_mangle_function(name)
{
    return "_f_" + lisp_mangle(name);
}

function lisp_mangle_slot(name)
{
    return "_s_" + lisp_mangle(name);
}

function lisp_mangle_method(name)
{
    return "_m_" + lisp_mangle(name);
}


/*** Utilities ***/

function lisp_set_function(lisp_name, js_function)
{
    eval(lisp_mangle_function(lisp_name) + " = " + js_function);
}

function lisp_set(lisp_name, js_object)
{
    eval(lisp_mangle_var(lisp_name) + " = " + js_object);
}

function lisp_show(obj)
{
    if (obj === null) {
        return "null";
    } else if (typeof obj == "function") {
        return "#<function>";
    } else {
        return JSON.stringify(obj);
    }
}

function lisp_array_contains(array, elt)
{
    for (var i in array) {
        if (array[i] == elt) return true;
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

function lisp_error(message, arg)
{
    throw Error(message + ": " + lisp_show(arg));
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
    lisp_assert(typeof value == "number", message, arg);
    return value;
}

function lisp_assert_string(value, message, arg)
{
    lisp_assert(typeof value == "string", message, arg);
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
    lisp_assert(typeof value == "function", message, arg);
    return value;
}

function lisp_assert_symbol_form(value, message, arg)
{
    lisp_assert(typeof value == "object", message, arg);
    lisp_assert(value.formt == "symbol", message, arg);
    lisp_assert_nonempty_string(value.name, message, arg);
    return value;
}

function lisp_assert_compound_form(value, message, arg)
{
    lisp_assert(typeof value == "object", message, arg);
    lisp_assert(value.formt == "compound", message, arg);
    lisp_assert_not_null(value.elts, message, arg);
    lisp_assert_not_null(value.elts.length, message, arg);
    return value;
}

/*** Built-in form manipulation functions ***/

/* These are provided so that early, pre-generics macros can inspect
   and manipulate forms. */

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
        if (elt.formt == "compound") {
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

/*** Built-in string dictionaries ***/

/* This is the type of dictionaries used to hold keyword arguments,
   i.e. the one a function with an `&all-keys' signature keyword
   receives.  They're called string dictionaries because their keys
   can only be strings.
   
   By convention, all keys are mangled (prefixed with "%", but not
   transformed like, say, variables), so we never get in conflict with
   JS's special names, such as "prototype". */

function lisp_mangle_string_dict_key(name)
{
    lisp_assert_string(name);
    return "%" + name;
}

function lisp_is_string_dict_key(k)
{
    return k[0] == "%";
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

/*** Export classes and built-in functions ***/

lisp_set("<boolean>", "Boolean.prototype");
lisp_set("<function>", "Function.prototype");
lisp_set("<compound-form>", "Lisp_compound_form.prototype");
lisp_set("<list>", "Array.prototype");
lisp_set("<number-form>", "Lisp_number_form.prototype");
lisp_set("<number>", "Number.prototype");
lisp_set("<string-dict>", "Lisp_string_dict.prototype");
lisp_set("<string-form>", "Lisp_string_form.prototype");
lisp_set("<string>", "String.prototype");
lisp_set("<symbol-form>", "Lisp_symbol_form.prototype");

lisp_set_function("append-compounds", "lisp_bif_append_compounds");
lisp_set_function("compound-apply", "lisp_bif_compound_apply");
lisp_set_function("compound-elt", "lisp_bif_compound_elt");
lisp_set_function("compound-elts", "lisp_bif_compound_elts");
lisp_set_function("compound-map", "lisp_bif_compound_map");
lisp_set_function("compound-slice", "lisp_bif_compound_slice");
lisp_set_function("make-compound", "lisp_bif_make_compound");
lisp_set_function("string-dict-get", "lisp_bif_string_dict_get");
lisp_set_function("string-dict-has-key", "lisp_bif_string_dict_has_key");
lisp_set_function("string-dict-put", "lisp_bif_string_dict_put");
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

function lisp_bif_throw(_key_, exception)
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

function lisp_bif_call_unwind_protected(_key_, protected_fun, cleanup_fun)
{
    try {
        return protected_fun(null);
    } finally {
        cleanup_fun(null);
    }
}

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
    return a === b;
}

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

function lisp_bif_make_class(_key_)
{
    return {};
}

function lisp_bif_set_superclass(_key_, clsA, clsB)
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

function lisp_bif_get_method(_key_, obj, name)
{
    return obj[lisp_mangle_method(name)];
}

function lisp_bif_set_method(_key_, obj, name, fun)
{
    return obj[lisp_mangle_method(name)] = fun;
}

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

function lisp_bif_is_typename(_key_, string)
{
    return lisp_is_type_name(string);
}

function lisp_bif_apply(_key_, fun, args, keys)
{
    return fun.apply(null, [ keys ].concat(args));
}

function lisp_is_true(obj) // T
{
    return (obj !== false) && (obj !== undefined);
}

function lisp_bif_add(_key_, a, b)
{
    return a + b;
}

function lisp_bif_sub(_key_, a, b)
{
    return a - b;
}

function lisp_bif_mult(_key_, a, b)
{
    return a * b;
}

function lisp_bif_div(_key_, a, b)
{
    return a / b;
}

function lisp_bif_lt(_key_, a, b)
{
    return a < b;
}

function lisp_bif_call_while(_key_, test_fun, body_fun)
{
    while(test_fun(null)) {
        body_fun(null);
    }
}

function lisp_bif_fast_apply(_key_, fun, _arguments)
{
    return fun.apply(null, _arguments);
}

var lisp_gensym_counter = 0;

function lisp_bif_gensym(_key_)
{
    lisp_gensym_counter++;
    return new Lisp_symbol_form("%%g-" + lisp_gensym_counter);
}

lisp_set("true", "true");
lisp_set("false", "false");
lisp_set("null", "undefined");

lisp_set_function("*", "lisp_bif_mult");
lisp_set_function("+", "lisp_bif_add");
lisp_set_function("-", "lisp_bif_sub");
lisp_set_function("/", "lisp_bif_div");
lisp_set_function("<", "lisp_bif_lt");
lisp_set_function("apply", "lisp_bif_apply");
lisp_set_function("bind-handlers", "lisp_bif_bind_handlers");
lisp_set_function("call-unwind-protected", "lisp_bif_call_unwind_protected");
lisp_set_function("call-with-escape-function", "lisp_bif_call_with_escape_function");
lisp_set_function("call-while", "lisp_bif_call_while");
lisp_set_function("compound?", "lisp_bif_compoundp");
lisp_set_function("eq", "lisp_bif_eq");
lisp_set_function("fast-apply", "lisp_bif_fast_apply");
lisp_set_function("get-method", "lisp_bif_get_method");
lisp_set_function("gensym", "lisp_bif_gensym");
lisp_set_function("has-slot", "lisp_bif_has_slot");
lisp_set_function("list", "lisp_bif_list");
lisp_set_function("list-add", "lisp_bif_list_add");
lisp_set_function("list-elt", "lisp_bif_list_elt");
lisp_set_function("list-len", "lisp_bif_list_len");
lisp_set_function("macroexpand-1", "lisp_bif_macroexpand_1");
lisp_set_function("make", "lisp_bif_make");
lisp_set_function("make-class", "lisp_bif_make_class");
lisp_set_function("print", "lisp_bif_print");
lisp_set_function("set-method", "lisp_bif_set_method");
lisp_set_function("set-slot", "lisp_bif_set_slot");
lisp_set_function("set-superclass", "lisp_bif_set_superclass");
lisp_set_function("slot", "lisp_bif_slot");
lisp_set_function("string-concat", "lisp_bif_string_concat");
lisp_set_function("string-to-form", "lisp_bif_string_to_form");
lisp_set_function("string-to-symbol", "lisp_bif_string_to_symbol");
lisp_set_function("subtype?", "lisp_bif_subtypep");
lisp_set_function("symbol-name", "lisp_bif_symbol_name");
lisp_set_function("symbol?", "lisp_bif_symbolp");
lisp_set_function("throw", "lisp_bif_throw");
lisp_set_function("type-of", "lisp_bif_type_of");
