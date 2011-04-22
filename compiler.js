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
   
   1.2   --> { formt: "number", sign: "+", integral_digits: "1",
               fractional_digits: ".2" }
   "foo" --> { formt: "string", s: "foo" }
   foo   --> { formt: "symbol", name: "foo" }
   (foo) --> { formt: "compound", 
               elts: [ { formt: "symbol", name: "foo" } ] } 
   ; line comments are ignored
*/

var lisp_expression_syntax =
    function(input) { return lisp_expression_syntax(input); }; // forward decl.

/**** Comments ****/

var lisp_line_terminator = choice(ch("\r"), ch("\n"));

var lisp_line_comment_syntax =
    action(join_action(sequence(";",
                                repeat0(negate(lisp_line_terminator)),
                                optional(lisp_line_terminator)),
                       ""),
           lisp_line_comment_action);

function lisp_line_comment_action(ast)
{
    return new Lisp_comment_form(ast);
}

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
    var sign = ast[0] ? ast[0] : "+";
    var integral_digits = ast[1];
    var fractional_digits = ast[2] || "";
    return new Lisp_number_form(sign, integral_digits, fractional_digits);
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
    choice("-", "&", ":", ".", "=", ">","<", "%", "+", "?", "/", "*", "#");

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
    var forms = ast[1];
    return new Lisp_compound_form(lisp_remove_comment_forms(forms));
}

/**** Inline JavaScript ****/

var lisp_native_escape =
    action(sequence("~", lisp_expression_syntax),
           lisp_native_escape_action);

var lisp_native_syntax =
    action(sequence("#{",
                    repeat1(choice(lisp_native_escape,
                                   action(choice(negate("#"),
                                                 join_action(sequence("#",
                                                                      not("}")),
                                                             "")),
                                          lisp_native_snippet_action))),
                    "#}"),
           lisp_native_action);

function lisp_native_escape_action(ast)
{
    return ast[1];
}

function lisp_native_snippet_action(ast)
{
    return new Lisp_compound_form([ new Lisp_symbol_form("native-snippet"),
                                    new Lisp_string_form(ast) ]);
}

function lisp_native_action(ast)
{
    return new Lisp_compound_form([ new Lisp_symbol_form("native") ]
                                  .concat(ast[1]));
}

/**** Misc shortcuts ****/

var lisp_quote_syntax =
    action(sequence("#'", lisp_expression_syntax),
           lisp_shortcut_syntax_action("%%quote"));

var lisp_quasiquote_syntax =
    action(sequence("#`", lisp_expression_syntax),
           lisp_shortcut_syntax_action("%%quasiquote"));

var lisp_unquote_syntax =
    action(sequence(",", lisp_expression_syntax),
           lisp_shortcut_syntax_action("%%unquote"));

var lisp_unquote_splicing_syntax =
    action(sequence(",@", lisp_expression_syntax),
           lisp_shortcut_syntax_action("%%unquote-splicing"));

var lisp_function_syntax =
    action(sequence("$", lisp_symbol_syntax),
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
    whitespace(choice(lisp_line_comment_syntax,
                      lisp_number_syntax,
                      lisp_string_syntax,
                      lisp_compound_syntax,
                      lisp_native_syntax,
                      lisp_quote_syntax,
                      lisp_quasiquote_syntax,
                      lisp_unquote_syntax,
                      lisp_unquote_splicing_syntax,
                      lisp_function_syntax,
                      lisp_symbol_syntax));

var lisp_program_syntax =
    whitespace(repeat1(lisp_expression_syntax));

function lisp_read(string)
{
    var result = lisp_program_syntax(ps(string));
    if (result.ast) return lisp_remove_comment_forms(result.ast);
    else lisp_error("Reader error", string);
}

function lisp_remove_comment_forms(forms)
{
    var res = [];
    for (var i = 0, len = forms.length; i < len; i++) {
        var form = forms[i];
        if (form.formt !== "comment")
            res.push(form);
    }
    return res;

}


/*** Compilation and Evaluation ***/

function lisp_eval(forms)
{
    var vops = forms.map(lisp_compile);
    var js = vops.map(lisp_emit).join(", ");
    return eval(js);
}

function lisp_eval_string(string)
{
    return lisp_eval(lisp_read(string));
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

/* Special forms are built-in forms with special evaluation rules
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
    "%%set": lisp_compile_special_set,
    "%%string-form": lisp_compile_special_string_form,
    "%%symbol-form": lisp_compile_special_symbol_form
};

/* VOPs (virtual operations) correspond roughly to special forms. */

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
    "set": lisp_emit_vop_set,
    "string": lisp_emit_vop_string,
    "string-form": lisp_emit_vop_string_form,
    "symbol-form": lisp_emit_vop_symbol_form
};

function lisp_special_function(name)
{
    lisp_assert_nonempty_string(name, "Bad special form name", name);
    return lisp_specials_table[name];
}

/**** List of special forms ****/

/* Contains a mixture of ordinary forms and NATIVE-SNIPPET forms.
   (%%native &rest forms) */
function lisp_compile_special_native(form) {
    return { vopt: "native",
             stuff: form.elts.slice(1).map(lisp_compile) };
}

/* A piece of JS text to directly emit.  Must appear inside NATIVE.
   (%%native-snippet text) */
function lisp_compile_special_native_snippet(form) {
    return{ vopt: "native-snippet",
            text: form.elts[1].s };
}

/* Returns true if `name' is defined (unlike Common Lisp's `boundp',
   name is not evaluated).  
   (%%defined? identifier) */
function lisp_compile_special_definedp(form)
{
    var name_form = lisp_assert_not_null(form.elts[1]);
    return { vopt: "defined?",
             name: lisp_compile(name_form) };
}

/* Assigns the `value' to the global variable named `name'.
   (%%defparameter name value) */
function lisp_compile_special_defparameter(form)
{
    var name_form = lisp_assert_not_null(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "set", 
             name: lisp_compile(name_form),
             value: lisp_compile(value_form) };
}

/* Executes body-form during compilation, not during evaluation.
   (%%eval-when-compile body-form) */
function lisp_compile_special_eval_when_compile(form)
{
    var body_form = lisp_assert_not_null(form.elts[1]);
    eval(lisp_emit(lisp_compile(body_form)));
    return { vopt: "identifier", name: "#f", namespace: "variable" };
}

/* Registers a macro expander function.  An expander function takes a
   form as input and must return a form.
   (%%defsyntax name expander-function) */
function lisp_compile_special_defsyntax(form)
{
    var name_form = lisp_assert_symbol_form(form.elts[1]);
    var expander_form = lisp_assert_not_null(form.elts[2]);
    lisp_set_macro_function(name_form.name,
                            eval(lisp_emit(lisp_compile(expander_form))));
    return { vopt: "identifier", name: "#f", namespace: "variable" };
}

/* Calls a function passed as argument.
   (%%funcall fun &rest args &all-keys keys) => result */
function lisp_compile_special_funcall(form)
{
    var fun = lisp_assert_not_null(form.elts[1]);
    var call_site = lisp_assert_not_null(form.elts.slice(2));
    return { vopt: "funcall",
             fun: lisp_compile(fun),
             call_site: lisp_compile_call_site(call_site) };
}

/* Accesses a binding.  Name and namespace are not evaluated.
   (%%identifier name namespace) */
function lisp_compile_special_identifier(form) {
    var name = lisp_assert_symbol_form(form.elts[1],
                                       "Bad identifier name", form);
    var namespace = lisp_assert_symbol_form(form.elts[2],
                                            "Bad identifier namespace", form);
    return { vopt: "identifier",
             name: name.name,
             namespace: namespace.name };
}

/* (%%symbol-form symbol) */
function lisp_compile_special_symbol_form(form) {
    var symbol = lisp_assert_symbol_form(form.elts[1], "Bad symbol");
    return { vopt: "symbol-form",
             name: symbol.name };
}

/* (%%number-form form) */
function lisp_compile_special_number_form(form) {
    var numform = form.elts[1];
    return { vopt: "number-form",
             sign: numform.sign, 
             integral_digits: numform.integral_digits,
             fractional_digits: numform.fractional_digits };
}

/* (%%string-form form) */
function lisp_compile_special_string_form(form) {
    var strform = form.elts[1];
    return { vopt: "string-form",
             s: strform.s };
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
   (%%progn &rest forms) */
function lisp_compile_special_progn(form)
{
    var forms = form.elts.slice(1);
    return { vopt: "progn", 
             vops: forms.map(lisp_compile) };
}

/* Updates the value of a binding.
   (%%set name value) */
function lisp_compile_special_set(form)
{
    var name_form = lisp_assert_not_null(form.elts[1]);
    var value_form = lisp_assert_not_null(form.elts[2]);
    return { vopt: "set",
             name: lisp_compile(name_form),
             value: lisp_compile(value_form) };
}

/* See heading ``Quasiquotation''.
   (%%quasiquote form) */
function lisp_compile_special_quasiquote(form)
{
    var quasiquoted = lisp_assert_not_null(form.elts[1]);
    return lisp_compile(lisp_qq(quasiquoted, 0));
}

/* (%%quote form) */
function lisp_compile_special_quote(form)
{
    return lisp_compile_special_quasiquote(form);
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
            (string[0] === "<") &&
            (string[string.length - 1] === ">"));
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
                   ((cur === opt) || (cur === key))) {
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

/* A quasiquote form is transformed into code that, when evaluated,
   produces a form.  No, really.

   Except for multi-argument unquotes, which are not supported, this
   algorithm should produce the same results as the one in Appendix B
   of Alan Bawden's paper "Quasiquotation in LISP", 1999. */

function lisp_qq(form, depth)
{
    if (depth < 0) 
        lisp_error("Negative quasiquotation nesting depth", x);

    switch(form.formt) {
    case "number":
        return new Lisp_compound_form([ new Lisp_symbol_form("%%number-form"), form ]);
    case "string":
        return new Lisp_compound_form([ new Lisp_symbol_form("%%string-form"), form ]);
    case "symbol":
        return new Lisp_compound_form([ new Lisp_symbol_form("%%symbol-form"), form ]);
    case "compound":
        return lisp_qq_compound(form, depth);
    }

    lisp_error("Bad quasiquoted form", form);    
}

function lisp_qq_compound(form, depth)
{
    var op = form.elts[0];
    if (op) {
        if (is_unquote(op)) {
            if (depth === 0) {
                return form.elts[1];
            } else {
                return unquote(form.elts[1], depth - 1);
            }
        } else if (is_quasiquote(op)) {
            return quasiquote(form.elts[1], depth + 1);
        } else {
            return qq_compound(form, depth);
        }
    } else {
        return make_compound([]);
    }

    function qq_compound(form, depth)
    {
        var compounds = [], compound_elts = [];
        for (var i = 0, len = form.elts.length; i < len; i++) {
            var sub = form.elts[i];
            if ((sub.formt === "compound") && is_unquote_splicing(sub.elts[0])) {
                compounds.push(make_compound(compound_elts));
                compound_elts = [];
                if (depth === 0) {
                    compounds.push(sub.elts[1]);
                } else {
                    compounds.push(unquote_splicing(sub.elts[1], depth - 1));
                }
            } else {
                compound_elts.push(lisp_qq(sub, depth));
            }
        }
        if (compound_elts.length > 0) 
            compounds.push(make_compound(compound_elts));
        return append_compounds(compounds);
    }

    function is_quasiquote(op)
    {
        return op && (op.formt === "symbol") && (op.name === "%%quasiquote");
    }

    function is_unquote(op)
    {
        return op && (op.formt === "symbol") && (op.name === "%%unquote");
    }

    function is_unquote_splicing(op)
    {
        return op && (op.formt === "symbol") && (op.name === "%%unquote-splicing");
    }

    function quasiquote(x, depth)
    {
        return make_compound([new Lisp_symbol_form("%%quasiquote"),
                              lisp_qq(x, depth)]);
    }

    function unquote(x, depth)
    {
        return make_compound([new Lisp_symbol_form("%%unquote"),
                              lisp_qq(x, depth)]);
    }

    function unquote_splicing(x, depth)
    {
        return make_compound([new Lisp_symbol_form("%%unquote-splicing"),
                              lisp_qq(x, depth)]);
    }

    function make_compound(elts)
    {
        return new Lisp_compound_form([new Lisp_symbol_form("make-compound")].concat(elts));
    }

    function append_compounds(elts)
    {
        return new Lisp_compound_form([new Lisp_symbol_form("append-compounds")].concat(elts));
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

/* { vopt: "native", stuff: <vops> } */
function lisp_emit_vop_native(vop)
{
    // Note that subforms of NATIVE are not separated by commas, as is
    // usually the case.
    return vop.stuff.map(lisp_emit).join("");
}

/* { vopt: "native-snippet", text: <vops> } */
function lisp_emit_vop_native_snippet(vop)
{
    return vop.text;
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
    var args = JSON.stringify(vop.name) + ", " + JSON.stringify(vop.namespace);
    return "(typeof " + mname + " !== \"undefined\" ? " + mname + " : lisp_undefined_identifier(" + args + "))";
}

/* Assigns a value to a variable.
   { vopt: "set", name: <vop>, value: <vop> }
   name: the "identifier" VOP of the variable;
   value: VOP for the value. */
function lisp_emit_vop_set(vop)
{
    var value = lisp_emit(vop.value);
    var mname = lisp_mangle(vop.name.name, vop.name.namespace);
    return "(" + mname + " = " + value + ")";
}

/* { vopt: "defined?", name: <vop> } */
function lisp_emit_vop_definedp(vop)
{
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
    return "(new Lisp_number_form("+JSON.stringify(vop.sign)+","+JSON.stringify(vop.integral_digits)+","+JSON.stringify(vop.fractional_digits)+"))";
}

/* String literal.
   { vopt: "string", s: <string> }
   s: the string in JavaScript syntax. */
function lisp_emit_vop_string(vop)
{
    lisp_assert_string(vop.s, "Bad .s", vop);
    return JSON.stringify(vop.s);
}

/* String form.
   { vopt: "string-form", s: <string> } */
function lisp_emit_vop_string_form(vop)
{
    lisp_assert_string(vop.s, "Bad .s", vop);
    return "(new Lisp_string_form(" + JSON.stringify(vop.s) + "))";
}

/* Symbol form.
   { vopt: "symbol-form", name: <string> } */
function lisp_emit_vop_symbol_form(vop)
{
    lisp_assert_string(vop.name, "Bad .name", vop);
    return "(new Lisp_symbol_form(" + JSON.stringify(vop.name) + "))";
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
            var value = param.init ? lisp_emit(param.init) : "undefined";
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
