// Transforms forms into intermediate representation.

function lispDecode(form) {
    switch (form.formt) {
    case "string": return lispDecodeString(form);
    case "symbol": return lispDecodeSymbol(form);
    case "compound": return lispDecodeCompound(form);
    }
    throw "Unrecognized form " + uneval(form);
}

var lispDecodeCompoundFunctionsTable = {
    "defclass": lispDecodeDefclass,
    "def": lispDecodeDef,
    "defun": lispDecodeDefun,
    "defvar": lispDecodeDefvar,
    "apply": lispDecodeApply,
    "function": lispDecodeFunction,
    "lambda": lispDecodeLambda,
    "new": lispDecodeNew,
    "set": lispDecodeSet,
    "finally": lispDecodeFinally,
    "throw": lispDecodeThrow,
    "with-handlers": lispDecodeWithHandlers,
    "call/ec": lispDecodeCallEC,
    "slot-value": lispDecodeSlotValue,
    "set-slot-value": lispDecodeSetSlotValue,
    "native": lispDecodeNative,
    "comment": lispDecodeNoop,
    "quasiquote": lispDecodeQuasiquote,
    "defmacro": lispDecodeDefmacro,
    "progn": lispDecodeProgn,
    // should be macros, but cannot, because they're used pre-macros
    "let": lispDecodeLet,
    // should be macros:
    "bind": lispDecodeBind,
};

var lispMacrosTable = {};

function lispDecodeString(form) {
    return { irt: "string", s: form.s };
}

function lispDecodeSymbol(form) {
    return { irt: "var", name: form.name };
}

function lispDecodeCompound(form) {
    if (form.elts.length == 0) throw "Empty form";
    var op = form.elts[0];
    switch (op.formt) {
    case "symbol":
        var decodeCompoundFunction = lispDecodeCompoundFunctionsTable[op.name];
        if (decodeCompoundFunction) {
            // special form
            return decodeCompoundFunction(form);
        } else if (lispIsTypeName(op.name)) {
            // type name as first element
            return lispDecodeCompoundFormWithTypeName(form);
        } else {
            var macro = lispMacrosTable[op.name];
            if (macro) {
                // macro
                return lispDecodeMacroApplication(macro, form);
            } else {
                // user-defined function
                return lispDecodeFunctionApplication(form);
            }
        }
    case "compound":
        if (lispIsTypeName(op.elts[0].name)) {
            // type-expr as first element
            return lispDecodeCompoundFormWithTypeExpr(op);
        }
    }
    throw "Malformed compound form " + uneval(form);
}

function lispDecodeFunctionApplication(form) {
    return { irt: "apply", 
             fun: { irt: "function", name: form.elts[0].name }, 
             args: form.elts.splice(1).map(lispDecode) };
}

function lispDecodeMacroApplication(macro, form) {
    var naturalForm = lispNaturalizeForm(form);
    print(";*  PRE: " + uneval(naturalForm));
    var naturalTransformedForm = macro(naturalForm);
    print(";*TRANS: " + uneval(naturalTransformedForm));
    var transformedForm = lispDenaturalizeForm(naturalTransformedForm);
    print(";* POST: " + uneval(transformedForm));
    return lispDecode(transformedForm);
}

// Needed because <T> parses as a symbol, and not as a type
// expression, but we still want (<T>) to create a new <T> instance.
function lispDecodeCompoundFormWithTypeName(form) {
    return { irt: "make-instance", "class": lispDecode(form.elts[0]) };
}

// Type expression:
//
// (<t> t ((.slot1 val1) ... (.slotN valN))) === <t t .slot1 val1 ... .slotN valN>
// 0    1 2 \               /
//           \             /
//            destructForms
function lispDecodeCompoundFormWithTypeExpr(form) {
    // Turns a destruct form of a type expression into a setter call.
    function decodeDestructFormForNewInstance(destructForm) {
        return { irt: "apply",
                 fun: { irt: "function", name: lispSetterName(destructForm.elts[0].name) },
                 args: [ { irt: "var", name: "--lisp-class"}, lispDecode(destructForm.elts[1]) ] }
    }
    // Create new instance, run slot setters on it, and return it.
    return lispDecodeOnce("--lisp-class", { irt: "make-instance", 
                                                 "class": { irt: "var", name: form.elts[0].name } },
                          { irt: "progn",
                            exprs: form.elts[2].elts.map(decodeDestructFormForNewInstance)
                                   .concat({ irt: "var", name: "--lisp-class"}) }); // return class
}

function lispSlotGetterName(memberName) {
    return "." + memberName;
}

function lispCleanSlotGetterName(slotGetterName) {
    return slotGetterName.slice(1);
}

function lispSetterName(getterName) {
    return getterName + "-setter";
}

// Lambda list parsing

function lispIsTypeName(name) {
    return (name[0] == "<") && (name[name.length - 1] == ">");
}

function lispCleanTypeName(name) {
    return name.slice(1, name.length - 1);
}

// (lambda (a <b>) ...) === (lambda (a (<b> b ())) ...)
//         ^^^^^^^
//            \
//             \---> [ { name: "a" }, { name: "b", type: "b" } ]
function lispDecodeLambdaList(llForm) {
    return llForm.elts.map(function(paramForm) {
            switch (paramForm.formt) {
            case "symbol":
                if (lispIsTypeName(paramForm.name))
                    return { "type": paramForm.name, "name": lispCleanTypeName(paramForm.name) };
                else
                    return { "type": null, "name": paramForm.name };
            case "compound":
                var name = paramForm.elts[1].name;
                return { "type": paramForm.elts[0].name,
                         "name": paramForm.elts[1].name };
            case "string":
                throw "String in lambda list " + llForm;
            }
        });
}

// (lambda (<person .name .age> ...) ...)
// ===
// (lambda ((<person> person ((.name) (.age))) ...) ...)
// |         0        1      2
// |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = paramForm;   paramName = paramForm[1]
// |                                  
// |                          ^^^^^^^ ^^^^^^   = destructForm(s)
// |
// \--> destructs = [ { name: "name", value: ``IR for (.name person)'' },
//                    { name: "age",  value: ``IR for (.age person)'' } ]
//
// Further down the line, the destructs get embedded in a lambda body,
// and add the names "name" and "age" to the function's lexical
// environment.  They are a generic, IR-driven mechanism for
// extracting additional information from a function's arguments.
// They could also be used to add list destructuring, for example.
function lispDecodeLambdaListDestructs(llForm) {
    var destructs = [];
    llForm.elts.map(function(paramForm) {
            if (paramForm.formt == "compound") {
                var paramName = paramForm.elts[1].name;
                if (!paramForm.elts[2]) return;
                paramForm.elts[2].elts.map(function(destructForm) {
                        var slotName = destructForm.elts[0].name;
                        destructs.push({ name: lispCleanSlotGetterName(slotName),
                                         value: { irt: "apply",
                                                  fun: { irt: "function", name: slotName },
                                                  args: [ { irt: "var", name: paramName } ] } });
                    });
            }
        });
    return destructs;
}

function lispDecodeLambda(form) {
    var ll = lispDecodeLambdaList(form.elts[1]);
    var body_ir = lispDecode(form.elts[2]);
    var destructs = lispDecodeLambdaListDestructs(form.elts[1]);
    return { irt: "lambda", req_params: ll, body: body_ir, destructs: destructs };
}

function lispDecodeApply(form) {
    var fun_ir = lispDecode(form.elts[1]);
    var arg_irs = form.elts.slice(2).map(lispDecode);
    return { irt: "apply", fun: fun_ir, args: arg_irs };
}

function lispDecodeFunction(form) {
    var funName = form.elts[1].name;
    return { irt: "function", name: funName };
}

function lispDecodeDefun(form) {
    var name = form.elts[1].name;
    var ll = lispDecodeLambdaList(form.elts[2]);
    var lambdaBody = lispDecodeImplicitProgn(form.elts.slice(3));
    var destructs = lispDecodeLambdaListDestructs(form.elts[2]);
    var lambda_ir = { irt: "lambda", req_params: ll, body: lambdaBody, destructs: destructs };
    return { irt: "defun", name: name, lambda: lambda_ir };
}

function lispDecodeDefvar(form) {
    var name = form.elts[1].name;
    var value_ir = lispDecode(form.elts[2]);
    return { irt: "defvar", name: name, value: value_ir };
}

// (defclass <c> a b)
// ===
// (progn
//   (defvar <c> (make-class))
//   (def .a (<c>) (slot-value c a))
//   (def .b (<c>) (slot-value c b))
//   (def .a-setter (<c> v) (set-slot-value c a v))
//   (def .b-setter (<c> v) (set-slot-value c b v))
//   <c>)
function lispDecodeDefclass(form) {
    var cls_name = form.elts[1].name;
    var member_names = form.elts.slice(2).map(function (member_form) { return member_form.name; });
    var cls_ir = { irt: "var", name: cls_name };

    var member_getters = member_names.map(function(member_name) {
            return lispDefineMethodAndGenericIR(cls_ir,
                                                lispSlotGetterName(member_name),
                                                [ { name: "--lisp-this", type: cls_name } ],
                                                [ { name: "--lisp-this" } ],
                                                { irt: "get-slot",
                                                  obj: { irt: "var", name: "--lisp-this" },
                                                  slotName: member_name },
                                                []);
        });

    var member_setters = member_names.map(function(member_name) {
            return lispDefineMethodAndGenericIR(cls_ir,
                                                lispSetterName(lispSlotGetterName(member_name)),
                                                [ { name: "--lisp-this", type: cls_name },
                                                  { name: "--lisp-new-value" } ],
                                                [ { name: "--lisp-this" },
                                                  { name: "--lisp-new-value" } ],
                                                { irt: "set-slot",
                                                  obj: { irt: "var", name: "--lisp-this" },
                                                  slotName: member_name,
                                                  value: { irt: "var", name: "--lisp-new-value" } },
                                                []);
        });

    var class_defvar = { irt: "defvar",
                         name: cls_name,
                         value: { irt: "make-class", name: cls_name, member_names: member_names } };

    return { irt: "progn",
            exprs: [ class_defvar ].concat(member_getters,
                                           member_setters,
                                           [ { irt: "var", name: cls_name } ]) };
}

function lispDecodeDef(form) {
    var name = form.elts[1].name;
    var ll = lispDecodeLambdaList(form.elts[2]);
    var destructs = lispDecodeLambdaListDestructs(form.elts[2]);
    var cls_ir = { irt: "var", name: ll[0].type };
    var generic_ll = ll.map(function(param) { return { name: param.name }; });
    var body = lispDecodeImplicitProgn(form.elts.slice(3));
    return lispDefineMethodAndGenericIR(cls_ir, name, ll, generic_ll, body, destructs);
}

function lispDefineMethodAndGenericIR(class_ir, name, method_ll, generic_ll, body, destructs) {
    return { irt: "progn",
             exprs: [ { irt: "set-method",
                        name: name,
                        "class": class_ir,
                        lambda: { irt: "lambda", req_params: method_ll, body: body, destructs: destructs } },
                      { irt: "defun",
                        name: name,
                        lambda: { irt: "lambda",
                                  req_params: generic_ll,
                                  body: { irt: "invoke-method",
                                          name: name,
                                          params: generic_ll.map(function(param) {
                                                  return { irt: "var", name: param.name } }) } } } ] };
}

function lispDecodeImplicitProgn(forms) {
    return { irt: "progn", exprs: forms.map(lispDecode) };
}

function lispDecodeNew(form) {
    var cls_ir = lispDecode(form.elts[1]);
    return { irt: "make-instance", "class": cls_ir };
}

function lispDecodeSet(form) {
    var place = form.elts[1];
    switch (place.formt) {
    case "symbol":
        return { irt: "set", name: place.name, value: lispDecode(form.elts[2]) };
    case "compound":
        // (set (getter args...) value) --> (getter-setter args... value)
        var getter = place.elts[0].name;
        var args = place.elts.slice(1).concat(form.elts[2]);
        return { irt: "apply", 
                 fun: { irt: "function", name: lispSetterName(getter) }, 
                 args: args.map(lispDecode) };
    }
    throw "Illegal set form " + uneval(form);
}

function lispDecodeFinally(form) {
    var protected_ir = lispDecode(form.elts[1]);
    var cleanup_ir = lispDecode(form.elts[2]);
    return { irt: "unwind-protect", "protected": protected_ir, cleanup: cleanup_ir };
}

function lispDecodeThrow(form) {
    var exception_ir = lispDecode(form.elts[1]);
    return { irt: "throw", exception: exception_ir };
}

function lispDecodeWithHandlers(form) {
    var handlers = form.elts[1].elts.map(function(handler) {
            return { "class": lispDecode(handler.elts[0]), "function": lispDecode(handler.elts[1]) };
        });
    return { irt: "bind-handlers", body: lispDecode(form.elts[2]), handlers: handlers };
}

function lispDecodeCallEC(form) {
    return { irt: "call-with-escape-continuation", fun: lispDecode(form.elts[1]) };
}

function lispDecodeBind(form) {
    return { irt: "bind",
             body: lispDecode(form.elts[2]),
             bindings: form.elts[1].elts.map(function(b) { return [ b.elts[0].name, lispDecode(b.elts[1]) ]; }) };
}

function lispDecodeSlotValue(form) {
    return { irt: "get-slot", obj: lispDecode(form.elts[1]), slotName: form.elts[2].name };
}

function lispDecodeSetSlotValue(form) {
    return { irt: "set-slot", obj: lispDecode(form.elts[1]), slotName: form.elts[2].name,
             value: lispDecode(form.elts[3]) };
}

// Inline JavaScript

var LispNativeEscape = repeat0(sequence(optional(join_action(repeat0(negate("~")), "")),
                                        optional(sequence("~(", LispForm, ")"))));

function lispDecodeNative(form) {
    var code = form.elts[1].s;
    var parser = LispNativeEscape(ps(code));
    var snippets = [];
    parser.ast.map(function(ast) {
            snippets.push(ast[0]);
            if (ast[1])
                snippets.push(lispDecode(ast[1][1]));
        });
    return { irt: "native", snippets: snippets };
}

function lispDecodeNoop(form) {
    return { irt: "noop" };
}

// Quasiquotation

// A QUASIQUOTE form produces code that, when evaluated, produces a form.
//
// (A frog jumps into the pond.)
//
// The --symbol, --string, and --compound IRs produce "natural"
// symbols, strings, and compounds, respectively.  They are called
// natural because they live in the Lisp world -- they mimick the
// compiler's data structures using Lisp classes and are provided so
// that macro writers can access and produce code.  Once a macro is
// done, the natural form it has produced is converted to a compiler
// form again.
function lispDecodeQuasiquote(form) {
    return lispDecodeQuasiquotedForm(form.elts[1], 0);
}

//                     (quasiquote form)
function lispDecodeQuasiquotedForm(form, level) {
    if (level < 0) throw "Ouch, negative quasiquote nesting level " + level + " " + uneval(form);
    switch(form.formt) {
    case "symbol":   // (quasiquote x)
        return { irt: "--symbol", name: form.name };
    case "string":   // (quasiquote "foo")
        return { irt: "--string", s: form.s };
    case "compound": // (quasiquote (...))
        return lispDecodeQuasiquotedCompoundForm(form, level);
    }
    throw "Illegal quasiquoted form " + uneval(form);
}

function lispDecodeQuasiquotedCompoundForm(form, level) {
    var op = form.elts[0];
    if ((op.formt == "symbol") && (op.name == "unquote")) { // (quasiquote (unquote ...))
        if (level == 0) {
            return lispDecode(form.elts[1]); // <---- unquoted code
        } else {
            return lispDecodeQuasiquotedForm(form.elts[1], level - 1);
        }
    } else if ((op.formt == "symbol") && (op.name == "quasiquote")) { // (quasiquote (quasiquote ...))
        return lispDecodeQuasiquotedForm(form.elts[1], level + 1);
    } else { // (quasiquote (sub-form1 sub-form2 ... sub-formN))
        var exprs = [];
        var currentCompound = { irt: "--compound", elts: [] };
        for (var i in form.elts) {
            var subForm = form.elts[i];
            if ((subForm.formt == "compound") && subForm.elts[0] &&
                (subForm.elts[0].name == "unquote-splicing")) { // (quasiquote (... (unquote-splicing ...) ...)
                exprs.push(currentCompound);
                currentCompound = { irt: "--compound", elts: [] };
                if (level == 0) {
                    exprs.push(lispDecode(subForm.elts[1])); // <---- unquoted code
                } else {
                    exprs.push(lispDecodeQuasiquotedForm(subForm.elts[1], level - 1));
                }
            } else {
                currentCompound.elts.push(lispDecodeQuasiquotedForm(subForm, level));
            }
        }
        if (currentCompound.elts.length > 0)
            exprs.push(currentCompound);
        return { irt: "--append", exprs: exprs };
    }
}

// Defmacro

function lispIsSplatName(name) {
    return name[0] == "@";
}

function lispCleanSplatName(name) {
    return name.slice(1);
}

// (DEFMACRO foobar (arg ... @rest) body ...) -- macro definition, creates macro function
//  0        1      2           \   3
//           name   args         \
//                                spl@t arg, gets rest of args
// 
// (FOOBAR arg1 arg2 ... argN) -- macro application form, passed as std-lib/forms object to m-function;
//  0      1    2 ...             macro uses destructs to extract args from form object,
//                                which has methods such as [] and SLICE.
function lispDecodeDefmacro(form) {
    var name = form.elts[1].name;
    var args = form.elts[2].elts;
    var i = 1; // skip over macro name in application
    var destructs = args.map(function(arg) {
            if (lispIsSplatName(arg.name)) {
                return { name: lispCleanSplatName(arg.name), 
                         value: { irt: "apply", 
                                  fun: { irt: "function", name: "slice" },
                                  args: [ { irt: "var", name: "--lisp-form" },
                                          { irt: "native", snippets: [ (i++).toString() ] } ] } };
            } else {
                return { name: arg.name, 
                         value: { irt: "apply",
                                  fun: { irt: "function", name: "[]" },
                                  args: [ { irt: "var", name: "--lisp-form" },
                                          { irt: "native", snippets: [ (i++).toString() ] } ] } }; // WJW
            }
        });
    var body = { irt: "progn", exprs: form.elts.slice(3).map(lispDecode) };
    var lambda = { irt: "lambda", req_params: [ { name: "--lisp-form" } ], body: body, destructs: destructs };
    return { irt: "defmacro", name: name, lambda: lambda };
}

function lispDenaturalizeForm(naturalForm) {
    // The form objects exposed to macro writers (called "natural"
    // because they exist in the Lisp world; see std-lib/forms.lisp)
    // are not equal to the actual forms used internally by the
    // compiler.  So, once a macro has produced forms, they need to be
    // turned back into compiler forms.  This is called
    // denaturalization.
    var x;
    if (x = naturalForm[lispEnvMangleSlotName("s")]) {
        return { formt: "string", s: x };
    } else if (x = naturalForm[lispEnvMangleSlotName("name")]) {
        return { formt: "symbol", name: x };
    } else if (x = naturalForm[lispEnvMangleSlotName("elts")]) {
        return { formt: "compound", elts: x[lispEnvMangleSlotName("peer")].map(lispDenaturalizeForm) }
    } else {
        throw "Illegal form for denaturalization " + uneval(naturalForm);
    }
}

function lispNaturalizeForm(form) {
    switch(form.formt) {
    case "string":
        return lispCall("new-string-form", form.s);
    case "symbol":
        return lispCall("new-symbol-form", form.name);
    case "compound":
        return lispCall("new-compound-form", form.elts.map(lispNaturalizeForm));
    }
    throw "Unknown form for naturalization " + uneval(form);
}

function lispDecodeProgn(form) {
    return lispDecodeImplicitProgn(form.elts.slice(1));
}

// (let ((name value) ...) body ...) -> (apply (lambda (name ...) (progn (set name value) ... body ...)))
function lispDecodeLet(form) {
    var bindings = form.elts[1].elts;
    var bodyForms = form.elts.slice(2).map(lispDecode);
    var params = bindings.map(function(b) { return { name: b.elts[0].name }; });
    var setters = bindings.map(function(b) { 
            return { irt: "set", name: b.elts[0].name, value: lispDecode(b.elts[1]) };
        });
    
    return { irt: "apply", 
             fun: { irt: "lambda", 
                    req_params: params,
                    body: { irt: "progn",
                            exprs: setters.concat(bodyForms) } },
             args: [] };
}

// Prevents multiple evaluation.
function lispDecodeOnce(varName, value, body) {
    return { irt: "apply", fun: { irt: "lambda", req_params: [ { name: varName } ], body: body }, args: [ value ] };
}

