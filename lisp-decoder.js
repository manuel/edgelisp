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
    "bind": lispDecodeBind,
    "slot-value": lispDecodeSlotValue,
    "set-slot-value": lispDecodeSetSlotValue,
    "native": lispDecodeNative,
    // should be macros:
    "let": lispDecodeLet,
};

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
            // user-defined function
            return lispDecodeFunctionApplication(form);
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
    return { irt: "apply", fun: { irt: "function", name: form.elts[0].name }, args: form.elts.splice(1).map(lispDecode) };
}

// Needed because <T> parses as a symbol, and not as a type
// expression, but we still want (<T>) to create a new <T> instance.
function lispDecodeCompoundFormWithTypeName(form) {
    return { irt: "make-instance", "class": lispDecode(form.elts[0]) };
}

function lispDecodeCompoundFormWithTypeExpr(form) {
    // Turns a destruct form of a type expression into a setter call.
    function decodeDestructFormForNewInstance(destructForm) {
        return { irt: "apply",
                 fun: { irt: "function", name: lispSetterName(destructForm.elts[0].name) },
                 args: [ { irt: "var", name: "--lisp-class"}, lispDecode(destructForm.elts[1]) ] }
    }

    // Create new instance, run slot setters on it, and return it.
    return lispDecodeOnce("--lisp-class", { irt: "make-instance", "class": { irt: "var", name: form.elts[0].name } },
                          { irt: "progn",
                                  exprs: form.elts[2].elts.map(decodeDestructFormForNewInstance)
                                         .concat({ irt: "var", name: "--lisp-class"}) });
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
    var lambdaBody = lispDecode(form.elts[3]);
    var destructs = lispDecodeLambdaListDestructs(form.elts[2]);
    var lambda_ir = { irt: "lambda", req_params: ll, body: lambdaBody, destructs: destructs };
    return { irt: "defun", name: name, lambda: lambda_ir };
}

function lispDecodeDefvar(form) {
    var name = form.elts[1].name;
    var value_ir = lispDecode(form.elts[2]);
    return { irt: "defvar", name: name, value: value_ir };
}

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
            exprs: [ class_defvar ].concat(member_getters, member_setters, [ { irt: "var", name: cls_name } ]) };
}

function lispDecodeDef(form) {
    var name = form.elts[1].name;
    var ll = lispDecodeLambdaList(form.elts[2]);
    var destructs = lispDecodeLambdaListDestructs(form.elts[2]);
    var cls_ir = { irt: "var", name: ll[0].type };
    var generic_ll = ll.map(function(param) { return { name: param.name }; });
    return lispDefineMethodAndGenericIR(cls_ir, name, ll, generic_ll, lispDecode(form.elts[3]), destructs);
}

function lispDefineMethodAndGenericIR(class_ir, name, method_ll, generic_ll, method_ir, destructs) {
    return { irt: "progn",
             exprs: [ { irt: "set-method",
                        name: name,
                        "class": class_ir,
                        lambda: { irt: "lambda", req_params: method_ll, body: method_ir, destructs: destructs } },
                      { irt: "defun",
                        name: name,
                        lambda: { irt: "lambda",
                                  req_params: generic_ll,
                                  body: { irt: "invoke-method",
                                          name: name,
                                          params: generic_ll.map(function(param) {
                                                  return { irt: "var", name: param.name } }) } } } ] };
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
        return { irt: "apply", fun: { irt: "function", name: lispSetterName(getter) }, args: args.map(lispDecode) };
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

function lispDecodeNative(form) {
    return { irt: "native", code: form.elts[1].s };
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

