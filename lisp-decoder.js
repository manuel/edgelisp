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
    case "symbol": // special form or function application
        var decodeCompoundFunction = lispDecodeCompoundFunctionsTable[op.name];
        if (decodeCompoundFunction) {
            return decodeCompoundFunction(form);
        } else {
            return lispDecodeFunctionApplication(form);
        }
    case "compound": // type-expr as first element
        if (lispIsTypeName(op.elts[0].name)) {
            return lispDecodeCompoundFormWithTypeExpr(op);
        }
    }
    throw "Malformed compound form " + uneval(form);
}

function lispDecodeFunctionApplication(form) {
    var funRefIR = { irt: "function", name: form.elts[0].name };
    var funArgs = form.elts.splice(1).map(lispDecode);
    return { irt: "apply", fun: funRefIR, args: funArgs };
}

function lispDecodeCompoundFormWithTypeExpr(form) {
    return { irt: "apply",
             fun: { irt: "lambda", 
                    req_params: [ { name: "--lisp-x-temp" } ],
                    body: { irt: "progn", 
                    exprs: form.elts[2].elts.map(function(destructForm) {
                            return { irt: "apply", 
                                     fun: { irt: "function", name: lispSlotSetterName(destructForm.elts[0].name) },
                                     args: [ { irt: "var", name: "--lisp-x-temp"},
                                             lispDecode(destructForm.elts[1]) ] };
                        }).concat({ irt: "var", name: "--lisp-x-temp"}) } },
             args: [ { irt: "make-instance", "class": { irt: "var", name: form.elts[0].name } } ] };
}

function lispSlotSetterName(slotGetterName) {
    return slotGetterName + "-setter";
}

function lispCleanSlotGetterName(slotGetterName) {
    return slotGetterName.slice(1);
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
    var name = form.elts[1].name;
    var member_names = form.elts.slice(2).map(function (member_form) { return member_form.name; });
    return { irt: "defvar", 
             name: name,
             value: { irt: "make-class", name: name, member_names: member_names } };
}

function lispDecodeDef(form) {
    var name = form.elts[1].name;
    var ll = lispDecodeLambdaList(form.elts[2]);
    return { irt: "progn",
             exprs: [ { irt: "set-method",
                        name: name,
                        "class": { irt: "var", name: ll[0].type },
                        lambda: { irt: "lambda", req_params: ll, body: lispDecode(form.elts[3]) } },
                      { irt: "defun",
                        name: name,
                        lambda: { irt: "lambda",
                                  req_params: ll,
                                  body: { irt: "invoke-method",
                                          name: name,
                                          params: ll.map(function(param) {
                                                           return { irt: "var", name: param.name } }) } } } ] };
}

function lispDecodeNew(form) {
    var cls_ir = lispDecode(form.elts[1]);
    return { irt: "make-instance", "class": cls_ir };
}

function lispDecodeSet(form) {
    var name = form.elts[1].name;
    var value_ir = lispDecode(form.elts[2]);
    return { irt: "set", name: name, value: value_ir };
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
    return { irt: "get-slot", obj: lispDecode(form.elts[1]), name: form.elts[2].name };
}

function lispDecodeSetSlotValue(form) {
    return { irt: "set-slot", obj: lispDecode(form.elts[1]), name: form.elts[2].name,
             value: lispDecode(form.elts[3]) };
}
