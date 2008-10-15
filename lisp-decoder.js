// Transforms forms into intermediate representation.

function lispDecode(form) {
    switch (form.formt) {
    case "string": return lispDecodeString(form);
    case "symbol": return lispDecodeSymbol(form);
    case "compound": return lispDecodeCompound(form);
    }
    throw "Unrecognized form " + uneval(form);
}

function lispDecodeString(form) {
    return { irt: "string", s: form.s };
}

function lispDecodeSymbol(form) {
    return { irt: "var", name: form.name };
}

function lispDecodeCompound(form) {
    var op = form.elts[0];
    if (op.formt == "symbol") {
        var decodeCompoundFunction = lispDecodeCompoundFunctionsTable[op.name];
        if (decodeCompoundFunction) {
            return decodeCompoundFunction(form);
        } else {
            return lispDecodeFunctionApplication(form);
        }
    } else {
        throw "First element of compound form must be a symbol " + uneval(form);
    }
}

function lispDecodeFunctionApplication(form) {
    var funRefIR = { irt: "function", name: form.elts[0].name };
    var funArgs = form.elts.splice(1).map(lispDecode);
    return { irt: "apply", fun: funRefIR, args: funArgs };
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
};

function lispDecodeLambda(form) {
    function lambdaReqParamNames(form) {
        return form.elts[1].elts.map(function(paramForm) { return paramForm.name; });
    }
    function lambdaBodyIR(form) {
        return lispDecode(form.elts[2]);
    }
    var req_param_names = lambdaReqParamNames(form);
    var body_ir = lambdaBodyIR(form);
    return { irt: "lambda", req_params: req_param_names, body: body_ir };
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
    var lambdaParams = form.elts[2].elts.map(function(paramForm) { return paramForm.name; });
    var lambdaBody = lispDecode(form.elts[3]);
    var lambda_ir = { irt: "lambda", req_params: lambdaParams, body: lambdaBody };
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
    // extract form data
    var name = form.elts[1].name;
    var args = form.elts[2];
    var body = form.elts[3];

    var the_arg = args.elts[0];
    var other_args = args.elts.slice(1);
    
    var cls = the_arg.elts[1];
    var inst = the_arg.elts[2].name;
    var cls_ir = lispDecode(cls);

    var param_names = [ inst ].concat(other_args.map(function(arg) { return arg.name; }));

    // create method and generic function lambdas
    var lambda_ir = { irt: "lambda", 
                      req_params: param_names,
                      body: lispDecode(body) };

    var param_refs = param_names.map(function(param) { return { irt: "var", name: param }; });
    var gf_ir = { irt: "lambda",
                  req_params: param_names,
                  body: { irt: "invoke-method", name: name, params: param_refs } };

    // create set-method and defun (for the generic)
    return { irt: "progn",
            exprs: [ { irt: "set-method", name: name, "class": cls_ir, lambda: lambda_ir },
                     { irt: "defun", name: name, lambda: gf_ir } ] };
}

function lispDecodeNew(form) {
    var cls_ir = lispDecode(form.elts[1]);
    return { irt: "make-instance", "class": cls_ir };
}
