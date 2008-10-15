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
    return { irt: "funcall", fun: funRefIR };
}

var lispDecodeCompoundFunctionsTable = {
    "defun": lispDecodeDefun,
    "defvar": lispDecodeDefvar,
    "funcall": lispDecodeFuncall,
    "function": lispDecodeFunction,
    "lambda": lispDecodeLambda,
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

function lispDecodeFuncall(form) {
    var fun_ir = lispDecode(form.elts[1]);
    return { irt: "funcall", fun: fun_ir };
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
