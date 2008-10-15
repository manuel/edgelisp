// Transforms intermediate representation to JavaScript representation.

function lispCompile(ir) {
    var compileFunction = lispCompileFunctionsMap[ir.irt];
    if (compileFunction) return compileFunction(ir);
    else throw "Unrecognized IR " + uneval(ir);
}

var lispCompileFunctionsMap = {
    "funcall": lispCompileFuncall,
    "lambda": lispCompileLambda,
    "string": lispCompileString
}

function lispCompileFuncall(ir) {
    return { jrt: "funapp", fun: lispCompile(ir.fun), args: [] };
}

function lispCompileLambda(ir) {
    return { jrt: "function", params: ir.req_params, body: lispCompile(ir.body) };
}

function lispCompileString(ir) {
    return { jrt: "string", s: ir.s };
}
