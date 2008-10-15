// Transforms intermediate representation to JavaScript representation.

load("lisp-env.js");

function lispCompile(ir) {
    var compileFunction = lispCompileFunctionsMap[ir.irt];
    if (compileFunction) return compileFunction(ir);
    else throw "Unrecognized IR " + uneval(ir);
}

var lispCompileFunctionsMap = {
    "defun": lispCompileDefun,
    "funcall": lispCompileFuncall,
    "function": lispCompileFunction,
    "lambda": lispCompileLambda,
    "string": lispCompileString,
}

function lispCompileDefun(ir) {
    var funName = lispEnvMangleFunName(ir.name);
    var funJR = lispCompile(ir.lambda);
    return { jrt: "multi", exprs: [ { jrt: "vardef", name: funName, value: funJR },
                                    { jrt: "var", name: funName } ] };
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

function lispCompileFunction(ir) {
    return { jrt: "var", name: lispEnvMangleFunName(ir.name) };
}
