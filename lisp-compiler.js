// Transforms intermediate representation to JavaScript representation.

load("lisp-env.js");

function lispCompile(ir) {
    var compileFunction = lispCompileFunctionsMap[ir.irt];
    if (compileFunction) return compileFunction(ir);
    else throw "Unrecognized IR " + uneval(ir);
}

var lispCompileFunctionsMap = {
    "defun": lispCompileDefun,
    "defvar": lispCompileDefvar,
    "apply": lispCompileApply,
    "function": lispCompileFunction,
    "lambda": lispCompileLambda,
    "make-class": lispCompileMakeClass,
    "make-instance": lispCompileMakeInstance,
    "set-method": lispCompileSetMethod,
    "invoke-method": lispCompileInvokeMethod,
    "string": lispCompileString,
    "var": lispCompileVar,
    "progn": lispCompileProgn,
}

function lispCompileDefun(ir) {
    var funName = lispEnvMangleFunName(ir.name);
    var funJR = lispCompile(ir.lambda);
    return { jrt: "multi", exprs: [ { jrt: "vardef", name: funName, value: funJR },
                                    { jrt: "var", name: funName } ] };
}

function lispCompileDefvar(ir) {
    var varName = lispEnvMangleVarName(ir.name);
    var valueJR = lispCompile(ir.value);
    return { jrt: "multi", exprs: [ { jrt: "vardef", name: varName, value: valueJR },
                                    { jrt: "var", name: varName } ] };
}

function lispCompileApply(ir) {
    return { jrt: "funapp", fun: lispCompile(ir.fun), args: ir.args.map(lispCompile) };
}

function lispCompileLambda(ir) {
    return { jrt: "function", 
             params: ir.req_params.map(lispEnvMangleVarName), 
             body: lispCompile(ir.body) };
}

function lispCompileString(ir) {
    return { jrt: "string", s: ir.s };
}

function lispCompileFunction(ir) {
    return { jrt: "var", name: lispEnvMangleFunName(ir.name) };
}

function lispCompileVar(ir) {
    return { jrt: "var", name: lispEnvMangleVarName(ir.name) };
}

function lispCompileMakeClass(ir) {
    return { jrt: "obj", props: { "name": { jrt: "string", s: ir.name } } };
}

function lispCompileMakeInstance(ir) {
    return { jrt: "obj", proto: lispCompile(ir["class"]) };
}

function lispCompileSetMethod(ir) {
    return { jrt: "setprop", 
            obj: lispCompile(ir["class"]),
            name: lispEnvMangleMethodName(ir.name),
            value: lispCompile(ir.lambda) };
}

function lispCompileInvokeMethod(ir) {
    return { jrt: "invoke",
            name: lispEnvMangleMethodName(ir.name),
            params: ir.params.map(lispCompile) };
}

function lispCompileProgn(ir) {
    return { jrt: "multi", exprs: ir.exprs.map(lispCompile) };
}
