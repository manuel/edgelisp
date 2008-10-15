// Transforms JavaScript representation into JavaScript text.

load("lisp-env.js");

function lispEmit(jr) {
    var emitFunction = lispEmitFunctionsMap[jr.jrt];
    if (emitFunction) return emitFunction(jr);
    else throw "unknown JR " + uneval(jr);
}

var lispEmitFunctionsMap = {
    "funapp": lispEmitFunapp,
    "function": lispEmitFunction,
    "string": lispEmitString,
    "multi": lispEmitMulti,
    "var": lispEmitVar,
    "vardef": lispEmitVardef,
}

function lispEmitFunapp(jr) {
    return "(" + lispEmit(jr.fun) + "(" + jr.args.map(lispEmit) + "))"; // fixme
}

function lispEmitFunction(jr) {
    return "(function(" + jr.params + ") { return " + lispEmit(jr.body) + "})";
}

function lispEmitString(jr) {
    return "(" + jr.s.toSource() + ")";
}

function lispEmitVar(jr) {
    return jr.name;
}

function lispEmitVardef(jr) {
    return "var " + jr.name + " = " + lispEmit(jr.value);
}

function lispEmitMulti(jr) {
    return jr.exprs.map(lispEmit).join(";");
}
