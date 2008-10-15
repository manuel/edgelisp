// Transforms JavaScript representation into JavaScript text.

load("lisp-env.js");

function lispEmit(jr) {
    var emitFunction = lispEmitFunctionsMap[jr.jrt];
    if (emitFunction) return emitFunction(jr);
    else throw "Unknown JR " + uneval(jr);
}

var lispEmitFunctionsMap = {
    "obj": lispEmitObj,
    "funapp": lispEmitFunapp,
    "function": lispEmitFunction,
    "string": lispEmitString,
    "multi": lispEmitMulti,
    "var": lispEmitVar,
    "vardef": lispEmitVardef,
    "setprop": lispEmitSetprop,
    "invoke": lispEmitInvoke,
}

function lispEmitFunapp(jr) {
    return "(" + lispEmit(jr.fun) + "(" + jr.args.map(lispEmit).join(", ") + "))";
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

function lispEmitObj(jr) {
    var res = "{";
    var props = jr.props;
    for (var k in props) {
        res += uneval(k) + ": " + lispEmit(props[k]) + ", ";
    }
    res += "}";
    if (jr.proto) {
        // this contortion is necessary because __proto__ cannot be put into props directly,
        // or otherwise __proto__'s properties show up in props.
        return "((function(__lispX_obj) { __lispX_obj.__proto__ = " + lispEmit(jr.proto) + "; return __lispX_obj; })(" + res + "))";
    } else {
        return res;
    }
}

function lispEmitSetprop(jr) {
    return "((" + lispEmit(jr.obj) + ")." + jr.name + " = " + lispEmit(jr.value) + ")";
}

function lispEmitInvoke(jr) {
    // slightly contorted, to prevent multiple evaluation of receiver
    var receiver = lispEmit(jr.params[0]); 
    var other_args = [ "__lispX_this" ].concat(jr.params.slice(1).map(lispEmit));
    return "((function(__lispX_this) { return __lispX_this." + jr.name + "(" + other_args.join(", ") + ") }(" + receiver + ")))";
}
