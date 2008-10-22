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
    "getprop": lispEmitGetprop,
    "setprop": lispEmitSetprop,
    "invoke": lispEmitInvoke,
    "set": lispEmitSet,
    "finally": lispEmitFinally,
    "throw": lispEmitThrow,
    "catch": lispEmitCatch,
    "callec": lispEmitCallEC,
    "checktype": lispEmitChecktype,
    "native": lispEmitNative,
    "noop": lispEmitNoop,
    "--symbol": lispEmit__Symbol,
    "--string": lispEmit__String,
    "--compound": lispEmit__Compound,
    "--append": lispEmit__Append,
    "defmacro": lispEmitDefmacro,
    // should be macros:
    "bind": lispEmitBind,
};

function lispEmitFunapp(jr) {
    return "(" + lispEmit(jr.fun) + "(" + jr.args.map(lispEmit).join(", ") + "))";
}

function lispEmitFunction(jr) {
    var destructs = jr.destructs;
    var destructsCode = "";
    for (var i in destructs) {
        var destruct = destructs[i];
        destructsCode += ("var " + destruct.name + " = " + lispEmit(destruct.value) + "; ");
    }
    return "(function(" + jr.params + ") { " + destructsCode + "return " + lispEmit(jr.body) + "; })";
}

function lispEmitString(jr) {
    return uneval(jr.s);
}

function lispEmitVar(jr) {
    return jr.name;
}

function lispEmitVardef(jr) {
    return "(" + jr.name + " = " + lispEmit(jr.value) + ")";
}

function lispEmitMulti(jr) {
    return "(" + jr.exprs.map(lispEmit).join(", ") + ")";
}

function lispEmitObj(jr) {
    var res = "{";
    var props = jr.props;
    for (var k in props) {
        res += uneval(k) + ": " + lispEmit(props[k]) + ", ";
    }
    res += "}";
    if (jr.proto) {
        return lispEmitOnce("__lispX_obj", res,
                            "__lispX_obj.__proto__ = " + lispEmit(jr.proto) + "; return __lispX_obj;");
    } else {
        return res;
    }
}

function lispEmitGetprop(jr) {
    return "((" + lispEmit(jr.obj) + ")." + jr.name + ")";
}

function lispEmitSetprop(jr) {
    return "((" + lispEmit(jr.obj) + ")." + jr.name + " = " + lispEmit(jr.value) + ")";
}

function lispEmitInvoke(jr) {
    var receiver = lispEmit(jr.params[0]);
    var args = [ "__lispX_this" ].concat(jr.params.slice(1).map(lispEmit));
    return lispEmitOnce("__lispX_this", receiver,
                        "return __lispX_this." + jr.name + "(" + args.join(", ") + ")");
}

function lispEmitSet(jr) {
    return "(" + jr.name + " = " + lispEmit(jr.value) + ")";
}

function lispEmitFinally(jr) {
    return "((function() { try { return " + lispEmit(jr["protected"]) + "; } finally { " + lispEmit(jr.cleanup) + "; } })())";
}

function lispEmitThrow(jr) {
    return "lispThrow(" + lispEmit(jr.exception) + ")";
}

function lispEmitCatch(jr) {
    var handlers = jr.handlers;
    var hCode = "[";
    for (var i in handlers) {
        var h = handlers[i];
        hCode += "{\"class\":" + lispEmit(h["class"]) + ",\"function\":" + lispEmit(h["function"]) + "},";
    }
    hCode += "]";
    return "lispCallWithHandlers(function() { return " + lispEmit(jr.body) + "; }, " + hCode + ")";
}

function lispEmitCallEC(jr) {
    return "lispCallEC(" + lispEmit(jr.fun) + ")";
}

function lispEmitChecktype(jr) {
    return "lispCheckType(" + lispEmit(jr.obj) + ", " + lispEmit(jr.type) + ")";
}

function lispEmitNative(jr) {
    function emitSnippet(snippet) {
        if (snippet.jrt)
            return lispEmit(snippet);
        else
            return snippet;
    }
    return "(" + jr.snippets.map(emitSnippet).join("") + ")";
}

function lispEmitNoop(jr) {
    return "undefined";
}

function lispEmit__String(jr) {
    return "lispCall(\"new-string-form\", " + uneval(jr.name) + ")";    
}

function lispEmit__Symbol(jr) {
    return "lispCall(\"new-symbol-form\", " + uneval(jr.name) + ")";
}

// this is getting ridiculously meta...
function lispEmit__Compound(jr) {
    return "lispCall(\"new-compound-form\", [" + jr.elts.map(lispEmit).join(", ") + "])";
}

function lispEmit__Append(jr) {
    return "lispCompoundFormsAppend([" + jr.exprs.map(lispEmit).join(", ") + "])";
}

function lispEmitDefmacro(jr) {
    return "(lispMacrosTable[\"" + jr.name + "\"] = " + lispEmit(jr.lambda) + ")";
}

function lispEmitBind(jr) {
    var setters = "", resetters = "";
    var bindings = jr.bindings;
    for (var i in bindings) {
        var b = bindings[i];
        var mangledName = lispEnvMangleVarName(b[0]);
        setters += ("var __lispTemp" + i + " = " + mangledName + "; ");
        setters += (mangledName + " = " + lispEmit(b[1]) + "; ");
        resetters += (mangledName + " = __lispTemp" + i + "; ");
    }
    var body = lispEmit(jr.body);
    return "((function() { try { " + setters + "return " + body + "} finally { " + resetters + "} })())";
}

// Prevents multiple evaluation.
function lispEmitOnce(varName, value, code) {
    return "((function(" + varName + ") {" + code + "}(" + value + ")))";
}
