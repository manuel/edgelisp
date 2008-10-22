// Lisp runtime: variables and functions needed by the compiled JavaScript code.

load("lisp-js.js");

// Returns true iff class B is a subclass of (or equal to) class A.
function lispIsSubclass(classA, classB) {
    return classA == classB;
}

function lispGetClass(obj) {
    return obj.__proto__;
}

function lispCheckType(obj, type) {
    if (!lispIsSubclass(lispGetClass(obj), type))
        throw (uneval(obj) + " is not of type " + uneval(type));
}

// Exception handler frame:
// 
// { handlers: <<handlers>>, parentFrame: <<handler_frame>> }
//
// handlers is a list of { class: <<exception_class>>, function: <<handler_function>> }

var lispHandlerFrame = null; // bottom-most frame or null

function lispCallWithHandlers(fun, handlers) {
    try {
        var origHandlerFrame = lispHandlerFrame;
        lispHandlerFrame = { handlers: handlers, parentFrame: origHandlerFrame };
        return fun();
    } finally {
        lispHandlerFrame = origHandlerFrame;
    }
}

function lispFindHandler(exception, handlerFrame) {
    if (!handlerFrame) return null;
    var handlers = handlerFrame.handlers;
    var exceptionClass = lispGetClass(exception);
    for (var i in handlers) {
        var handler = handlers[i];
        if (lispIsSubclass(handler["class"], exceptionClass))
            return handler;
    }
    return lispFindHandler(exception, handlerFrame.parentFrame);
}

function lispThrow(exception) {
    var handler = lispFindHandler(exception, lispHandlerFrame);
    if (handler) 
        return handler["function"](exception, function() { throw "NIY"; });
    else 
        throw "No applicable handler for exception " + uneval(lispGetClass(exception).name);
}

// Escape continuations (jump buffers, first class exit procedures)

function lispCallEC(fun) {
    var token = {};
    var ec = function(result) {
        token.result = result;
        throw token;
    };
    try {
        return fun(ec);
    } catch(obj) {
        if (obj == token) {
            return token.result;
        } else {
            throw obj;
        }
    }
}

// WHILE

function lispWhile(testFn, fn) {
    while(testFn()) {
        fn();
    }
    return null;
}

// Macros

// Produce a new natural compound from a list of natural compound forms
function lispCompoundFormsAppend(forms) {
    var res = [];
    for (var i in forms) {
        var form = forms[i];
        var elts = lispSlot(form, "elts");
        var peer = lispSlot(elts, "peer");
        res = res.concat(peer);
    }
    return lispCall("new-compound-form", res);
}
