// Accessing Lisp from JavaScript

function lispVar(varName) {
    return eval(lispEnvMangleVarName(varName));
}

function lispCall(funName, args) {
    return eval(lispEnvMangleFunName(funName))(args);
}

function lispSlot(obj, slotName) {
    return obj[lispEnvMangleSlotName(slotName)];
}
