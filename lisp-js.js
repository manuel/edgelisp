// Accessing Lisp from JavaScript

function lispVar(varName) {
    return eval(lispEnvMangleVarName(varName));
}

function lispCall(funName, arg) {
    return eval(lispEnvMangleFunName(funName))(arg);
}

function lispSlot(obj, slotName) {
    return obj[lispEnvMangleSlotName(slotName)];
}
