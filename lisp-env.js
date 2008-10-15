// The global environment contains top level function and variable
// bindings, created by the DEF... operators.

// CyberLisp is a Lisp-2, which means that functions live in a
// separate namespace from other variables.

function lispEnvMangleVarName(name) {
    return "__lispV_" + lispEnvMangle(name);
}

function lispEnvMangleFunName(name) {
    return "__lispF_" + lispEnvMangle(name);
}

function lispEnvMangleMethodName(name) {
    return "__lispM_" + lispEnvMangle(name);
}

function lispEnvMangle(name) {
    return name;
}
