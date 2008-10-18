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

function lispEnvMangleSlotName(name) {
    return "__lispS_" + lispEnvMangle(name);
}

// Lisp symbols have to be lowercase, so we can use uppercase
// characters to escape special characters.
var lispMangleTable = [["-", "H"],
                       ["*", "A"],
                       ["?", "Q"],
                       ["!", "X"],
                       ["<", "L"],
                       [">", "G"],
                       ["$", "D"],
                       ["/", "S"]];

function lispEnvMangle(name) {
    for (var i in lispMangleTable) {
        var pair = lispMangleTable[i];
        var re = new RegExp("\\" + pair[0], "g");
        name = name.replace(re, pair[1]);
    }
    return name;
}

