// Parses Lisp code into a list of forms.

load("jsparse.js");

function lispParse(text) {
    return LispForms(ps(text)).ast;
}

var LispForm = function(input) { return LispForm(input); }; // forward decl.
var LispFormReduced = function(input) { return LispFormReduced(input); }; // forward decl.

var LispStringChar = negate("\"");
var LispStringChars = repeat1(LispStringChar);
var LispStringLiteral = action(sequence("\"", optional(LispStringChars), "\""), lispStringLiteralAction);
function lispStringLiteralAction(ast) { return { formt: "string", s: ast[1].join("") }; }

var LispSymbolCharReduced = choice(range("a", "z"), "-", "*", "?", "!", "_", "/", "[", "]", range("0", "9"));
var LispSymbolCharsReduced = repeat1(LispSymbolCharReduced);
var LispSymbolFormReduced = action(LispSymbolCharsReduced, lispSymbolFormAction);
var LispSymbolChar = choice("<", ">", ".", LispSymbolCharReduced);
var LispSymbolChars = repeat1(LispSymbolChar);
var LispSymbolForm = action(LispSymbolChars, lispSymbolFormAction);
function lispSymbolFormAction(ast) { return { formt: "symbol", name: ast.join("") }; }

var LispCompoundForm = action(sequence("(", repeat0(LispForm), ")"), lispCompoundFormAction);
function lispCompoundFormAction(ast) { return { formt: "compound", elts: ast[1] }; }

var LispFunctionForm = action(sequence("#'", LispSymbolForm), lispFunctionFormAction);
function lispFunctionFormAction(ast) {
    return { formt: "compound", elts: [ { formt: "symbol", name: "function" }, ast[1] ] }; }

var LispSlotName = action(sequence(".", LispSymbolFormReduced), lispSlotNameAction);
function lispSlotNameAction(ast) { return "." + ast[1].name; }

var LispTypeExpr = action(wsequence("<",
                                    LispSymbolFormReduced, 
                                    whitespace(optional(LispSymbolFormReduced)),
                                    repeat0(wsequence(LispSlotName, optional(LispFormReduced))),
                                    ">"),
                          lispTypeExprAction);
function lispTypeExprAction(ast) {
    var typeName = "<" + ast[1].name + ">";
    var objName = ast[2].name || ast[1].name;
    var slotFuns = [];
    for (var i in ast[3]) {
        var slotFunAst = ast[3][i];
        var slotName = slotFunAst[0];
        var slotValue = slotFunAst[1];
        var elts = [ { formt: "symbol", name: slotName } ];
        if (slotValue) elts.push(slotValue);
        slotFuns.push({ formt: "compound", elts: elts }); // called destructs in other parts; both names suck
    }
    if (ast[2].name || (slotFuns.length > 0))
        return { formt: "compound", elts: [ { formt: "symbol", name: typeName },
                                            { formt: "symbol", name: objName },
                                            { formt: "compound", elts: slotFuns } ] }
    else
        // We want simple type expressions like <T> to turn into
        // symbols, but we have to put type expressions before symbols
        // into the grammar, or "<T ..." will parse as "<T" "...".
        return { formt: "symbol", name: typeName };
}

var LispNativeForm = action(sequence("{%", repeat0(negate("%")), "%}"),
                            lispNativeFormAction);
function lispNativeFormAction(ast) {
    return { formt: "compound", elts: [ { formt: "symbol", name: "native" },
                                        { formt: "string", s: ast[1].join("") } ] };
}

// This is needed because otherwise <person .name> will be parsed as "<person" ".name" ">"
var LispFormReduced = whitespace(choice(LispNativeForm,
                                        LispTypeExpr,
                                        LispFunctionForm,
                                        LispStringLiteral,
                                        LispSymbolFormReduced,
                                        LispCompoundForm));
var LispForm = whitespace(choice(LispNativeForm,
                                 LispTypeExpr,
                                 LispFunctionForm,
                                 LispStringLiteral,
                                 LispSymbolForm,
                                 LispCompoundForm));
var LispForms = repeat0(LispForm);

function lispShowForm(form) {
    switch (form.formt) {
    case "symbol": return form.name;
    case "string": return uneval(form.s);
    case "compound": return "(" + form.elts.map(lispShowForm).join(" ") + ")";
    }
    return "[Unrecognized form: " + uneval(form) + "]";
}
