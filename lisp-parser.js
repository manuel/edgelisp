// Parses Lisp code into a list of forms.

load("jsparse.js");

function lispParse(text) {
    return LispForms(ps(text)).ast;
}

var LispForm = function(input) { return LispForm(input); }; // forward decl.

var LispStringChar = negate("\"");
var LispStringChars = repeat1(LispStringChar);
var LispStringLiteral = action(sequence("\"", optional(LispStringChars), "\""), lispStringLiteralAction);
function lispStringLiteralAction(ast) { return { formt: "string", s: ast[1].join("") }; }

var LispSymbolCharNoClass = choice(range("a", "z"), "-", "*", "?", "!", "$", "_", "/", range("0", "9"));
var LispSymbolCharsNoClass = repeat1(LispSymbolCharNoClass);
var LispSymbolFormNoClass = action(LispSymbolCharsNoClass, lispSymbolFormAction);
var LispSymbolChar = choice("<", ">", LispSymbolCharNoClass);
var LispSymbolChars = repeat1(LispSymbolChar);
var LispSymbolForm = action(LispSymbolChars, lispSymbolFormAction);
function lispSymbolFormAction(ast) { return { formt: "symbol", name: ast.join("") }; }

var LispCompoundForm = action(sequence("(", repeat0(LispForm), ")"), lispCompoundFormAction);
function lispCompoundFormAction(ast) { return { formt: "compound", elts: ast[1] }; }

var LispFunctionForm = action(sequence("#'", LispSymbolForm), lispFunctionFormAction);
function lispFunctionFormAction(ast) {
    return { formt: "compound", elts: [ { formt: "symbol", name: "function" }, ast[1] ] }; }

var LispSlotName = action(sequence(".", LispSymbolFormNoClass), lispSlotNameAction);
function lispSlotNameAction(ast) { return "." + ast[1].name; }

var LispTypeExpr = action(wsequence("<",
                                    LispSymbolFormNoClass, 
                                    optional(LispSymbolFormNoClass), 
                                    repeat0(wsequence(LispSlotName, optional(LispForm))),
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
        slotFuns.push({ formt: "compound", elts: [ { formt: "symbol", name: slotName }, slotValue ] });
    }
    if (ast[2].name || (slotFuns.length > 0))
        return { formt: "compound", elts: [ { formt: "symbol", name: typeName },
                                            { formt: "symbol", name: objName },
                                            { formt: "compound", elts: slotFuns } ] }
    else
        return { formt: "symbol", name: typeName };
}

var LispForm = whitespace(choice(LispTypeExpr,
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
    throw "show: Unrecognized form " + uneval(form);
}
