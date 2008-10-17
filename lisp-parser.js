// Parses Lisp code into a list of forms.

load("jsparse.js");

var LispForm = function(input) { return LispForm(input); }; // forward decl.

var LispStringChar = negate("\"");
var LispStringChars = repeat1(LispStringChar);
var LispStringLiteral = action(sequence("\"", optional(LispStringChars), "\""),
                               lispStringLiteralAction);

var LispSymbolChar = choice(range("a", "z"), "-", "*", "?", "!", "$", "_", "/", "<", ">", range("0", "9"));
var LispSymbolChars = repeat1(LispSymbolChar);
var LispSymbolForm = action(LispSymbolChars,
                            lispSymbolFormAction);

var LispCompoundForm = action(sequence("(", repeat0(LispForm), ")"),
                              lispCompoundFormAction);

var LispFunctionForm = action(sequence("#'", LispSymbolForm),
                              lispFunctionFormAction);

var LispForm = whitespace(choice(LispFunctionForm,
                                 LispStringLiteral, 
                                 LispSymbolForm, 
                                 LispCompoundForm));
var LispForms = repeat0(LispForm);

function lispParse(text) {
    return LispForms(ps(text)).ast;
}

function lispStringLiteralAction(jsparse_ast) {
    return { formt: "string", s: jsparse_ast[1].join("") };
}

function lispSymbolFormAction(jsparse_ast) {
    return { formt: "symbol", name: jsparse_ast.join("") };
}

function lispCompoundFormAction(jsparse_ast) {
    return { formt: "compound", elts: jsparse_ast[1] };
}

function lispFunctionFormAction(jsparse_ast) {
    return { formt: "compound", elts: [ { formt: "symbol", name: "function" },
                                        jsparse_ast[1] ] };
}
