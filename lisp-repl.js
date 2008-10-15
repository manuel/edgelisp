load("lisp-parser.js");
load("lisp-decoder.js");
load("lisp-compiler.js");
load("lisp-emitter.js");

while(1) {
    try {
        var lispText = readline();
        var lispForms = lispParse(lispText);
        var lispIR = lispDecode(lispForms[0]);
        var lispJR = lispCompile(lispIR);
        var lispJavaScriptText = lispEmit(lispJR);
        print(eval(lispJavaScriptText));
    } catch(e) {
        print(e);
    }
}
