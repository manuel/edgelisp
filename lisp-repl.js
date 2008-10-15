load("lisp-parser.js");
load("lisp-decoder.js");
load("lisp-compiler.js");
load("lisp-emitter.js");

print("CyberLisp 0.0.1");

while(1) {
    try {
        var lispText = readline();
        var lispForms = lispParse(lispText);
        var lispIR = lispDecode(lispForms[0]);
        var lispJR = lispCompile(lispIR);
        var lispJS = lispEmit(lispJR);
        print(eval(lispJS));
    } catch(e) {
        print(e);
    }
}
