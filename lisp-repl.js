load("lisp-parser.js");
load("lisp-decoder.js");
load("lisp-compiler.js");
load("lisp-emitter.js");

print("CyberLisp 0.0.1");

var lispDebug = false;

while(1) {
    try {
        var lispText = readline();

        if ((lispText == "/debug") || (lispText == "/d")) {
            lispDebug = !lispDebug;
            if (lispDebug) {
                print("Debugging ON");
            } else {
                print("Debugging OFF");
            }
            continue;
        } else if (lispText[0] == "{") {
            print(uneval(eval(lispText)));
            continue;
        }

        var lispForms = lispParse(lispText);
        if (lispDebug) print(uneval(lispForms));
        var lispIR = lispDecode(lispForms[0]);
        if (lispDebug) print(uneval(lispIR));
        var lispJR = lispCompile(lispIR);
        if (lispDebug) print(uneval(lispJR));
        var lispJS = lispEmit(lispJR);
        if (lispDebug) print(uneval(lispJS));
        print(uneval(eval(lispJS)));
    } catch(e) {
        print(uneval(e));
    }
}
