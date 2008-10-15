load("lisp-parser.js");
load("lisp-decoder.js");
load("lisp-compiler.js");
load("lisp-emitter.js");

print("CyberLisp 0.0.1");

var lispDebugToggle = false;

while(1) {
    try {
        var lispText = readline();
        
        if (lispText == "/d") {
            lispDebugToggle = !lispDebugToggle;
            print("Debugging " + (lispDebugToggle ? "ON" : "OFF"));
            continue;
        } else if (lispText[0] == "{") {
            print(uneval(eval(lispText)));
            continue;
        } else if (lispText == "") {
            continue;
        }

        var lispForms = lispParse(lispText);
        lispDebug(lispForms);
        var lispIR = lispDecode(lispForms[0]);
        lispDebug(lispIR);
        var lispJR = lispCompile(lispIR);
        lispDebug(lispJR);
        var lispJS = lispEmit(lispJR);
        lispDebug(lispJS);
        print(uneval(eval(lispJS)));

    } catch(e) {
        print(uneval(e));
    }
}

function lispDebug(obj) {
    if (lispDebugToggle) print(uneval(obj));
}
