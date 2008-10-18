print("CyberLisp 0.0.1");

load("lisp-parser.js");
load("lisp-decoder.js");
load("lisp-compiler.js");
load("lisp-emitter.js");
load("lisp-rt.js");

var lispDebugToggle = false;

while(1) {
    try {
        var lispText = readline();
        
        if (lispText == "/d") { // debug
            lispDebugToggle = !lispDebugToggle;
            print("Debugging " + (lispDebugToggle ? "ON" : "OFF"));
            continue;
        } else if (lispText == "/r") { // reload
            load("lisp-repl.js");
        } else if (lispText[0] == "{") { // inline JS
            print(uneval(eval(lispText)));
            continue;
        } else if (lispText == "") { // empty line
            continue;
        }

        // Lisp evaluation
        var lispForms = lispParse(lispText);
        lispDebug("S-EXP: " + lispShowForm(lispForms[0]));
        lispDebug("FORMS: " + uneval(lispForms));
        var lispIR = lispDecode(lispForms[0]);
        lispDebug("   IR: " + uneval(lispIR));
        var lispJR = lispCompile(lispIR);
        lispDebug("   JR: " + uneval(lispJR));
        var lispJS = lispEmit(lispJR);
        lispDebug("   JS: " + lispJS);
        print(uneval(eval(lispJS)));

    } catch(e) {
        print(uneval(e));
    }
}

function lispDebug(obj) {
    if (lispDebugToggle) print("; " + obj);
}
