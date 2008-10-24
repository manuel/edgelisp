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
        } else if (lispText == "") { // empty line
            continue;
        }

        // Lisp evaluation
        var lispForms = lispParse(lispText);
        lispDebug("FORMS: " + lispUneval(lispForms));
        var lispIR = { irt: "progn", exprs: lispForms.map(lispDecode) };
        lispDebug("   IR: " + lispUneval(lispIR));
        var lispJR = lispCompile(lispIR);
        lispDebug("   JR: " + lispUneval(lispJR));
        var lispJS = lispEmit(lispJR);
        lispDebug("   JS: " + lispJS);
        var lispResult = eval(lispJS);
        if(lispResult && lispResult.__lispM_show) {
            print(lispResult.__lispM_show(lispResult));
        } else {
            print(lispUneval(lispResult));
        }
    } catch(e) {
        print(lispUneval(e));
    }
}

function lispDebug(obj) {
    if (lispDebugToggle) print("; " + obj);
}

function lispUneval(obj) {
    return uneval(obj);
}
