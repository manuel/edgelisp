load("lisp.js");
load("lisp-rt.js");

var repl_debug = false;

print("CyberLisp 0.1");

while(1) {
    try {
        var repl_text = readline();
        
        if (repl_text == "/d") { // debug
            repl_debug = !repl_debug;
            print("Debugging " + (repl_debug ? "ON" : "OFF"));
            continue;
        } else if (repl_text == "/r") { // reload
            load("lisp-repl.js");
        } else if (repl_text == "") { // empty line
            continue;
        }

        // Lisp evaluation
        var repl_forms = lisp_parse(repl_text);
        repl_dbg(repl_forms);
        var repl_vop = { vopt: "progn", vops: repl_forms.map(lisp_compile) };
        repl_dbg(repl_vop);
        var repl_js = lisp_emit(repl_vop);
        repl_dbg(repl_js);
        var repl_result = eval(repl_js);
        print(lisp_show(repl_result));
    } catch(e) {
        print(e);
        print(e.stack);
    }
}

function repl_dbg(obj) {
    if (repl_debug) print("; " + lisp_show(obj));
}
