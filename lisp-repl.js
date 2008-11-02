print("DNALisp 0.25");

load("lisp.js");
load("lisp-rt.js");

var repl_debug = false;

function repl_debug_print(obj)
{
    if (repl_debug) print("; " + lisp_show(obj));
}

var repl_cont = "";

for(;;) {
    try {
        var repl_line = readline();
        
        if (repl_line == "/d") {
            repl_debug = !repl_debug;
            print("Debugging " + (repl_debug ? "ON" : "OFF"));
            continue;
        } else if (repl_line == "/r") {
            load("lisp-repl.js");
        } else if ((repl_line == "") && (repl_cont == "")) {
            continue;
        } else if (repl_line[0] == ";") {
            continue;
        }

        if (repl_cont) {
            var repl_forms = lisp_parse(repl_cont + repl_line);
        } else {
            var repl_forms = lisp_parse(repl_line);
        }
        
        if (repl_forms) {
            repl_cont = "";
        } else {
            if (repl_line == "") {
                print("syntax error:" + repl_cont);
                repl_cont = "";
                continue;
            } else {
                repl_cont += repl_line;
                continue;
            }
        }
        
        repl_debug_print(repl_forms);
        var repl_vop = { vopt: "progn", vops: repl_forms.map(lisp_compile) };
        repl_debug_print(repl_vop);
        var repl_js = lisp_emit(repl_vop);
        repl_debug_print(repl_js);
        var repl_result = eval(repl_js);
        print(lisp_show(repl_result));
        
    } catch(e) {
        print(e);
        print(e.stack);
    }
}
