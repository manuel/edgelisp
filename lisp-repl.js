/* CyberLisp: A Lisp that compiles to JavaScript 1.5.
   
   Copyright (C) 2008 by Manuel Simoni.
   
   CyberLisp is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.
   
   CyberLisp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA. */
   
print("CyberLisp 0.26");

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
        } else if (repl_line[0] == ";") {
            continue;
        } else if ((repl_line == "") && (repl_cont == "")) {
            continue;
        }

        if (repl_cont) {
            var repl_forms = lisp_parse(repl_cont + "\n" + repl_line);
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
                repl_cont = repl_cont + "\n" + repl_line;
                continue;
            }
        }
        
        repl_debug_print(repl_forms);
        var repl_vops = repl_forms.map(lisp_compile);
        repl_debug_print(repl_vops);
        var repl_js = lisp_emit({ vopt: "progn", vops: repl_vops });
        repl_debug_print(repl_js);
        var repl_result = eval(repl_js);
        print(lisp_show(repl_result));
        
    } catch(e) {
        print(e);
        print(e.stack);
    }
}
