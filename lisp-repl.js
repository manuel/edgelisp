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

/* The REPL serves two purposes:
   1) Command interpreter;
   2) Tool that reads in Lisp and emits fast-load JavaScript.
   
   Using switches, the REPL can be switched into the compiler tool
   mode, thereby enabling its use in Unix pipelines. */

load("lisp.js");
load("lisp-rt.js");

var repl_debug = false;
var repl_compile = false;
var repl_silent = false;
var repl_fasl = false;

var repl_cont = "";

var repl_beginfasl_re = new RegExp("^//beginfasl");
var repl_endfasl_re = new RegExp("^//endfasl");

for (;;) {
    try {
        var repl_line = readline();
        
        if (repl_line == "/d") {
            repl_debug = !repl_debug;
            print("Debugging " + (repl_debug ? "ON" : "OFF"));
            continue;
        } else if (repl_line == "/c") {
            repl_compile = !repl_compile;
            print(repl_compile ? "//beginfasl" : "//endfasl");
            continue;
        } else if (repl_line == "/s") {
            repl_silent = !repl_silent;
            continue;
        } else if (repl_line == "/q") {
            break;
        } else if (repl_line == "/r") {
            load("lisp-repl.js");
        } else if (repl_line[0] == ";") {
            continue;
        } else if ((repl_line == "") && (repl_cont == "")) {
            continue;
        } else if (repl_beginfasl_re.test(repl_line)) {
            repl_fasl = true;
            print("Loading FASL...");
            continue;
        } else if (repl_endfasl_re.test(repl_line)) {
            repl_fasl = false;
            print("done");
            continue;
        }

        if (repl_fasl) {
            eval(repl_line);
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
        repl_forms.map(repl_process_form);
        
    } catch(e) {
        print(e);
        print(e.stack);
    }
}

function repl_process_form(repl_form)
{
    var repl_vop = lisp_compile(repl_form);
    repl_debug_print(repl_vop);
    repl_process_vop(repl_vop);
}

function repl_process_vop(repl_vop)
{
    if (repl_vop.vopt == "progn") {
        repl_vop.vops.map(repl_process_vop);
    } else {
        var repl_js = lisp_emit(repl_vop);
        repl_debug_print(repl_js);
        if (repl_compile) {
            if (lisp_should_eval_at_compile_time(repl_vop))
                eval(repl_js);
            if (lisp_should_eval_at_load_time(repl_vop))
                print(repl_js);
        } else {
            var repl_result = eval(repl_js);
            if (!repl_silent)
                print(lisp_show(repl_result));
        }
    }
}

function repl_debug_print(obj)
{
    if (repl_debug) print("; " + lisp_show(obj));
}

