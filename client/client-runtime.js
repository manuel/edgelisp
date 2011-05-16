/*  EdgeLisp: A Lisp that compiles to JavaScript.
    Copyright (C) 2008-2011 by Manuel Simoni.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, version 3.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* This file contains functions required for running EdgeLisp in
   browsers.  It is loaded immediately after runtime.js is loaded, and
   before the compiler. */

function lisp_read_unit_as_progn(string)
{
    var forms = lisp_read(string);
    var progn = new Lisp_compound_form([new Lisp_identifier_form("%%progn")].concat(forms));
    return progn
}

function lisp_bif_read_unit_as_progn(_key_, string)
{
    return lisp_read_unit_as_progn(string);
}

function lisp_get_file(path)
{
    var req = new XMLHttpRequest();
    // Append UUID to file path to bypass browser cache.
    req.open("GET", path + "?" + uuid(), false);
    req.send(null);
    if(req.status == 200) {
        return req.responseText;
    } else {
        return lisp_error("XHR error", req.status);
    }
}

function lisp_compile_file(path)
{
    return lisp_compile_unit(lisp_read_unit_as_progn(lisp_get_file(path)));
}

function lisp_bif_compile_file(_key_, path)
{
    return lisp_compile_file(path);
}

function lisp_load_file(path)
{
    lisp_note("Loading " + path);
    var res = lisp_eval(lisp_read_unit_as_progn(lisp_get_file(path)));
    lisp_note("done");
    return res;
}

function lisp_bif_load_file(_key_, path)
{
    return lisp_load_file(path);
}

function lisp_print(object)
{
    document.body.appendChild(document.createTextNode(lisp_show(object)));
    document.body.appendChild(document.createElement("br"));
    return null;
    //    return _lisp_function_print(null, object);
}

function lisp_bif_note(_key_, object)
{
    return lisp_note(object);
}

function lisp_note(object)
{
    return lisp_print("; " + lisp_show(object));
}

function _lisp_function_print(_key_, object)
{
    if (console.log)
        console.log(object);
    return null;
}

lisp_export_function("%compile-file", "lisp_bif_compile_file");
lisp_export_function("%load-file", "lisp_bif_load_file");
lisp_export_function("%note", "lisp_bif_note");
lisp_export_function("%read-unit-as-progn", "lisp_bif_read_unit_as_progn");

// Additional interface functions:
// - print
