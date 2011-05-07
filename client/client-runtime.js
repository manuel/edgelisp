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

function lisp_read_unit_as_progn(string)
{
    var forms = lisp_read(string);
    var progn = new Lisp_compound_form([new Lisp_identifier_form("%%progn")].concat(forms));
    return progn
}

function lisp_load(path)
{
    var req = new XMLHttpRequest();
    // Append UUID to file path to bypass browser cache.
    req.open("GET", path + "?" + uuid(), false);
    req.send(null);
    if(req.status == 200) {
        lisp_eval(lisp_read_unit_as_progn(req.responseText));
    } else {
        lisp_error("XHR error", req.status);
    }
    return null;
}

function lisp_bif_load(_key_, path)
{
    return lisp_load(path);
}

function lisp_print(object)
{
    return _lisp_function_print(null, object);
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

lisp_export_function("%load", "lisp_bif_load");
lisp_export_function("%note", "lisp_bif_note");

// Additional interface functions:
// - print
