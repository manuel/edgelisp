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

function lisp_load(path)
{
    lisp_note("Loading " + path, path);
    var req = new XMLHttpRequest();
    // Append UUID to file path to bypass browser cache.
    req.open("GET", path + "?" + uuid(), false);
    req.send(null);
    if(req.status == 200) {
        lisp_eval(lisp_repl_prepare(req.responseText));
    } else {
        lisp_error("XHR error", req.status);
    }
    return null;
}

function lisp_bif_load(_key_, path)
{
    return lisp_load(path);
}

lisp_export_function("%load", "lisp_bif_load");
