load("lisp.js");
load("lisp-rt.js");
load("lib/jsparse-tests.js");

function lisp_test_all()
{
    try {
        // Utilities
        var form = {"formt":"compound",
                    "elts":[{"formt":"symbol","name":"+"},
                            {"formt":"number","n":"1"},
                            {"formt":"symbol","name":"a"}]};
        lisp_test(lisp_objects_equal(form, form));

        lisp_test(lisp_objects_equal("a", "a"));
        lisp_test(lisp_objects_equal(["a"], ["a"]));
        lisp_test(lisp_objects_equal(["a", "b"], ["a", "b"]));
        lisp_test(lisp_objects_equal({ x: ["a", "b"] }, { x: ["a", "b"] }));
        lisp_test(!lisp_objects_equal({ x: ["a", "b", [1]] }, { x: ["a", "b", [2]] }));

        lisp_test(lisp_array_contains([1,2,3], 1));
        lisp_test(lisp_array_contains([1,2,3], 2));
        lisp_test(lisp_array_contains([1,2,3], 3));
        lisp_test(!lisp_array_contains([1,2,3], 4));

        lisp_test(lisp_is_keyword_arg("foo:"));
        lisp_test(!lisp_is_keyword_arg("foo:bar"));
        lisp_test(lisp_clean_keyword_arg("foo:") == "foo");

        // Name Mangling
        lisp_test(lisp_mangle("foo-bar") == "fooHbar");
        lisp_test(lisp_mangle("foobar") == "foobar");
        lisp_test(lisp_mangle("<foo-bar>") == "LfooHbarG");
        lisp_test(lisp_mangle("<") == "L");
        lisp_test(lisp_mangle(">") == "G");
        lisp_test(lisp_mangle("%") == "N");
        lisp_test(lisp_mangle_function("%%make") == "_f_NNmake");

        // Parser
        assertFullyParsed(lisp_forms, "0");
        assertFullyParsed(lisp_forms, "-0");
        assertFullyParsed(lisp_forms, "-1");
        assertFullyParsed(lisp_forms, "1");
        assertFullyParsed(lisp_forms, "12");
        assertFullyParsed(lisp_forms, "12.234");
        assertFullyParsed(lisp_forms, "-12.234");
        assertFullyParsed(lisp_forms, "-12.0004");
        assertFullyParsed(lisp_forms, "\"\"");
        assertFullyParsed(lisp_forms, "\"a\"");
        assertFullyParsed(lisp_forms, "\"a string with a doublequote \\\" \"");
        assertFullyParsed(lisp_forms, "\"a string with a backslash \\\\ \"");
        assertFullyParsed(lisp_forms, "%");
        assertFullyParsed(lisp_forms, "a");
        assertFullyParsed(lisp_forms, "(a)");
        assertFullyParsed(lisp_forms, "(a b c)");
        assertFullyParsed(lisp_forms, " (a  b c)");
        assertFullyParsed(lisp_forms, " (a  b c) \"string\"");
        assertFullyParsed(lisp_forms, "(+ 1 2)");
        assertFullyParsed(lisp_forms, "(\"blargh\")");
        assertFullyParsed(lisp_forms, "(lambda (&key k) k)");
        assertFullyParsed(lisp_forms, "&rest");
        assertFullyParsed(lisp_forms, "key:");
        if (!(failed.length == 0)) {
            throw Error("Parser tests failed: " + failed);
        }

        // AST processing
        lisp_parse_form_test("0", { formt: "number", n: "0" });
        lisp_parse_form_test("12", { formt: "number", n: "12" });
        lisp_parse_form_test("-12", { formt: "number", n: "-12" });
        lisp_parse_form_test("-12.0", { formt: "number", n: "-12.0" });
        
        lisp_parse_form_test("\"\"", { formt: "string", s: "" });
        lisp_parse_form_test("\"foo\"", { formt: "string", s: "foo" });
        lisp_parse_form_test("\"foo\nbar\"", { formt: "string", s: "foo\nbar" });
        lisp_parse_form_test("\"foo\rbar\"", { formt: "string", s: "foo\rbar" });
        lisp_parse_form_test("\"foo\\\"bar\"", { formt: "string", s: "foo\"bar" });
        lisp_parse_form_test("\"foo\\\\bar\"", { formt: "string", s: "foo\\bar" });

        lisp_parse_form_test("abap", { formt: "symbol", name: "abap" });
        lisp_parse_form_test("abap-2", { formt: "symbol", name: "abap-2" });
        lisp_parse_form_test("*", { formt: "symbol", name: "*" });
        lisp_parse_form_test("*%-/", { formt: "symbol", name: "*%-/" });

        lisp_parse_form_test("(bar)",
                             { formt: "compound",
                               elts: [ { formt: "symbol", name: "bar" } ] });
        lisp_parse_form_test("(+ 1 a)",
                             { formt: "compound",
                               elts: [ { formt: "symbol", name: "+" },
                                       { formt: "number", n: "1" },
                                       { formt: "symbol", name: "a" } ] });
        
        // Quasiquote syntax
        lisp_parse_2_forms_test("`x", "(%%quasiquote x)");
        lisp_parse_2_forms_test("``x", "(%%quasiquote (%%quasiquote x))");
        lisp_parse_2_forms_test("`,x", "(%%quasiquote (%%unquote x))");
        lisp_parse_2_forms_test("`,@x", "(%%quasiquote (%%unquote-splicing x))");
        lisp_parse_2_forms_test("`,@(x)", "(%%quasiquote (%%unquote-splicing (x)))");
        lisp_parse_2_forms_test("`,(x)", "(%%quasiquote (%%unquote (x)))");
        lisp_parse_2_forms_test("`(x)", "(%%quasiquote (x))");
        lisp_parse_2_forms_test("`(x y z)", "(%%quasiquote (x y z))");
        lisp_parse_2_forms_test("`(x y ,(z))", "(%%quasiquote (x y (%%unquote (z))))");
        lisp_parse_2_forms_test("`(x y ,@(z zz))", "(%%quasiquote (x y (%%unquote-splicing (z zz))))");
        lisp_parse_2_forms_test("`,`z", "(%%quasiquote (%%unquote (%%quasiquote z)))");
        lisp_parse_2_forms_test("`(,`z)", "(%%quasiquote ((%%unquote (%%quasiquote z))))");

        // Number VOP
        lisp_emit_test({ vopt: "number", n: "12" }, 12);
        lisp_emit_test({ vopt: "number", n: "-12" }, -12);
        lisp_emit_test({ vopt: "number", n: "-12.34" }, -12.34);
        lisp_emit_test({ vopt: "number", n: "-0" }, 0);
        lisp_emit_test({ vopt: "number", n: "0" }, 0);
        lisp_emit_test({ vopt: "number", n: "+0" }, 0);
        
        // String VOP
        lisp_emit_test({ vopt: "string", s: "foo" }, "foo");
        lisp_emit_test({ vopt: "string", s: "foo\n" }, "foo\n");
        lisp_emit_test({ vopt: "string", s: "" }, "");
        lisp_emit_test({ vopt: "string", s: "\"" }, "\"");

        // %%defparameter
        lisp_eval("(%%defparameter foo 1)");
        lisp_test(lisp_eval("foo") == 1);

        lisp_eval("(%%defparameter foo \"bar\")");
        lisp_test(lisp_eval("foo") == "bar");

        lisp_eval("(%%defparameter quux \"bar\")");
        lisp_test(lisp_eval("quux") == "bar");

        // %%defun, %%function
        lisp_eval("(%%defun foo 2)");
        lisp_test(lisp_eval("(%%function foo)") == 2);
        lisp_test(lisp_eval("foo") == "bar");

        lisp_eval("(%%defun foo \"bar\")");
        lisp_test(lisp_eval("(%%function foo)") == "bar");

        lisp_eval("(%%defun quux \"bar\")");
        lisp_test(lisp_eval("(%%function quux)") == "bar");

        // %%progn
        lisp_test(lisp_eval("(%%progn)") == null);
        lisp_test(lisp_eval("(%%progn 1)") == 1);
        lisp_test(lisp_eval("(%%progn 0 1)") == 1);
        lisp_test(lisp_eval("(%%progn -1 0 1)") == 1);

        // %%funcall, %%lambda
        lisp_test(lisp_eval("(%%funcall (%%lambda (a) a) 1)") == 1);
        lisp_test(lisp_eval("(%%funcall (%%lambda (a b) a) 1 2)") == 1);
        lisp_test(lisp_eval("(%%funcall (%%lambda (a b) b) 1 2)") == 2);
        lisp_test(lisp_eval("(%%funcall (%%lambda (&opt a b) a) 1 2)") == 1);
        lisp_test(lisp_eval("(%%funcall (%%lambda (&opt a b) b) 1 2)") == 2);
        lisp_test(lisp_eval("(%%funcall (%%lambda (r1 r2 &opt a b) r1) 1 2 3 4)") == 1);
        lisp_test(lisp_eval("(%%funcall (%%lambda (r1 r2 &opt a b) r2) 1 2 3 4)") == 2);
        lisp_test(lisp_eval("(%%funcall (%%lambda (r1 r2 &opt a b) a) 1 2 3 4)") == 3);
        lisp_test(lisp_eval("(%%funcall (%%lambda (r1 r2 &opt a b) b) 1 2 3 4)") == 4);
        lisp_test(lisp_eval("(%%funcall (%%lambda (r1 r2 &opt a b) a) 1 2)") == null);
        lisp_test(lisp_eval("(%%funcall (%%lambda (r1 r2 &opt a b) b) 1 2)") == null);

        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (&rest r) r))"), []));
        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (&rest r) r) 1)"), [1]));
        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (&rest r) r) 1 2)"), [1, 2]));

        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (a &rest r) a) 1 2 3)"), 1));
        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (a &rest r) r) 1 2 3)"), [2, 3]));

        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (a &opt b &rest r) a) 1 2 3 4)"), 1));
        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (a &opt b &rest r) b) 1 2 3 4)"), 2));
        lisp_test(lisp_objects_equal(lisp_eval("(%%funcall (%%lambda (a &opt b &rest r) r) 1 2 3 4)"), [3, 4]));

        // %%defmacro
        lisp_eval("(%%defmacro m1 (%%lambda (form) `1))");
        lisp_test(lisp_eval("(m1)") == 1);

        // %%if
        lisp_test(lisp_eval("(%%if true true false)") == true);
        lisp_test(lisp_eval("(%%if false true false)") == false);
        lisp_test(lisp_eval("(%%if nil true false)") == false);
        
    } catch(e) {
        print(e);
        print(e.stack);
    }
}

function lisp_test(obj)
{
    if (!obj) throw Error("Test failed");
}

function lisp_emit_test(vop, result)
{
    lisp_test(eval(lisp_emit(vop)) == result);
}

function lisp_parse_form_test(input, result) {
    var ast = lisp_form(ps(input)).ast;
    if(!(lisp_objects_equal(ast, result))) {
        throw Error("Got: " + lisp_show(ast) + " expected: " + lisp_show(result));
    }
}

function lisp_parse_2_forms_test(input1, input2) {
    var ast1 = lisp_form(ps(input1)).ast;
    var ast2 = lisp_form(ps(input2)).ast;
    if(!(lisp_objects_equal(ast1, ast2))) {
        throw Error("Got: " + lisp_show(ast1) + " expected: " + lisp_show(ast2));
    }
}

/* Returns true iff two JavaScript objects are "deeply" equal. */
function lisp_objects_equal(a, b)
{
    function deep_compare_objects(a, b)
    {
        if (a.length != b.length) return false;
        for (var i in a)            
            if (!lisp_objects_equal(a[i], b[i])) return false;
        return true;
    }

    if ((typeof a) != (typeof b)) return false;

    if (typeof a == "object") 
        return deep_compare_objects(a, b);
    else
        return a == b;
}

lisp_test_all();
