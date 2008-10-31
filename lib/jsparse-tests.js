// Copyright (C) 2007 Chris Double.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// DEVELOPERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

var passed = [];
var failed = [];
function assertTrue(msg, test) {
    if(test)
	passed.push(msg);
    else
	failed.push(msg);
}

function assertTrue2(test) {
    if(eval(test))
	passed.push(test);
    else
	failed.push(test);
}

function assertFalse(msg, test) {
    if(test)
	failed.push(msg);
    else
	passed.push(msg);
}

function assertEqual(msg, value1, value2) {
    if(value1 == value2) 
	passed.push(msg);
    else
	failed.push(msg);
}

function assertNotEqual(msg, value1, value2) {
    if(value1 != value2) 
	passed.push(msg);
    else
	failed.push(msg);
}

function assertFullyParsed(parser, string) {
    var msg = parser + " did not fully parse: " + string;
    try {
	var result = eval(parser)(ps(string));
	if(result && result.remaining.length == 0) 
	    passed.push(msg);
	else
	    failed.push(msg);
    }
    catch(e) {
	failed.push(msg);
    }
}

function assertParseFailed(parser, string) {
    var msg = parser + " succeeded but should have failed: " + string;
    try {
	var result = eval(parser)(ps(string));
	if(!result) 
	    passed.push(msg);
	else
	    failed.push(msg);
    }
    catch(e) {
	failed.push(msg);
    }
}

function assertParseMatched(parser, string, expected) {
    var msg = parser + " parse did not match: " + string;
    try {
	var result = eval(parser)(ps(string));
	if(result && result.matched == expected) 
	    passed.push(msg);
	else
	    failed.push(msg + " got [" + result.matched + "] expected [" + expected + "]");
    }
    catch(e) {
	failed.push(msg);
    }
}

