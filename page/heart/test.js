J.ready(function(){
  "use strict";
  var logI = J.logI, assert = J.assert, assertEq = J.assertEq, isntype = J.isntype;
  J.logLimit = 800;
  (function CORTEST() {
    var obj={}, noinit, nodeList=document.body.childNodes, htmlColl=document.links, arr=[0,1,2], str="321";
    J.forEach(arr, function(ai, i) { assertEq(i, ai, "forEach array " + i); });
    J.forEach(str, function(si, i) { assertEq(""+(3-i), si, "forEach string " + i); });
    J.forBack(arr, function(ai, i) { assertEq(3-i, ai, "forBack array " + i); });
    J.forBack(str, function(si, i) { assertEq(""+i, si, "forBack string " + i); });
    assertEq(undefined == null, true, "undefined == null");
    assertEq(undefined === null, false, "undefined === null false");
    assert(J.ntdefined(obj.undefinedProperty), "undefinedProperty is undefined");
    assert(J.ntdefined(noinit), "noinit variable is undefined");
    assert(J.ntdefined(undefined), "typeof undefined");
    assert(J.isnull(null), "typeof null");
    assert(J.isnan(NaN), "NaN isnan");
    assert(J.isbtype(false), "false is boolean");
    assert(J.isbtype(true), "true is boolean");
    assert(J.isntype(NaN), "NaN is number");
    assert(J.isntype(10.0), "10.0 is number");
    assert(J.isntype(1000), "1000 is number");
    assert(J.isstype("az"), "'az' is string");
    assert(J.isftype(Date), "Date is function");
    assert(J.isotype([1,2]), "[1,2] is object");
    assert(J.isatype(["s"]), "['s'] is array");
    assert(J.ntatype("s"), "'s' isn't array");
    assert(J.ntatype({i:0}), "{i:0} isn't array");
    assert(J.isalike([1]), "isalike [1]");
    assert(J.ntalike("ab"), "ntalike('ab')");
    assert(J.isalike(nodeList), "isalike nodeList " + nodeList.constructor);
    assert(J.isalike(htmlColl), "isalike htmlColl " + htmlColl.constructor);
    assert(J.contains("abc", ""), "contains abc ''");
    assert(J.contains("abc", "d") == false, "contains abc d false");
    assert(J.contains("abc", "a"), "contains abc a");
    assert(J.contains("abc", "b"), "contains abc b");
    assert(J.contains("abc", "c"), "contains abc c");
    assert(J.contains("", ""), "contains '' ''");
    assert(J.contains("", "a") == false, "contains '' a false");
    assert(J.contains([1,2,3], 0) == false, "contains [1,2,3] 0 false");
    assert(J.contains([1,2,3], 4) == false, "contains [1,2,3] 4 false");
    assert(J.contains([1,2,3], 1), "contains [1,2,3] 1");
    assert(J.contains([1,2,3], 2), "contains [1,2,3] 2");
    assert(J.contains([1,2,3], 3), "contains [1,2,3] 3");
    assert(J.sstarts("", ""), "sstarts '' ''");
    assert(J.sstarts("abc", ""), "sstarts abc ''");
    assert(J.sstarts("abc", "a"), "sstarts abc a");
    assert(J.sstarts("abc", "abc"), "sstarts abc abc");
    assert(J.sstarts("abc", "abcd") == false, "sstarts abc abcd false");
    assert(J.strends("", ""), "strends '' ''");
    assert(J.strends("abc", ""), "strends abc ''");
    assert(J.strends("abc", "c"), "strends abc c");
    assert(J.strends("abc", "abc"), "strends abc abc");
    assert(J.strends("abc", "aabc") == false, "strends abc aabc false");
    assert(J.isletter("az"), "isletter az");
    assert(J.isletter("A"), "isletter A");
    assert(J.isletter("a"), "isletter a");
    assert(J.isletter("z"), "isletter z");
    assert(J.isletter("Z"), "isletter Z");
    assert(J.isletter("AZ"), "isletter AZ");
    assert(J.ntletter(""), "ntletter ''");
    assert(J.isletter("aZbq"), "isletter aZbq");
    assert(J.isblank(" "), "isblank ' '");
    assert(J.isblank("\t"), "isblank \\t");
    assert(J.isblank("\n"), "isblank \\n");
    assert(J.isblank("\r"), "isblank \\r");
    assert(J.isblank("\f"), "isblank \\f");
    //assert(J.isblank("\v"), "isblank \\v"); // IE8 don't support \v
    assert(J.isblank("\u0009"), "isblank \\u0009");
    assert(J.isblank("\u000A"), "isblank \\u000A");
    assert(J.isblank("\u000B"), "isblank \\u000B");
    assert(J.isblank("\u000C"), "isblank \\u000C");
    assert(J.isblank("\u000D"), "isblank \\u000D");
    assert(J.isblank("\u0020"), "isblank \\u0020");
    assert(J.ntblank("0"), "ntblank 0");
    assert(J.ntblank("2"), "ntblank 2");
    assert(J.ntblank("9"), "ntblank 9");
    assert(J.ntblank("A"), "ntblank A");
    assert(J.ntblank("B"), "ntblank B");
    assert(J.ntblank("C"), "ntblank C");
    assert(J.ntblank("D"), "ntblank D");
    assert(J.ntblank("U"), "ntblank U");
    assert(J.ntblank("a"), "ntblank a");
    assert(J.ntblank("b"), "ntblank b");
    assert(J.ntblank("c"), "ntblank c");
    assert(J.ntblank("d"), "ntblank d");
    assert(J.ntblank("u"), "ntblank u");
    assert(J.ntblank("t"), "ntblank t");
    assert(J.ntblank("n"), "ntblank n");
    assert(J.ntblank("r"), "ntblank r");
    assert(J.ntblank("f"), "ntblank f");
    assert(J.ntblank("v"), "ntblank v");
    assert(J.ntblank(""), "ntblank ''");
    assertEq(J.slower("az"), "az", "slower az");
    assertEq(J.slower("AbC"), "abc", "slower AbC");
    assertEq(J.supper("az"), "AZ", "supper az");
    assertEq(J.supper("AbC"), "ABC", "supper AbC");
    assertEq(J.strim(" ab c \t \n \r "), "ab c", "strim ab c");
    assertEq(J.strim(" ab\nc \t \n \r "), "ab\nc", "strim ab\\nc");
    assertEq(J.strim(" a \t b\nc \t \n \r ", "nb"), "abc", "strim abc noblank");
    assertEq(J.nstrim(12), "12", "nstrim 12");
    assertEq(J.nstrim(1.2), "1.2", "nstrim 1.2");
    assertEq(J.nstrim(" ab\nc \t \n \r "), "ab\nc", "nstrim ab\\nc");
    assertEq(J.nstrim(" a \t b\nc \t \n \r ", "nb"), "abc", "nstrim abc noblank");
    assertEq(J.ssplit(", sf , ad f , , "), ["sf","ad f"], "ssplit ', sf , ad f , , '");
    assertEq(J.ssplit(" \r\n sf ad f \f \t"), ["sf","ad","f"], "ssplit ' sf ad f'");
    assertEq(J.ssplit(" [a] [b] [c] ", "[]"), ["a", "b", "c"], "ssplit [a] [b] [c]");
    assertEq(J.ssplit(" a : b : c ", ":"), ["a", "b", "c"], "ssplit a : b : c");
    assertEq(J.sfrags(" # id # id2 # id3 ", "#|.|[]")["#"], ["id", "id2", "id3"], "sfrags #id #id2 #id3");
    assertEq(J.sfrags(" . cl . cl2 . cl3 ", "#|.|[]")["."], ["cl", "cl2", "cl3"], "sfrags .cl .cl2 .cl3");
    assertEq(J.sfrags(" [ a=1 ] xx [ b=2 ] yy [ c=3 ] zz ", "#|.|[]")["[]"], ["a=1", "b=2", "c=3"], "sfrags a=1 b=2 c=3");
    assertEq(J.sfrags(" tag # id . cl [a=1] . cl2 [b=2] # id2 [c=3] xx [d=4]", "#|.|[]"),
      {"":["tag", "xx"], "#":["id", "id2"], ".":["cl", "cl2"], "[]":["a=1", "b=2", "c=3", "d=4"]}, "sfrags complex");
    assertEq(J.sjoin(["ab","cd","efg"]), "abcdefg", "sjoin 'ab' 'cd' 'efg'");
    assertEq(J.sjoin(["ab","cd","efg"], ""), "abcdefg", "sjoin 'ab' 'cd' 'efg' ''");
    assertEq(J.sjoin([" ", "", " "]), "  ", "sjoin ' ' '' ' '");
    assertEq(J.sjoin(["ab","cd","efg"], "."), ".ab.cd.efg", "sjoin 'ab' 'cd' 'efg' .");
    assertEq(J.sjoin(["ab","cd","efg"], " "), " ab cd efg", "sjoin 'ab' 'cd' 'efg' ' '");
    var a = 0, b = a + 1, c = b + 2, d = c + 3, s = "fortest";
    assertEq(b, 1, "a = 0, b = a + 1, ");
    assertEq(c, 3, "c = b + 2, ");
    assertEq(d, 6, "d = c + 3");
    // string string.charAt(index)
    assertEq(s[0], "f", "s[0]");
    assertEq(s[1], "o", "s[1]");
    assertEq(s.charAt(0), "f", "charAt 0");
    assertEq(s.charAt(s.length-1), "t", "charAt length-1");
    assertEq(s.charAt(30), "", "charAt out of range");
    assertEq(s.charAt(-3), "", "charAt negative pos");
    assertEq(J.schar(s, 0), "f", "schar 0");
    assertEq(J.schar(s, s.length-1), "t", "schar length-1");
    assertEq(J.schar(s, 30), "", "schar out of range");
    assertEq(J.schar(s, -3), "e", "schar negative pos");
    // int string.indexOf(substring, startIndex)
    assertEq(s.indexOf("for"), 0, "indexOf for");
    assertEq(s.indexOf("test"), 3, "indexOf test");
    assertEq(s.indexOf("fore"), -1, "indexOf not found");
    assertEq(s.indexOf("t", 3), 3, "indexOf t, 3");
    assertEq(s.indexOf("t", 4), s.length-1, "indexof t, 4");
    assertEq(s.indexOf("t", 30), -1, "indexOf too large: the string is not searched");
    assertEq(s.indexOf("t", -3), 3, "indexOf negative pos: entire string is searched");
    // int string.lastIndexOf(substring, startIndex)
    assertEq(s.lastIndexOf("t"), s.length-1, "lastIndexOf t");
    assertEq(s.lastIndexOf("t", s.length-1), s.length-1, "lastIndexOf t, length-1");
    assertEq(s.lastIndexOf("t", s.length-2), 3, "lastIndexOf t, length-2");
    assertEq(s.lastIndexOf("t", 3), 3, "lastIndexOf t, 3");
    assertEq(s.lastIndexOf("t", 2), -1, "lastIndexOf t, 2");
    assertEq(s.lastIndexOf("test"), 3, "lastIndexOf test");
    assertEq(s.lastIndexOf("for"), 0, "lastIndexOf for");
    // array string.split(delim, limit)
    assertEq(s.split(), ["fortest"], "split undefined");
    assertEq(s.split("", 4), ["f", "o", "r", "t"], "split '', 4");
    assertEq(s.split("f"), ["", "ortest"], "split f");
    assertEq(s.split("t"), ["for", "es", ""], "split t");
    // string string.substring(start, end)
    assertEq(s.substring(3), "test", "substring 3");
    assertEq(s.substring(3, 3), "", "substring 3, 3");
    assertEq(s.substring(3, s.length), "test", "substring 3,s.length");
    assertEq(s.substring(3, s.length-1), "tes", "substring 3,s.length-1");
    assertEq(s.substring(1, 3), "or", "substring 1, 3");
    assertEq(s.substring(3, 1), "or", "substring 3, 1");
    assertEq(s.substring(3, "1"), "or", "substring 3, '1'");
    assertEq(s.substring(3, "a"), "for", "substring 3, 'a'");
    assertEq(s.substring(30), "", "substring out of range");
    assertEq(s.substring(-3), "fortest", "substring negative pos: entire string returned");
    assertEq(J.ssub(s, 3), "test", "ssub 3");
    assertEq(J.ssub(s, 3, 3), "", "ssub 3, 3");
    assertEq(J.ssub(s, 3, s.length), "test", "ssub 3,s.length");
    assertEq(J.ssub(s, 3, s.length-1), "tes", "ssub 3,s.length-1");
    assertEq(J.ssub(s, 1, 3), "or", "ssub 1, 3");
    assertEq(J.ssub(s, 3, 1), "", "ssub 3, 1");
    assertEq(J.ssub(s, 3, "1"), "", "ssub 3, '1'");
    assertEq(J.ssub(s, 30), "", "ssub out of range");
    assertEq(J.ssub(s, -3), "est", "ssub negative pos");
    assertEq(J.ssub(s, -3, -1), "es", "ssub -3,-1");
    assertEq(J.ssub(s, -3, -2), "e", "ssub -3,-2");
    // boolean isnstr(s, prefix)
    assert(J.isnstr("-.1"), "isnstr -.1");
    assert(J.isnstr("+.1"),  "isnstr +.1");
    assert(J.isnstr("-1."), "isnstr -1.");
    assert(J.isnstr("+1."),  "isnstr +1.");
    assert(J.isnstr("0001.1"), "isnstr 0001.1");
    assert(J.isnstr("-001.1"), "isnstr -001.1");
    assert(J.isnstr("+001.1"), "isnstr +001.1");
    assert(J.isnstr("0002tail", "prefix"), "isnstr 0002tail");
    assert(J.isnstr("-002tail", "prefix"), "isnstr -002tail");
    assert(J.isnstr("+002tail", "prefix"), "isnstr +002tail");
    assert(J.isnstr(['12', '0.1', '1.', '.0']), "isnstr ['12', '0.1', '1.', '.0']");
    assert(J.isnstr(['12', '0.1px', '1.em', '.0ex'], "prefix"), "isnstr ['12', '0.1px', '1.em', '.0ex']");
    // string nsuffix(s)
    assertEq(J.nsuffix("-.1"), "", "nsuffix -.1");
    assertEq(J.nsuffix("+.1"),  "", "nsuffix +.1");
    assertEq(J.nsuffix("-1."), "", "nsuffix -1.");
    assertEq(J.nsuffix("+1."),  "", "nsuffix +1.");
    assertEq(J.nsuffix("0001.1"), "", "nsuffix 0001.1");
    assertEq(J.nsuffix("-001.1"), "", "nsuffix -001.1");
    assertEq(J.nsuffix("+001.1"), "", "nsuffix +001.1");
    assertEq(J.nsuffix("-.1em"), "em", "nsuffix -.1em");
    assertEq(J.nsuffix("+.1px"),  "px", "nsuffix +.1px");
    assertEq(J.nsuffix("-1.pt"), "pt", "nsuffix -1.pt");
    assertEq(J.nsuffix("+1.ex"),  "ex", "nsuffix +1.ex");
    assertEq(J.nsuffix("0001.1 px "), "px", "nsuffix 0001.1 px ");
    assertEq(J.nsuffix("-001.1 ab"), "ab", "nsuffix -001.1 ab");
    assertEq(J.nsuffix("+001.1cd "), "cd", "nsuffix +001.1cd ");
    assertEq(J.nsuffix("0002tail"), "tail", "nsuffix 0002tail");
    assertEq(J.nsuffix("-002 tail"), "tail", "nsuffix -002 tail");
    assertEq(J.nsuffix("+002tail "), "tail", "nsuffix +002tail ");
    // number nparse(s)
    assert(J.isnan(J.nparse("- 1")), "nparse - 1");
    assert(J.isnan(J.nparse("- .1")), "nparse - .1");
    assert(J.isnan(J.nparse("+ 1")), "nparse + 1");
    assert(J.isnan(J.nparse("+ .1")), "nparse + .1");
    assertEq(J.nparse("- 1", "nb"), -1, "nparse - 1 noblank");
    assertEq(J.nparse("- .1", "nb"), -0.1, "nparse - .1 noblank");
    assertEq(J.nparse("+ 1", "nb"), 1, "nparse + 1 noblank");
    assertEq(J.nparse("+ .1", "nb"), 0.1, "nparse + .1 noblank");
    assertEq(J.nparse("-.1"), -0.1, "nparse -.1");
    assertEq(J.nparse("+.1"), 0.1,  "nparse +.1");
    assertEq(J.nparse("-1."), -1, "nparse -1.");
    assertEq(J.nparse("+1."), 1,  "nparse +1.");
    assertEq(J.nparse("0001.1"), 1.1, "nparse 0001.1");
    assertEq(J.nparse("-001.1"), -1.1, "nparse -001.1");
    assertEq(J.nparse("+001.1"), 1.1, "nparse +001.1");
    assertEq(J.nparse("0002tail"), 2, "nparse 0002tail");
    assertEq(J.nparse("-002tail"), -2, "nparse -002tail");
    assertEq(J.nparse("+002tail"), 2, "nparse +002tail");
    // object and prototype
    obj.prop = null;        assert(J.isnull(obj.prop), "obj.prop isnull");
    obj.prop = undefined;   assert(J.ntdefined(obj.prop), "obj.prop ntdefined");
    obj.prop = [1];         function modify(prop) { prop = [2]; }
    modify(obj.prop); // obj.prop is a different variable from prop, so modify prop doesn't affact obj.prop
    assertEq(obj.prop, [1], "modify() o.prop");
    function getFunc() { var i = 0; function func() { return i; } i = 1; return func; }
    assertEq(getFunc()(), 1, "getFunc()() === 1");
    var Class = function() { this.a = 1; }
    Class.prototype.get = function() { return this.a; }
    var o = new Class();
    assert(o.get === Class.prototype.get, "object.get === prototype.get");
    o.get = function() { return 2; }
    assert(o.get !== Class.prototype.get, "object.get !== prototype.get");
    assert(Class.prototype.get.call(o), 1, "prototype.get.call(o)");
    assertEq(o.get(), 2, "change prop doesn't affect prototype, a new object's own prop is added to override prototype");
    function proto(o) {
      if (J.isdefined(o.__proto__)) return o.__proto__;
      if (J.isdefined(Object.getPrototypeOf)) return Object.getPrototypeOf(o);
      return null;
    }
    if (!proto(o)) { J.logW("object [[prototype]] is not checked"); return; }
    assertEq(proto(o), Class.prototype, "o.[[prototype]] == Class.prototype")
    assertEq(proto(Class.prototype), Object.prototype, "Class.prototype.[[prototype]] == Object.prototype");
    assertEq(proto(Class), Function.prototype, "Class.[[prototype]] == Function.prototype");
    assertEq(proto(Function.prototype), Object.prototype, "Function.prototype.[[prototype]] == Object.prototype");
    assertEq(proto(Function), Function.prototype, "Function.[[prototype]] == Object.prototype");
  })();
  (function BOMTEST() {
    var docElem = document.documentElement, bodyElem = document.body, htmlElem = bodyElem.parentNode;
    logI("document.compatMode " + J.mode() + " compat? " + J.compat());
    logI("screen.width/height " + J.screenW() + ", " + J.screenH());
    logI("screen.availWidth/availHeight " + J.screenAW() + ", " + J.screenAH());
    logI("screen.colorDepth " + J.colorDepth());
    logI("window.scrollX/Y " + (isntype(window.pageXOffset) ? window.pageXOffset : "undefined") + ", " +
      (isntype(window.pageYOffset) ? window.pageYOffset : "undefined"));
    logI("docElem.scrollLeft/Top " + (isntype(docElem.scrollLeft) ? docElem.scrollLeft : "undefined") + ", " +
      (isntype(docElem.scrollTop) ? docElem.scrollTop : "undefined"));
    logI("htmlElem.scrollLeft/Top " + (isntype(htmlElem.scrollLeft) ? htmlElem.scrollLeft : "undefined") + ", " +
      (isntype(htmlElem.scrollTop) ? htmlElem.scrollTop : "undefined"));
    logI("bodyElem.scrollLeft/Top " + (isntype(bodyElem.scrollLeft) ? bodyElem.scrollLeft : "undefined") + ", " +
      (isntype(bodyElem.scrollTop) ? bodyElem.scrollTop : "undefined"));
    logI("window.innerWidth/Height " + (isntype(window.innerWidth) ? window.innerWidth : "undefined") + ", " +
      (isntype(window.innerHeight) ? window.innerHeight : "undefined"));
    logI("document.clientWidth/Height " + (isntype(document.clientWidth) ? document.clientWidth : "undefined") + ", " +
      (isntype(document.clientHeight) ? document.clientHeight : "undefined"));
    logI("docElem.clientWidth/Height " + (isntype(docElem.clientWidth) ? docElem.clientWidth : "undefined") + ", " +
      (isntype(docElem.clientHeight) ? docElem.clientHeight : "undefined"));
    logI("htmlElem.clientWidth/Height " + (isntype(htmlElem.clientWidth) ? htmlElem.clientWidth : "undefined") + ", " +
      (isntype(htmlElem.clientHeight) ? htmlElem.clientHeight : "undefined"));
    logI("bodyElem.clientWidth/Height " + (isntype(bodyElem.clientWidth) ? bodyElem.clientWidth : "undefined") + ", " +
      (isntype(bodyElem.clientHeight) ? bodyElem.clientHeight : "undefined"));
    logI("docElem.clientLeft/Top " + (isntype(docElem.clientLeft) ? docElem.clientLeft : "undefined") + ", " +
      (isntype(docElem.clientTop) ? docElem.clientTop : "undefined"));
    logI("htmlElem.clientLeft/Top " + (isntype(htmlElem.clientLeft) ? htmlElem.clientLeft : "undefined") + ", " +
      (isntype(htmlElem.clientTop) ? htmlElem.clientTop : "undefined"));
    logI("bodyElem.clientLeft/Top " + (isntype(bodyElem.clientLeft) ? bodyElem.clientLeft : "undefined") + ", " +
      (isntype(bodyElem.clientTop) ? bodyElem.clientTop : "undefined"));
    var sp = J.scrollPos(), cp = J.clientPos(), vp = J.viewport(), hb = J.html().getBound(), bb = J.body().getBound(), oldtt = J.doctt();
    J.doctt("hello"); assertEq(J.doctt(), "hello", "doctt set hello");
    J.doctt(oldtt); assertEq(J.doctt(), oldtt, "doctt set old");
    logI("scroll pos " + sp.x + ", " + sp.y);
    logI("client pos " + cp.x + ", " + cp.y);
    logI("viewport size " + vp.w + ", " + vp.h);
    logI("html rect " + hb.w + ", " + hb.h + " (" + hb.x + ", " + hb.y + ") -> view (" + hb.vx + ", " + hb.vy + ")");
    logI("body rect " + bb.w + ", " + bb.h + " (" + bb.x + ", " + bb.y + ") -> view (" + bb.vx + ", " + bb.vy + ")");
    logI("cookie " + J.cookie() + ", domain " + J.domain() + ", fromUrl " + J.fromUrl());
    logI("url " + J.url());
    logI("href " + J.href());
    logI("modtime " + J.modtime());
    logI("navigator.appName " + J.appName());
    logI("navigator.appVersion " + J.appVersion());
    logI("navigator.userAgent " + J.userAgent());
    logI("navigator.platform " + J.platform());
    logI("navigator.onLine " + J.online());
    logI("navigator.geolocation " + J.geolocation());
    logI("navigator.cookieEnabled " + J.cookieEnabled());
    logI("navigator.javaEnabled() " + J.javaEnabled());
  })();
  (function DOMTEST() {
    assertEq(J.html().nodeName(), "HTML", "documentElement is HTML");
    assertEq(J.body().nodeName(), "BODY", "document.body is BODY");
    assertEq(J.head().nodeName(), "HEAD", "document.head is HEAD");
    assert(J.html().isElemNode(), "html isElemNode");
    assert(J.body().isElemNode(), "body isElemNode");
    assert(J.head().isElemNode(), "head isElemNode");
    assert(J.newElem(document).isDocNode(), "document isDocNode");
    assert(J.newElem("@tree").isDocFrag(), "newElem(@tree) isDocFrag");
    assert(J.newElem("@text").isTextNode(), "newElem(@text) isTextNode");
    assert(J.newElem("@comm").isTextNode(), "newElem(@comm) isTextNode");
    assert(J.newElem("@text").isPureText(), "newElem(@text) isPureText");
    assert(J.newElem("@comm").isCommNode(), "newElem(@comm) isCommNode");
    assertEq(J.html().ownerdoc(), document, "html ownerdoc");
    assertEq(J.body().ownerdoc(), document, "body ownerdoc");
    assertEq(J.head().ownerdoc(), document, "head ownerdoc");
    assertEq(J.newElem(document).ownerdoc(), document, "document ownerdoc");
    var div = J.elem("#debug-jstst"), count = 0, content =
      "<p class='jsdebug-c1' name='jsdebug-a1'>text aa <span>span <i>text</i> ab</span> text ac</p>" +
      "<p class='jsdebug-c2 jsdebug-c3' name='jsdebug-a2'>text ba <span name='jsdebug-a1'>span <i>text</i> bb</span> text bc</p>" +
      "<p class='jsdebug-c3 jsdebug-c1' name='jsdebug-a1'>text ca <span>span <i>text</i> cb</span> text cc</p>";
    div.nodeHtml(content);
    // element selectors
    assertEq(J.elems(".jsdebug-c1").length, 2, "select c1");
    assertEq(J.elems(".jsdebug-c2").length, 1, "select c2");
    assertEq(J.elems(".jsdebug-c3").length, 2, "select c3");
    assertEq(J.elems(".jsdebug-c4").length, 0, "select c4");
    assertEq(J.elems(".jsdebug-c1.jsdebug-c2").length, 0, "select c1c2");
    assertEq(J.elems(".jsdebug-c1.jsdebug-c3").length, 1, "select c1c3");
    assertEq(J.elems(".jsdebug-c2.jsdebug-c3").length, 1, "select c2c3");
    assertEq(J.elems(".jsdebug-c1.jsdebug-c2.jsdebug-c3").length, 0, "select c1c2c3");
    assertEq(J.elems("[name=jsdebug-a1]").length, 3, "select a1");
    assertEq(J.elems("[name=jsdebug-a2]").length, 1, "select a2");
    assertEq(J.elems("[name=jsdebug-a1][class=jsdebug-c1]").length, 1, "select a1c1");
    var a2 = J.elems("[name=jsdebug-a2]")[0], span = a2.elems("[name=jsdebug-a1]"), tagNames = [], name = "";
    assertEq(span.length, 1, "select a1 in a2 span");
    assertEq(span[0].nodeName(), "SPAN", "select a1 in a2 SPAN");
    assertEq(J.elems("[name^=jsde]").length, 4, "select [name^=jsde]");
    assertEq(J.elems("[name|=jsde]").length, 0, "select [name|=jsde]");
    assertEq(J.elems("[name|=jsdebug]").length, 4, "select [name|=jsdebug]");
    assertEq(J.elems("[name$=debug-a1]").length, 3, "select [name$=debug-a1]");
    assertEq(J.elems("[name*=sdebug-]").length, 4, "select [name*=sdebug-]");
    assertEq(J.elems("[class~=jsdebug-c3]").length, 2, "select [class!=jsdebug-c3]");
    assertEq(J.elems("p[name^=jsde]").length, 3, "select p[name^=jsde]");
    assertEq(J.elems("p[name|=jsde]").length, 0, "select p[name|=jsde]");
    assertEq(J.elems("p[name|=jsdebug]").length, 3, "select p[name|=jsdebug]");
    assertEq(J.elems("p[name$=debug-a1]").length, 2, "select p[name$=debug-a1]");
    assertEq(J.elems("p[name*=sdebug-]").length, 3, "select p[name*=sdebug-]");
    assertEq(J.elems("p[class~=jsdebug-c3]").length, 2, "select p[class!=jsdebug-c3]");
    assertEq(J.elems("div#debug-jstst").length, 1, "select div#debug-jstst");
    assertEq(J.elem("div#debug-jstst").nodeName(), "DIV", "select div#debug-jstst DIV");
    assertEq(J.elems("p.jsdebug-c1").length, 2, "select p.jsdebug-c1");
    assertEq(J.elems("p.jsdebug-c3.jsdebug-c2").length, 1, "select p.jsdebug-c3.jsdebug-c2");
    assertEq(J.elems("p.jsdebug-c2.jsdebug-c1").length, 0, "select p.jsdebug-c2.jsdebug-c1");
    assertEq(J.elem("p [name]").nodeName(), "SPAN", "select p [name]");
    assertEq(J.elems("p[name=jsdebug-a2] * ").length, 2, "select p[name=jsdebug-a2] *");
    assertEq(J.elems("p[name=jsdebug-a2] > * ").length, 1, "select p[name=jsdebug-a2] > *");
    assertEq(J.elems("p[name=jsdebug-a2] + * ").length, 1, "select p[name=jsdebug-a2] + *");
    assertEq(J.elems("p[name=jsdebug-a1] + * ").length, 2, "select p[name=jsdebug-a1] + *");
    assertEq(J.elems("span[name=jsdebug-a1] + *").length, 0, "select span[name=jsdebug-a1] + *");
    assertEq(J.elems("span[name=jsdebug-a1] > *").length, 1, "select span[name=jsdebug-a1] > *");
    assertEq(J.elems("span[name=jsdebug-a1] *").length, 1, "select span[name=jsdebug-a1] *");
    assert(J.elems(" * ").length > 8, "select *");
    J.forEach(J.elems("*"), function(ai, i) {
      if (!J.contains(tagNames, (name = ai.nodeName()))) {
        tagNames.push(name);
        logI("element[" + i + "] " + name);
      }
    });
    // element attribute
    assertEq(a2.getAttr("name"), "jsdebug-a2", "getAttr a2");
    a2.setAttr("name", "attr2");
    assertEq(a2.getAttr("name"), "attr2", "setAttr a2");
    assertEq(a2.hasAttr("name"), true, "hasAttr a2");
    a2.delAttr("name");
    assertEq(a2.hasAttr("name"), false, "delAttr a2");
    assertEq(a2.getAttr("name"), "", "getAttr of unexist");
    a2.setAttr("name", "jsdebug-a2");
    assertEq(a2.hasAttr("name"), true, "hasAttr added back");
    assertEq(a2.getAttr("name"), "jsdebug-a2", "getAttr added back");
    // element class
    assertEq(a2.hasClass("jsdebug-c1"), false, "hasClass c1");
    assertEq(a2.hasClass("jsdebug-c2"), true, "hasClass c2");
    assertEq(a2.hasClass("jsdebug-c3"), true, "hasClass c3");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "getClass c2 c3");
    a2.delClass("jsdebug-c3");
    assertEq(a2.hasClass("jsdebug-c3"), false, "delClass c3");
    assertEq(a2.getClass(), "jsdebug-c2", "getClass c2");
    a2.addClass("jsdebug-c3");
    assertEq(a2.hasClass("jsdebug-c3"), true, "addClass c3");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "getClass c2 c3 added back");
    a2.delClass("jsdebug-c2");
    assertEq(a2.hasClass("jsdebug-c2"), false, "delClass c2");
    assertEq(a2.getClass(), "jsdebug-c3", "getClass c3");
    a2.setClass(" jsdebug-c2 jsdebug-c3 ");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "setClass c2 c3");
    a2.clrClass();
    assertEq(a2.getClass(), "", "clrClass a2");
    assertEq(a2.hasAttr("class"), false, "hasAttr false after clr");
    a2.setClass("jsdebug-c2 jsdebug-c3");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "getClass after clr and set");
    // element style
    a2.addStyle(" color: red; ");
    assertEq(a2.getStyle("color"), "red", "addStyle color");
    a2.setStyle(" color:red; font-size:20pt; ");
    assertEq(a2.getStyle("color"), "red", "getStyle color");
    assertEq(a2.getStyle("font-size"), "20pt", "getStyle font-size");
    a2.addStyle(" margin:50px;");
    assertEq(a2.getStyle("margin"), "50px", "addStyle margin:50px;");
    assertEq(a2.hasStyle("color"), true, "hasStyle color");
    assertEq(a2.hasStyle("margin"), true, "hasStyle margin");
    assertEq(a2.hasStyle("font-size"), true, "hasStyle font-size");
    assertEq(a2.hasStyle("fontSize"), false, "hasStyle fontSize");
    a2.delStyle("color"); a2.delStyle("font-size");
    assertEq(a2.hasStyle("color"), false, "hasStyle color false");
    assertEq(a2.hasStyle("font-size"), false, "hasStyle font-size false");
    assertEq(a2.hasStyle("margin"), true, "hasStyle margin true");
    a2.clrStyle();
    assertEq(a2.hasStyle("margin"), false, "hasStyle margin false");
    assertEq(a2.getStyle(), "", "getStyle after clr");
    // parent and children
    assertEq(a2.parent(), div, "parent is div");
    assertEq(a2.hasChild(), true, "a2 hasChild");
    assertEq(span[0].hasChild(), true, "span hasChild");
    assertEq(span[0].frontChild(0).hasChild(), false, "span frontChild(0) has child");
    assertEq(span[0].frontChild(1).hasChild(), true, "span frontChild(1) has child false");
    assertEq(div.childCount(), 3, "children number");
    assertEq(div.frontChild(), div.firstChild(), "frontChild is 1st child");
    assertEq(div.frontChild(0), div.firstChild(), "frontChild 0 is 1st child");
    assertEq(div.frontChild(1), a2, "frontChild 1 is 2nd child");
    assertEq(div.frontChild(2), div.lastChild(), "frontChild 2 is last child");
    assertEq(div.frontChild(3), null, "frontChild 3 is null");
    assertEq(div.frontChild(2), a2.nextSibling(), "nextSibling a2");
    assertEq(div.firstChild(), a2.prevSibling(), "prevSibling a2");
    div.forElems(function(ei, i) {
      count = i;
      assertEq(ei.nodeName(), "P", "div forElems " + i);
    });
    assertEq(count+1, 3, "div forElems count");
    div.forElems(function(ei, i) {
      count = i;
      if (i == 1) return "break";
    });
    assertEq(count+1, 2, "div forElems break");
    div.elemsBack(function(ei, c) {
      if (c == 1) assertEq(ei.hasClass("jsdebug-c3"), true, "div elemsBack 1");
      if (c == 2) assertEq(ei.hasClass("jsdebug-c2"), true, "div elemsBack 2");
      if (c == 3) assertEq(ei.hasClass("jsdebug-c1"), true, "div elemsBack 3");
      count = c;
    });
    assertEq(count, 3, "div elemsBack");
    a2.childBack(function(ei, c) {
      if (c == 1) assertEq(ei.nodeText(), " text bc", "a2 childBack 1");
      if (c == 2) assertEq(ei.nodeText(), "span text bb", "a2 childBack 2");
      if (c == 3) assertEq(ei.nodeText(), "text ba ", "a2 childBack 3");
      count = c;
    });
    assertEq(count, 3, "a2 childBack");
    a2.forNext(function(ei, i) {
      count = i;
      assertEq(ei.hasClass(".jsdebug-c1.jsdebug-c3"), true, "a2 forNext");
    });
    assertEq(count+1, 1, "a2 forNext count");
    a2.forPrev(function(ei, i) {
      count = i;
      assertEq(ei.getClass(), "jsdebug-c1", "a2 forPrev getClass");
      assertEq(ei.hasAttr("name=jsdebug-a1"), true, "a2 hasAttr name=jsdebug-a1");
      assertEq(ei.hasAttr("[name=jsdebug-a1] [class=jsdebug-c1]"), true, "a2 hasAttr a1 c1");
    });
    span = span[0]; // <span name='jsdebug-a1'>span <i>text</i> bb</span>
    span.forChild(function(ei, i) {
      count = i;
      if (i == 0) assertEq(ei.nodeText(), "span ", "span forChild 0");
      if (i == 1) assertEq(ei.nodeName(), "I", "span forChild 1 nodeName");
      if (i == 1) assertEq(ei.nodeText(), "text", "span forChild 1 nodeText");
      if (i == 2) assertEq(ei.nodeText(), " bb", "span forChild 2");
    });
    assertEq(count+1, 3, "span forChild count");
    span.forElems(function(ei, i) {
      count = i;
      assertEq(ei.nodeName(), "I", "span forElems 0");
    });
    assertEq(count+1, 1, "span forElems count");
    span.childBack(function(ei, c) {
      count = c;
      if (c == 1) assertEq(ei.nodeText(), " bb", "span forBack 1");
      if (c == 2) assertEq(ei.nodeText(), "text", "span forBack 2");
      if (c == 3) assertEq(ei.nodeText(), "span ", "span forBack 3");
    });
    assertEq(count, 3, "span forBack count");
    assertEq(span.childCount(), 3, "span childCount");
    assertEq(span.elemCount(), 1, "span elemCount");
    assertEq(span.nextElems().length, 0, "span nextElems");
    assertEq(span.prevElems().length, 0, "span prevElems");
    assertEq(a2.childNodes().length, 3, "a2 childNodes");
    assertEq(a2.elemNodes().length, 1, "a2 elemNodes");
    assertEq(a2.textNodes().length, 2, "a2 textNodes");
    assertEq(a2.nextElems().length, 1, "a2 nextElems");
    assertEq(a2.prevElems().length, 1, "a2 prevElems");
    // add and del node
    div.delChild(a2);
    assertEq(div.childCount(), 2, "delChild a2");
    div.delChild(1);
    assertEq(div.childCount(), 1, "delChild 1");
    assertEq(div.firstChild().nodeText(), "text aa span text ab text ac", "nodeText after delChild");
    div.addChild(J.newElem("p", "text ca span text cb text cc"));
    assertEq(div.childCount(), 2, "addChild p3");
    div.addChild(J.newElem("p", "text ba span text bb text bc"), 1);
    assertEq(div.childCount(), 3, "addChild p2");
    assertEq(div.frontChild(1).nodeText(), "text ba span text bb text bc", "nodeText after addChild 1");
    div.delChild(0);
    assertEq(div.childCount(), 2, "delChild 0");
    var p1 = J.newElem("p"), p2 = J.newElem("p", "paragraph 2");
    div.addChild(p1, 0);
    p1.addChild(J.newElem("@text", "text aa span text aa text aa"));
    assertEq(p1.nodeText(), "text aa span text aa text aa", "nodeText after addChild 0");
    div.addBefore(p2, p1);
    assertEq(div.firstChild(), p2, "addBefore p1");
    div.addAfter(p2, p1);
    assertEq(div.frontChild(1), p2, "addAfter p1");
    assertEq(div.childCount(), 4, "addAfter p1 count");
    div.repChild(p1, p2);
    assertEq(div.childCount(), 3, "repChild p2");
    assertEq(div.frontChild(1).nodeText(), "text ba span text bb text bc", "repChild p2 check");
    div.repChild(p1, p1);
    assertEq(div.childCount(), 3, "repChild p1 p1");
    div.repChild(p1, 1);
    assertEq(div.childCount(), 2, "repChild p1 1");
    div.repChild(p1, 2);
    assertEq(div.childCount(), 2, "repChild p1 2");
    assertEq(div.childIndex(p1), 0, "childIndex p1");
    assertEq(div.childIndex(div.lastChild()), 1, "childIndex last");
    div.lastChild().clrNodes();
    assertEq(div.lastChild().nodeText(), "", "lastChild clrNodes");
    div.clrNodes();
    assertEq(div.childCount(), 0, "div clrNodes");
    div.addChild(p1);
    assertEq(div.childCount(), 1, "childCount div after clr and add");
    div.nodeHtml(null);
    assertEq(div.childCount(), 0, "nodeHtml set null");
    // for in-memory but not in-tree nodes
    var div = J.newElem("div"), count = 0, content =
      "<p class='jsdebug-c1' name='jsdebug-a1'>text aa <span>span <i>text</i> ab</span> text ac</p>" +
      "<p class='jsdebug-c2 jsdebug-c3' name='jsdebug-a2'>text ba <span name='jsdebug-a1'>span <i>text</i> bb</span> text bc</p>" +
      "<p class='jsdebug-c3 jsdebug-c1' name='jsdebug-a1'>text ca <span>span <i>text</i> cb</span> text cc</p>";
    div.nodeHtml(content);
    // element selectors
    assertEq(div.elems(".jsdebug-c1").length, 2, "inmem select c1");
    assertEq(div.elems(".jsdebug-c2").length, 1, "inmem select c2");
    assertEq(div.elems(".jsdebug-c3").length, 2, "inmem select c3");
    assertEq(div.elems(".jsdebug-c4").length, 0, "inmem select c4");
    assertEq(div.elems(".jsdebug-c1.jsdebug-c2").length, 0, "inmem select c1c2");
    assertEq(div.elems(".jsdebug-c1.jsdebug-c3").length, 1, "inmem select c1c3");
    assertEq(div.elems(".jsdebug-c2.jsdebug-c3").length, 1, "inmem select c2c3");
    assertEq(div.elems(".jsdebug-c1.jsdebug-c2.jsdebug-c3").length, 0, "inmem select c1c2c3");
    assertEq(div.elems("[name=jsdebug-a1]").length, 3, "inmem select a1");
    assertEq(div.elems("[name=jsdebug-a2]").length, 1, "inmem select a2");
    assertEq(div.elems("[name=jsdebug-a1][class=jsdebug-c1]").length, 1, "inmem select a1c1");
    var a2 = div.elems("[name=jsdebug-a2]")[0], span = a2.elems("[name=jsdebug-a1]"), tagNames = [], name = "";
    assertEq(span.length, 1, "inmem select a1 in a2 span");
    assertEq(span[0].nodeName(), "SPAN", "inmem select a1 in a2 SPAN");
    assertEq(div.elems("[name^=jsde]").length, 4, "inmem select [name^=jsde]");
    assertEq(div.elems("[name|=jsde]").length, 0, "inmem select [name|=jsde]");
    assertEq(div.elems("[name|=jsdebug]").length, 4, "inmem select [name|=jsdebug]");
    assertEq(div.elems("[name$=debug-a1]").length, 3, "inmem select [name$=debug-a1]");
    assertEq(div.elems("[name*=sdebug-]").length, 4, "inmem select [name*=sdebug-]");
    assertEq(div.elems("[class~=jsdebug-c3]").length, 2, "inmem select [class!=jsdebug-c3]");
    assertEq(div.elems("p[name^=jsde]").length, 3, "inmem select p[name^=jsde]");
    assertEq(div.elems("p[name|=jsde]").length, 0, "inmem select p[name|=jsde]");
    assertEq(div.elems("p[name|=jsdebug]").length, 3, "inmem select p[name|=jsdebug]");
    assertEq(div.elems("p[name$=debug-a1]").length, 2, "inmem select p[name$=debug-a1]");
    assertEq(div.elems("p[name*=sdebug-]").length, 3, "inmem select p[name*=sdebug-]");
    assertEq(div.elems("p[class~=jsdebug-c3]").length, 2, "inmem select p[class!=jsdebug-c3]");
    assertEq(div.elems("p.jsdebug-c1").length, 2, "inmem select p.jsdebug-c1");
    assertEq(div.elems("p.jsdebug-c3.jsdebug-c2").length, 1, "inmem select p.jsdebug-c3.jsdebug-c2");
    assertEq(div.elems("p.jsdebug-c2.jsdebug-c1").length, 0, "inmem select p.jsdebug-c2.jsdebug-c1");
    assertEq(div.elem("p [name]").nodeName(), "SPAN", "inmem select p [name]");
    assertEq(div.elems("p[name=jsdebug-a2] * ").length, 2, "inmem select p[name=jsdebug-a2] *");
    assertEq(div.elems("p[name=jsdebug-a2] > * ").length, 1, "inmem select p[name=jsdebug-a2] > *");
    assertEq(div.elems("p[name=jsdebug-a2] + * ").length, 1, "inmem select p[name=jsdebug-a2] + *");
    assertEq(div.elems("p[name=jsdebug-a1] + * ").length, 2, "inmem select p[name=jsdebug-a1] + *");
    assertEq(div.elems("span[name=jsdebug-a1] + *").length, 0, "inmem select span[name=jsdebug-a1] + *");
    assertEq(div.elems("span[name=jsdebug-a1] > *").length, 1, "inmem select span[name=jsdebug-a1] > *");
    assertEq(div.elems("span[name=jsdebug-a1] *").length, 1, "inmem select span[name=jsdebug-a1] *");
    assert(div.elems(" * ").length > 8, "inmem select *");
    // element attribute
    assertEq(a2.getAttr("name"), "jsdebug-a2", "inmem getAttr a2");
    a2.setAttr("name", "attr2");
    assertEq(a2.getAttr("name"), "attr2", "inmem setAttr a2");
    assertEq(a2.hasAttr("name"), true, "inmem hasAttr a2");
    a2.delAttr("name");
    assertEq(a2.hasAttr("name"), false, "inmem delAttr a2");
    assertEq(a2.getAttr("name"), "", "inmem getAttr of unexist");
    a2.setAttr("name", "jsdebug-a2");
    assertEq(a2.hasAttr("name"), true, "inmem hasAttr added back");
    assertEq(a2.getAttr("name"), "jsdebug-a2", "inmem getAttr added back");
    // element class
    assertEq(a2.hasClass("jsdebug-c1"), false, "inmem hasClass c1");
    assertEq(a2.hasClass("jsdebug-c2"), true, "inmem hasClass c2");
    assertEq(a2.hasClass("jsdebug-c3"), true, "inmem hasClass c3");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "inmem getClass c2 c3");
    a2.delClass("jsdebug-c3");
    assertEq(a2.hasClass("jsdebug-c3"), false, "inmem delClass c3");
    assertEq(a2.getClass(), "jsdebug-c2", "inmem getClass c2");
    a2.addClass("jsdebug-c3");
    assertEq(a2.hasClass("jsdebug-c3"), true, "inmem addClass c3");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "inmem getClass c2 c3 added back");
    a2.delClass("jsdebug-c2");
    assertEq(a2.hasClass("jsdebug-c2"), false, "inmem delClass c2");
    assertEq(a2.getClass(), "jsdebug-c3", "inmem getClass c3");
    a2.setClass(" jsdebug-c2 jsdebug-c3 ");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "inmem setClass c2 c3");
    a2.clrClass();
    assertEq(a2.getClass(), "", "inmem clrClass a2");
    assertEq(a2.hasAttr("class"), false, "inmem hasAttr false after clr");
    a2.setClass("jsdebug-c2 jsdebug-c3");
    assertEq(a2.getClass(), "jsdebug-c2 jsdebug-c3", "inmem getClass after clr and set");
    // element style
    a2.addStyle(" color: red; ");
    assertEq(a2.getStyle("color"), "red", "inmem addStyle color");
    a2.setStyle(" color:red; font-size:20pt; ");
    assertEq(a2.getStyle("color"), "red", "inmem getStyle color");
    assertEq(a2.getStyle("font-size"), "20pt", "inmem getStyle font-size");
    a2.addStyle(" margin:50px;");
    assertEq(a2.getStyle("margin"), "50px", "inmem addStyle margin:50px;");
    assertEq(a2.hasStyle("color"), true, "inmem hasStyle color");
    assertEq(a2.hasStyle("margin"), true, "inmem hasStyle margin");
    assertEq(a2.hasStyle("font-size"), true, "inmem hasStyle font-size");
    assertEq(a2.hasStyle("fontSize"), false, "inmem hasStyle fontSize");
    a2.delStyle("color"); a2.delStyle("font-size");
    assertEq(a2.hasStyle("color"), false, "inmem hasStyle color false");
    assertEq(a2.hasStyle("font-size"), false, "inmem hasStyle font-size false");
    assertEq(a2.hasStyle("margin"), true, "inmem hasStyle margin true");
    a2.clrStyle();
    assertEq(a2.hasStyle("margin"), false, "inmem hasStyle margin false");
    assertEq(a2.getStyle(), "", "inmem getStyle after clr");
    // parent and children
    assertEq(a2.parent(), div, "inmem parent is div");
    assertEq(a2.hasChild(), true, "inmem a2 hasChild");
    assertEq(span[0].hasChild(), true, "inmem span hasChild");
    assertEq(span[0].frontChild(0).hasChild(), false, "inmem span frontChild(0) has child");
    assertEq(span[0].frontChild(1).hasChild(), true, "inmem span frontChild(1) has child false");
    assertEq(div.childCount(), 3, "inmem children number");
    assertEq(div.frontChild(), div.firstChild(), "inmem frontChild is 1st child");
    assertEq(div.frontChild(0), div.firstChild(), "inmem frontChild 0 is 1st child");
    assertEq(div.frontChild(1), a2, "inmem frontChild 1 is 2nd child");
    assertEq(div.frontChild(2), div.lastChild(), "inmem frontChild 2 is last child");
    assertEq(div.frontChild(3), null, "inmem frontChild 3 is null");
    assertEq(div.frontChild(2), a2.nextSibling(), "inmem nextSibling a2");
    assertEq(div.firstChild(), a2.prevSibling(), "inmem prevSibling a2");
    div.forElems(function(ei, i) {
      count = i;
      assertEq(ei.nodeName(), "P", "inmem div forElems " + i);
    });
    assertEq(count+1, 3, "inmem div forElems count");
    div.forElems(function(ei, i) {
      count = i;
      if (i == 1) return "break";
    });
    assertEq(count+1, 2, "inmem div forElems break");
    div.elemsBack(function(ei, c) {
      if (c == 1) assertEq(ei.hasClass("jsdebug-c3"), true, "inmem div elemsBack 1");
      if (c == 2) assertEq(ei.hasClass("jsdebug-c2"), true, "inmem div elemsBack 2");
      if (c == 3) assertEq(ei.hasClass("jsdebug-c1"), true, "inmem div elemsBack 3");
      count = c;
    });
    assertEq(count, 3, "inmem div elemsBack");
    a2.childBack(function(ei, c) {
      if (c == 1) assertEq(ei.nodeText(), " text bc", "inmem a2 childBack 1");
      if (c == 2) assertEq(ei.nodeText(), "span text bb", "inmem a2 childBack 2");
      if (c == 3) assertEq(ei.nodeText(), "text ba ", "inmem a2 childBack 3");
      count = c;
    });
    assertEq(count, 3, "inmem a2 childBack");
    a2.forNext(function(ei, i) {
      count = i;
      assertEq(ei.hasClass(".jsdebug-c1.jsdebug-c3"), true, "inmem a2 forNext");
    });
    assertEq(count+1, 1, "inmem a2 forNext count");
    a2.forPrev(function(ei, i) {
      count = i;
      assertEq(ei.getClass(), "jsdebug-c1", "inmem a2 forPrev getClass");
      assertEq(ei.hasAttr("name=jsdebug-a1"), true, "inmem a2 hasAttr name=jsdebug-a1");
      assertEq(ei.hasAttr("[name=jsdebug-a1] [class=jsdebug-c1]"), true, "inmem a2 hasAttr a1 c1");
    });
    span = span[0]; // <span name='jsdebug-a1'>span <i>text</i> bb</span>
    span.forChild(function(ei, i) {
      count = i;
      if (i == 0) assertEq(ei.nodeText(), "span ", "inmem span forChild 0");
      if (i == 1) assertEq(ei.nodeName(), "I", "inmem span forChild 1 nodeName");
      if (i == 1) assertEq(ei.nodeText(), "text", "inmem span forChild 1 nodeText");
      if (i == 2) assertEq(ei.nodeText(), " bb", "inmem span forChild 2");
    });
    assertEq(count+1, 3, "inmem span forChild count");
    span.forElems(function(ei, i) {
      count = i;
      assertEq(ei.nodeName(), "I", "inmem span forElems 0");
    });
    assertEq(count+1, 1, "inmem span forElems count");
    span.childBack(function(ei, c) {
      count = c;
      if (c == 1) assertEq(ei.nodeText(), " bb", "inmem span forBack 1");
      if (c == 2) assertEq(ei.nodeText(), "text", "inmem span forBack 2");
      if (c == 3) assertEq(ei.nodeText(), "span ", "inmem span forBack 3");
    });
    assertEq(count, 3, "inmem span forBack count");
    assertEq(span.childCount(), 3, "inmem span childCount");
    assertEq(span.elemCount(), 1, "inmem span elemCount");
    assertEq(span.nextElems().length, 0, "inmem span nextElems");
    assertEq(span.prevElems().length, 0, "inmem span prevElems");
    assertEq(a2.childNodes().length, 3, "inmem a2 childNodes");
    assertEq(a2.elemNodes().length, 1, "inmem a2 elemNodes");
    assertEq(a2.textNodes().length, 2, "inmem a2 textNodes");
    assertEq(a2.nextElems().length, 1, "inmem a2 nextElems");
    assertEq(a2.prevElems().length, 1, "inmem a2 prevElems");
    // add and del node
    div.delChild(a2);
    assertEq(div.childCount(), 2, "inmem delChild a2");
    div.delChild(1);
    assertEq(div.childCount(), 1, "inmem delChild 1");
    assertEq(div.firstChild().nodeText(), "text aa span text ab text ac", "inmem nodeText after delChild");
    div.addChild(J.newElem("p", "inmem text ca span text cb text cc"));
    assertEq(div.childCount(), 2, "inmem addChild p3");
    div.addChild(J.newElem("p", "text ba span text bb text bc"), 1);
    assertEq(div.childCount(), 3, "inmem addChild p2");
    assertEq(div.frontChild(1).nodeText(), "text ba span text bb text bc", "inmem nodeText after addChild 1");
    div.delChild(0);
    assertEq(div.childCount(), 2, "inmem delChild 0");
    var p1 = J.newElem("p"), p2 = J.newElem("p", "paragraph 2");
    div.addChild(p1, 0);
    p1.addChild(J.newElem("@text", "text aa span text aa text aa"));
    assertEq(p1.nodeText(), "text aa span text aa text aa", "inmem nodeText after addChild 0");
    div.addBefore(p2, p1);
    assertEq(div.firstChild(), p2, "inmem addBefore p1");
    div.addAfter(p2, p1);
    assertEq(div.frontChild(1), p2, "inmem addAfter p1");
    assertEq(div.childCount(), 4, "inmem addAfter p1 count");
    div.repChild(p1, p2);
    assertEq(div.childCount(), 3, "inmem repChild p2");
    assertEq(div.frontChild(1).nodeText(), "text ba span text bb text bc", "inmem repChild p2 check");
    div.repChild(p1, p1);
    assertEq(div.childCount(), 3, "inmem repChild p1 p1");
    div.repChild(p1, 1);
    assertEq(div.childCount(), 2, "inmem repChild p1 1");
    div.repChild(p1, 2);
    assertEq(div.childCount(), 2, "inmem repChild p1 2");
    assertEq(div.childIndex(p1), 0, "inmem childIndex p1");
    assertEq(div.childIndex(div.lastChild()), 1, "inmem childIndex last");
    div.lastChild().clrNodes();
    assertEq(div.lastChild().nodeText(), "", "inmem lastChild clrNodes");
    div.clrNodes();
    assertEq(div.childCount(), 0, "inmem div clrNodes");
    div.addChild(p1);
    assertEq(div.childCount(), 1, "inmem childCount div after clr and add");
    div.nodeHtml(null);
    assertEq(div.childCount(), 0, "inmem nodeHtml set null");
  })();
  (function EVTTEST() {
    var ajax = J.newAjax(function(self, error) {
      if (error) J.logW(error);
      else J.logI("ajax test: " + self.rspData);
    });
    J.elem("#debug-jsbtn").addEvent("click", function(e, btn) {
      var s = J.strim(J.elem("#debug-jsipt").value());
      if (J.sstarts(s, "ajax:")) ajax.send("/ajax.test?" + J.ssub(s, 5));
      else if (J.sstarts(s, "event:clear")) { btn.clrEvent("click"); logI("click event cleared"); }
    });
  })();
  (function SVGTEST() {
    var svg = J.newSvg(200, 300);
    if (!svg.$node) { J.logW("svg unsupported"); return; }
    assertEq(svg.nodeName(), "SVG", "svg nodeName");
    assertEq(svg.getAttr("xmlns"), J.svgns, "svg xmlns");
    assertEq(svg.getAttr("xmlns:xlink"), J.xlink, "svg xmlns:xlink");
    assertEq(svg.getAttr("width"), "200", "svg width 200");
    assertEq(svg.getAttr("height"), "300", "svg height 300");
    svg.title("svg title hello");
    svg.desc("svg desc info");
    var desc = svg.desc("svg desc info new"),
        title = svg.title("svg title new"),
        line = svg.line(10, 10, 150, 150).setStyle("stroke:red;stroke-width:3;"),
        rect = svg.rectFr(60, 60, 100, 100).setRound(20, 20),
        path = svg.path("M0 10 10 100 50 150 180 80 130 30");
    assertEq(desc.nodeName(), "DESC", "desc nodeName");
    assertEq(title.nodeName(), "TITLE", "title nodeName");
    assertEq(line.nodeName(), "LINE", "line nodeName");
    assertEq(rect.nodeName(), "RECT", "rect nodeName");
    assertEq(path.nodeName(), "PATH", "path nodeName");
    assertEq(line.getStyle("stroke"), "red", "line getStyle stroke");
    assertEq(line.getStyle("stroke-width"), "3", "line getStyle stroke-width");
    svg.render(J.elem("#debug-jstst"));
    var polyline = svg.polyline("30 80 105 200 130 104"),
        circle = svg.circle(100, 100, 40),
        ellipse = svg.ellipseFr(80, 200, 50, 80),
        polygon = svg.polygon("180 50 50 150 100 200 130 50");
    svg.render();
    assertEq(polyline.nodeName(), "POLYLINE", "polyline nodeName");
    assertEq(circle.nodeName(), "CIRCLE", "circle nodeName");
    assertEq(ellipse.nodeName(), "ELLIPSE", "ellipse nodeName");
    assertEq(polygon.nodeName(), "POLYGON", "polygon nodeName");
    assertEq(J.snoblk(line.getCStyle("stroke")), "rgb(255,0,0)", "line getCStyle");
  })();
});
