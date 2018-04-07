(function(){
  "use strict";
  var EARG = "invalid argument ", EDEF = "property undefined";
  function logger(level, tag, detail) {
    if (!J || !J.logLevel || level > J.logLevel) return;
    var e = document.getElementById("debug-jslog");
    if (!e || J.logCount > J.logLimit) return;
    var line = document.createElement("p");
    line.className = "jslog-level" + level;
    line.innerHTML = tag + (detail ? (": " + detail) : "");
    e.appendChild(line);
    J.logCount += 1;
  }
  // 1:error, 2:warning, 3:flow, 4:info, 5:debug, 6:verbose
  function logE(tag, detail) { logger(1, "[E] " + tag, detail) }
  function logW(tag, detail) { logger(2, "[W] " + tag, detail) }
  function logF(tag, detail) { logger(3, "[F] " + tag, detail) }
  function logI(tag, detail) { logger(4, "[I] " + tag, detail) }
  function logD(tag, detail) { logger(5, "[D] " + tag, detail) }
  function logV(tag, detail) { logger(6, "[V] " + tag, detail) }
  function isstype(s) { return typeof s == "string"; }
  function ntstype(s) { return typeof s != "string"; }
  function isbtype(b) { return typeof b == "boolean"; }
  function ntbtype(b) { return typeof b != "boolean"; }
  function isntype(n) { return typeof n == "number"; }
  function ntntype(n) { return typeof n != "number"; }
  function isftype(f) { return typeof f == "function"; }
  function ntftype(f) { return typeof f != "function"; }
  function isotype(o) { return (!!o && typeof o == "object"); }
  function ntotype(o) { return (!o || typeof o != "object"); }
  function ntdefined(d) { return typeof d == "undefined"; }
  function isdefined(d) { return typeof d != "undefined"; }
  function isnull(n) { return n === null; }
  function ntnull(n) { return n !== null; }
  function isnan(n) { return n !== n; }
  function ntnan(n) { return n === n; }
  function isatype(a) {
    if (Array.isArray) return Array.isArray(a);
    return isotype(a) && Object.prototype.toString.call(a) == "[object Array]";
  }
  function isalike(a) {
    function istype(o, name) {
      var name = "[object " + name + "]";
      return Object.prototype.toString.call(o) == name || o.constructor.toString() == name;
    }
    if (ntotype(a)) return false; // NodeList and HTMLCollection don't have propertyIsEnumerable in IE8
    if (isntype(a.length) && (a.propertyIsEnumerable) && !a.propertyIsEnumerable("length")) return true;
    return istype(a, "NodeList") || istype(a, "HTMLCollection");
  }
  function ntatype(a) { return !isatype(a); }
  function ntalike(a) { return !isalike(a); }
  function ForEach(o, f) {
    var i = 0, name = null;
    if (isalike(o) || isstype(o)) {
      for (; i < o.length; ++i) {
        if (f(o[i], i) == "break") break;
      }
      return;
    }
    if (ntotype(o)) return;
    for (name in o) {
      if (!o.hasOwnProperty(name)) continue;
      if (f(o[name], name) == "break") break;
    }
  }
  function ForBack(a, f) {
    if (ntalike(a) && ntstype(a)) return;
    var i = a.length-1, count = 1;
    for (; i >= 0; --i, ++count) {
      if (f(a[i], count) == "break") break;
    }
  }
  function ForNext(o, prop, f) {
    if (ntotype(o) || ntstype(prop)) return;
    var e = o[prop], i = 0;
    for(; e; e = e[prop], ++i) {
      if (f(e, i) == "break") break;
    }
  }
  function assert(x, m) {
    if (x) { logD("assert pass", m); return true; }
    logE("assert fail", m);
    return false;
  }
  function assertEq(a, b, m) {
    if (a === b || ((isbtype(a) || isbtype(b)) && a == b)) { logD("assertEq pass", m); return true; }
    if (isatype(a) && isatype(b) && a.length == b.length) {
      for (var i = 0; i < a.length; ++i) {
        if (a[i] !== b[i]) { logE("assertEq fail [" + i + "] " + a[i] + "!=" + b[i], m); return false; }
      }
      logD("assertEq pass", m);
      return true;
    }
    if (isotype(a) && isotype(b)) {
      if (isdefined(a.$node) && a.$node === b.$node) { logD("assertEq pass", m); return true; }
      var eq = true;
      ForEach(a, function(v, n) {
        if (ntdefined(b[n])) { logE("assertEq fail b.'" + n + "' undefined", m); eq = false; return "break"; };
        if (!assertEq(v, b[n])) { eq = false; return "break"; }
      });
      if (!eq) return false;
      ForEach(b, function(v, n) {
        if (ntdefined(a[n])) { logE("assertEq fail a.'" + n + "' undefined", m); eq = false; return "break"; }
        if (!assert(a[n], v)) { eq = false; return "break"; }
      });
      return eq;
    }
    logE("assertEq fail " + a + "!=" + b, m);
    return false;
  }
  function contains(s, sub) {
    if (isstype(s)) {
      if (ntstype(sub)) return 0;
      var idx = s.indexOf(sub);
      if (idx == -1) return 0;
      return idx+1;
    }
    if (!s || ntntype(s.length) || s.length == 0) return 0;
    for (var i = 0; i < s.length; ++i) {
      if (s[i] === sub) return i+1;
    }
    return 0;
  }
  function schar(s, i) {
    if (ntstype(s) || ntntype(i)) { logW("schar", EARG); return ""; }
    if (i < 0) i = s.length + i;
    if (i >= 0 && i < s.length) return s.charAt(i);
    return "";
  }
  function sidx(s, sub) {
    if (ntstype(s) || ntstype(sub)) { logW("sidx", EARG); return -1; }
    return s.indexOf(sub);
  }
  function ssub(s, i, j) {
    if (ntstype(s) || ntntype(i)) { logW("ssub", EARG); return ""; }
    if (i < 0) i = s.length + i;
    var j = ntdefined(j) ? s.length : j;
    if (ntntype(j)) return "";
    if (j < 0) j = s.length + j;
    if (i >= 0 && j >= i && j <= s.length) return s.substring(i, j);
    return "";
  }
  function sstarts(s, sub) {
    if (ntstype(s) || ntstype(sub)) return false;
    if (sub.length > s.length) return false;
    for (var i = 0; i < sub.length; ++i) {
      if (sub[i] != s[i]) return false;
    }
    return true;
  }
  function strends(s, sub) {
    if (ntstype(s) || ntstype(sub)) return false;
    if (sub.length > s.length) return false;
    var i = sub.length-1, j = s.length-1;
    for(; i >= 0; --i, --j) {
      if (sub[i] != s[j]) return false;
    }
    return true;
  }
  function slower(s) {
    if (ntstype(s)) return "";
    return s.toLowerCase();
  }
  function supper(s) {
    if (ntstype(s)) return "";
    return s.toUpperCase();
  }
  function isletter(s) {
    if (ntstype(s) || !s) return false;
    for (var i = 0; i < s.length; ++i) {
      if (s[i] >= 'a' && s[i] <= 'z' || s[i] >= 'A' && s[i] <= 'Z') continue;
      return false;
    }
    return true;
  }
  var Blanks = "\u0009\u000A\u000B\u000C\u000D\u0020"; // "\t\n\v\f\r ", IE8 don't support \v
  function isblank(s) {
    if (ntstype(s) || !s) return false;
    for (var i = 0; i < s.length; ++i) {
      if (!contains(Blanks, s[i])) return false;
    }
    return true;
  }
  function ntletter(s) { return !isletter(s); }
  function ntblank(s) { return !isblank(s); }
  function strim(s, noblank) {
    if (!isstype(s) || !s) { return ""; }
    var i = 0, j = 0;
    if (noblank == "nb") {
      var r = "";
      for (i = 0; i < s.length; ++i) { if (!contains(Blanks, s[i])) r += s[i]; }
      return r;
    }
    for (i = 0; i < s.length; ++i) { if (!contains(Blanks, s[i])) break; }
    if (i == s.length) return "";
    for (j = s.length - 1; j >= i; --j) { if (!contains(Blanks, s[j])) break; }
    return ssub(s, i, j+1);
  }
  function snoblk(s) { return strim(s, "nb"); }
  function nstrim(v, noblank) {
    if (isntype(v)) return "" + v;
    if (isstype(v)) return strim(v, noblank);
    return "";
  }
  function ssplit(s, d) {
    if (!isstype(s) || !s) { return []; }
    s = strim(s);
    if (s.length == 0) return [];
    var parts = [], dest = [], i = 0, name = "", ch = s[0];
    if (isstype(d) && (d = strim(d))) {
      if (d == "[]" || d == "()" || d == "{}" || d == "<>") {
        var beg = d[0], end = d[1], subi = 0;
        for (i = 0; i < s.length; ++i) {
          while (i < s.length && s[i] != beg) ++i;
          if (i == s.length) break;
          subi = i + 1;
          while (i < s.length && s[i] != end) ++i;
          if (i == s.length) break;
          parts.push(ssub(s, subi, i));
        }
      } else {
        parts = s.split(d);
      }
    } else if (contains("|.,=:", ch)) {
      if (s.length == 1) return [];
      parts = s.split(ch);
    } else {
      parts = s.split(" ");
    }
    for (i = 0; i < parts.length; ++i) {
      if (!parts[i]) continue;
      name = strim(parts[i]);
      if (!name) continue;
      dest.push(name);
    }
    return dest;
  }
  function sfrags(s, divs) {
    var divs = ssplit(divs, "|"), dest = {};
    if (ntstype(s) || !divs.length) return dest;
    var i = 0, idx = 0, start = 0, sub = "", prev = "", find = "", quotes = false, qstate = 0;
    ForEach(divs, function(di) { dest[di] = []; }); dest[""] = [];
    for (; i < s.length; find = "", quotes = false) {
      if (qstate == 1) {
        idx = sidx(ssub(s, i), prev[1]);
        if (idx == -1) i = s.length; else i += idx;
        find = prev; quotes = true; qstate = 2;
      } else {
          var minIdx = 0x7FFFFFF;
          ForEach(dest, function(v, n) {
            if (!n) return;
            if (n == "[]" || n == "()" || n == "{}" || n == "<>") {
              idx = sidx(ssub(s, i), n[0]);
              if (idx == -1) return; else idx += i;
              if (idx < minIdx) { minIdx = idx; find = n; quotes = true; qstate = 1; return; }
            } else {
              idx = sidx(ssub(s, i), n);
              if (idx == -1) return; else idx += i;
              if (idx < minIdx) { minIdx = idx; find = n; quotes = false; qstate = 0; return; }
            }
          });
          if (!find) i = s.length;
          else i = minIdx;
      }
      if (i == s.length) break;
      sub = strim(ssub(s, start, i));
      if (sub) {
        if (prev) dest[prev].push(sub);
        else dest[""].push(sub);
      }
      if (!quotes) { i += find.length; start = i; prev = find; continue; }
      ++i; start = i; // skip [ or ( or { or <
      prev = (qstate == 2 ? "" : find);
      qstate = (qstate == 2 ? 0 : qstate);
    }
    sub = strim(ssub(s, start, i));
    if (sub) {
      if (prev) dest[prev].push(sub);
      else dest[""].push(sub);
    }
    return dest;
  }
  function sjoin(a, d) {
    if (ntalike(a)) return "";
    var d = (isstype(d) ? d : ""), s = "";
    ForEach(a, function(ai) {
      s += (d + ai);
    });
    return s;
  }
  function isnstr(s, prefix) {
    if (isatype(s)) {
      for (var i = 0; i < s.length; ++i) {
        if (!isnstr(s[i], prefix)) return 0;
      }
      return 1;
    }
    if (isntype(s)) return 1;
    if (ntstype(s) || !s) return 0;
    var i = 0, first = s[0], sign = false, num = 0, ndot = 0;
    if (first == "-" || first == "+") { i += 1; sign = true; }
    for (; i < s.length; ++i) {
      if (s[i] == ".") {
        ndot += 1; if (ndot > 1) return 0;
        if (i == 0 || (sign && i == 1)) {
          if (i + 1 == s.length) return 0;
          if (s[i+1] < '0' || s[i+1] > '9') return 0;
        }
        continue;
      }
      if (s[i] >= '0' && s[i] <= '9') { num += 1; continue; }
      if (num == 0) return 0;
      if (sstarts(prefix, "pr")) return i + 1;
      break;
    }
    return i == s.length ? i + 1 : 0;
  }
  function isnparse(s) {
    return isnstr(s, "pr");
  }
  function nsuffix(s) {
    var s = strim(s), n = isnstr(s, "pr");
    if (!n) return "";
    return strim(ssub(s, n-1));
  }
  function nparse(s, noblank) {
    if (isntype(s)) return s;
    var s = strim(s, noblank);
    if (!s) return NaN;
    return parseFloat(s);
  }
  function ReuseParentMethod(Parent, Child) {
    var F = function() {};
    F.prototype = Parent.prototype;
    Child.prototype = new F();
    Child.prototype.constructor = Child;
    Child.sproto = Parent.prototype;
  }
  var JS = function() {
     this.isReady = false;
     this.readyHandlers = [];
     this.store = {};
     this.logLevel = 4;
     this.logLimit = 300;
     this.logCount = 0;
  }
  JS.prototype.logE = logE;
  JS.prototype.logW = logW;
  JS.prototype.logF = logF;
  JS.prototype.logI = logI;
  JS.prototype.logD = logD;
  JS.prototype.logV = logV;
  JS.prototype.assert = assert;
  JS.prototype.assertEq = assertEq;
  JS.prototype.isatype = isatype;
  JS.prototype.isalike = isalike;
  JS.prototype.isstype = isstype;
  JS.prototype.isbtype = isbtype;
  JS.prototype.isntype = isntype;
  JS.prototype.isftype = isftype;
  JS.prototype.isotype = isotype;
  JS.prototype.isdefined = isdefined;
  JS.prototype.ntdefined = ntdefined;
  JS.prototype.isnull = isnull;
  JS.prototype.isnan = isnan;
  JS.prototype.isletter = isletter;
  JS.prototype.ntletter = ntletter;
  JS.prototype.isblank = isblank;
  JS.prototype.ntblank = ntblank;
  JS.prototype.ntstype = ntstype;
  JS.prototype.ntbtype = ntbtype;
  JS.prototype.ntntype = ntntype;
  JS.prototype.ntftype = ntftype;
  JS.prototype.ntotype = ntotype;
  JS.prototype.ntnull = ntnull;
  JS.prototype.ntnan = ntnan;
  JS.prototype.ntatype = ntatype;
  JS.prototype.ntalike = ntalike;
  JS.prototype.contains = contains;
  JS.prototype.slower = slower;
  JS.prototype.supper = supper;
  JS.prototype.schar = schar
  JS.prototype.sidx = sidx;
  JS.prototype.ssub = ssub;
  JS.prototype.sstarts = sstarts;
  JS.prototype.strends = strends;
  JS.prototype.strim = strim;
  JS.prototype.snoblk = snoblk;
  JS.prototype.nstrim = nstrim;
  JS.prototype.ssplit = ssplit;
  JS.prototype.sfrags = sfrags;
  JS.prototype.sjoin = sjoin;
  JS.prototype.isnstr = isnstr;
  JS.prototype.isnparse = isnparse;
  JS.prototype.nsuffix = nsuffix;
  JS.prototype.nparse = nparse;
  JS.prototype.forEach = ForEach;
  JS.prototype.forBack = ForBack;
  JS.prototype.forNext = ForNext;
  JS.prototype.ReuseParentMethod = ReuseParentMethod;
  function SetTimeout(t, f) {
    var Timeout = function(t, f) {
      this.time = (isntype(t) && isftype(f)) ? window.setTimeout(f, t) : null;
    }
    Timeout.prototype.clear = function() {
      if (this.time) window.clearTimeout(this.time);
      this.time = null;
    }
    return new Timeout(t, f);
  }
  function SetInterval(t, f) {
    var Interval = function(t, f) {
      this.time = (isntype(t) && isftype(f)) ? window.setInterval(f, t) : null;
    }
    Interval.prototype.clear = function() {
      if (this.time) window.clearInterval(this.time);
      this.time = null;
    }
    return new Interval(t, f);
  }
  function IsCompat(d) {
    return (d || document).compatMode == "CSS1Compat";
  }
  function ScrollPos(w) {
    var w = (w || window); // window.pageXOffset is an alias for the window.scrollX
    if (isntype(w.pageXOffset)) return {x: w.pageXOffset, y: w.pageYOffset};
    var doce = w.document.documentElement || w.document.body.parentNode || w.document.body, body = w.document.body;
    if (IsCompat()) return {x: doce.scrollLeft, y: doce.scrollTop};
    return {x: body.scrollLeft, y: body.scrollTop};
  }
  function ClientPos(w) {
    var d = (w || window).document, doce = d.documentElement || d.body.parentNode || d.body, doby = d.body;
    if (isntype(d.clientLeft)) return {x: d.clientLeft, y: d.clientTop};
    if (IsCompat()) return {x: doce.clientLeft, y: doce.clientTop};
    return {x: body.clientLeft, y: d.body.clientTop};
  }
  // clientWidth/Height: cssWidth/height + padding, note that the scroll bar is in the pading
  // offsetWidth/Height: cssWidth/height + padding + border
  // scrollWidth/Height: the real size of element, bounded by pading eages
  // these are read-only attributes representing the current visual layout, and all of them are integers (thus possibly subject to rounding errors)
  // http://stackoverflow.com/questions/21064101/understanding-offsetwidth-clientwidth-scrollwidth-and-height-respectively
  // https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Determining_the_dimensions_of_elements
  JS.prototype.viewport = function(w) { // https://bugzilla.mozilla.org/show_bug.cgi?id=189112#c7
    var w = w || window; // broswer window viewport including scrollbar (if rendered)
    if (isntype(w.innerWidth)) return {w: w.innerWidth, h: w.innerHeight};
    var d = w.document, doce = d.documentElement || d.body.parentNode || d.body, doby = d.body;
    if (isntype(d.clientWidth)) return {w: d.clientWidth, h: d.clientHeight};
    if (IsCompat()) return {w: doce.clientWidth, h: doce.clientHeight};
    return {w: body.clientWidth, h: body.clientHeight};
  }
  JS.prototype.disDefault = function(e) {
    if (ntotype(e)) { logE("disDefault", EARG); return false; }
    if (e.preventDefault) { e.preventDefault(); return true; }
    if (isbtype(e.returnValue)) { e.returnValue = false; return true; }
    return false;
  }
  JS.prototype.stopBubble = function(e) {
    if (ntotype(e)) { logE("stopBubble", EARG); return false; }
    if (e.stopPropagation) { e.stopPropagation(); return true; }
    if (isbtype(e.cancelBubble)) { e.cancelBubble = true; return true; }
    logE("stopPropagation/cancelBubble", EDEF);
    return false;
  }
  JS.prototype.setTimeout = SetTimeout;
  JS.prototype.setInterval = SetInterval;
  JS.prototype.compat = IsCompat;
  JS.prototype.scrollPos = ScrollPos;
  JS.prototype.clientPos = ClientPos;
  JS.prototype.mode = function(d) { return (d || document).compatMode || ""; }
  JS.prototype.doctt = function(s) { if (isstype(s)) { document.title = s; return s; } return document.title; }
  JS.prototype.cookie = function() { return document.cookie || ""; }
  JS.prototype.url = function() { return document.URL || ""; }
  JS.prototype.href = function() { return location.href || ""; };
  JS.prototype.modtime = function() { return document.lastModified || ""; }
  JS.prototype.domain = function() { return document.domain || ""; }
  JS.prototype.fromUrl = function() { return document.referrer || ""; }
  JS.prototype.fwrite = function(s) { document.write(s); }
  JS.prototype.fclose = function() { document.close(); }
  JS.prototype.screenW = function() { return screen.width; }
  JS.prototype.screenH = function() { return screen.height; }
  JS.prototype.screenAW = function() { return screen.availWidth; }
  JS.prototype.screenAH = function() { return screen.availHeight; }
  JS.prototype.colorDepth = function() { return screen.colorDepth; }
  JS.prototype.appName = function() { return navigator.appName; }
  JS.prototype.appVersion = function() { return navigator.appVersion; }
  JS.prototype.userAgent = function() { return navigator.userAgent; }
  JS.prototype.platform = function() { return navigator.platform; }
  JS.prototype.online = function() { return navigator.onLine; }
  JS.prototype.geolocation = function() { return navigator.geolocation; }
  JS.prototype.cookieEnabled = function() { return navigator.cookieEnabled; }
  JS.prototype.javaEnabled = function() { return navigator.javaEnabled(); }
  function GetEvent(e) { return e || window.event; }
  function EventName(e) { return e.type; }
  function EventTarget(e) { return e.target || e.srcElement; }
  function IsStdEom(e) { var t = EventTarget(e); return !!(t.addEventListener); }
  function IsIe8Eom(e) { var t = EventTarget(e); return !!(t.attachEvent); }
  function StdAddEvent(t, n, h) { // reject to add same handler
    if (t.addEventListener) { t.addEventListener(n, h, false); return true; }
    return false;
  }
  function Ie8AddEvent(t, n, h) { // can add same handler multi times
    if (t.attachEvent) { t.attachEvent(n, h); return true; }
    return false;
  }
  function AddEvent(t, n, h) {
    var t = RawElem(t), n = strim(n), h = isftype(h) ? h : null;
    if (!t || !n || !h) return true;
    if (StdAddEvent(t, n, h)) return true;
    if (Ie8AddEvent(t, "on"+n, h)) return true;
    logE("stdAddEvent/ie8AddEvent", EDEF);
    return false;
  }
  function StdDelEvent(t, n, h) {
    if (t.removeEventListener) { t.removeEventListener(n, h, false); return true; }
    return false;
  }
  function Ie8DelEvent(t, n, h) {
    if (t.detachEvent) { t.detachEvent(n, h); return true; }
    return false;
  }
  function DelEvent(t, n, h) {
    var t = RawElem(t), n = strim(n), h = isftype(h) ? h : null;
    if (!t || !n || !h) return true;
    if (StdDelEvent(t, n, h)) return true;
    if (Ie8DelEvent(t, "on"+n, h)) return true;
    logE("stdDelEvent/ie8DelEvent", EDEF);
    return false;
  }
  function StdAddCapture(t, n, h) {
    if (t.addEventListener) { t.addEventListener(n, h, true); return true; }
    return false;
  }
  function StdDelCapture(t, n, h) {
    if (t.removeEventListener) { t.removeEventListener(n, h, true); return true; }
    return false;
  }
  function EOMTEST(e) {
    var e = GetEvent(e), name = EventName(e), t = EventTarget(e);
    assert(isotype(e) && e !== null, "event object != null");
    if (isdefined(e.cancelable)) {
      assert(isbtype(e.cancelable), name + " cancelable is boolean");
      assert(e.preventDefault, name + " has preventDefault");
      assert(e.stopPropagation, name + " has stopPropagation");
      logI(name + ".cancelable is boolean, value " + e.cancelable);
    }
    if (isdefined(e.returnValue)) {
      assert(isbtype(e.returnValue), name + " ie.returnValue is boolean");
      assert(isbtype(e.cancelBubble), name + " ie.cancelBubble is boolean");
      logI(name + ".returnValue is boolean, value " + e.returnValue);
      logI(name + ".cancelBubble is boolean, value " + e.cancelBubble);
    }
    if (!t) { logW("event " + name, "target is null"); return e; }
    if (IsStdEom(e)) {
      assert(t.addEventListener, name + " has addEventListener");
      assert(t.removeEventListener, name + " has removeEventListener");
      assert(e.preventDefault, name + " has preventDefault");
      assert(e.stopPropagation, name + " has stopPropagation");
      assert(isbtype(e.cancelable), name + " has cancelable");
    }
    if (IsIe8Eom(e)) {
      assert(t.attachEvent, name + " has ie.attachEvent");
      assert(t.detachEvent, name + " has ie.detachEvent");
      assert(isbtype(e.returnValue), name + " has ie.returnValue");
      assert(isbtype(e.cancelBubble), name + " has ie.cancelBubble");
    }
    return e;
  }
  var Svgns = "http://www.w3.org/2000/svg";
  var Xlink = "http://www.w3.org/1999/xlink";
  function RawElem(e) {
    return isotype(e) ? (isdefined(e.$node) ? e.$node : e) : null;
  }
  function NodeType(e) {
    var e = RawElem(e);
    return (e && isntype(e.nodeType)) ? e.nodeType : 0;
  }
  function NodeName(e) {
    var e = RawElem(e);
    if (!e || ntstype(e.nodeName)) return "";
    return supper(e.nodeName);
  }
  function IsElemNode(e) {
    return NodeType(e) == 1;
  }
  function IsTextNode(e, hint) {
    var type = NodeType(e);
    if (hint == "pure") return type == 3;
    if (sstarts(hint, "cdat")) return type == 4;
    if (sstarts(hint, "comm")) return type == 8;
    return type == 3 || type == 4 || type == 8;
  }
  function GetAttr(e, a) {
    var e = RawElem(e), a = strim(a);
    if (!e || !a || !IsElemNode(e)) return "";
    if (sstarts(a, "xlink:")) {
      var name = strim(ssub(a, 6));
      if (!name) return "";
      if (e.getAttributeNS) return e.getAttributeNS(Xlink, name) || "";
    }
    if (e.getAttribute) return e.getAttribute(a) || "";
    logE("getAttribute", EDEF);
    return "";
  }
  function SetAttr(e, a, v) {
    if (ntdefined(v) && isatype(a)) {
      ForEach(a, function(ai) {
        var nv = ssplit(ai, "=");
        if (nv.length < 2) return;
        SetAttr(e, nv[0], nv[1]);
      });
      return true;
    }
    if (ntdefined(v) && isotype(a)) {
      ForEach(a, function(value, name) { SetAttr(e, name, value); });
      return true;
    }
    var e = RawElem(e), a = strim(a), v = nstrim(v);
    if (!e || !a || !v || !IsElemNode(e)) return false;
    if (sstarts(a, "xlink:")) {
      var name = strim(ssub(a, 6));
      if (!name) return false;
      if (e.setAttributeNS) { e.setAttributeNS(Xlink, name, v); return true; }
    }
    if (e.setAttribute) { e.setAttribute(a, v); return true; }
    logE("setAttribute", EDEF);
    return false;
  }
  function DelAttr(e, a) {
    var e = RawElem(e), a = strim(a);
    if (!e || !a || !IsElemNode(e)) return false;
    if (sstarts(a, "xlink:")) {
      var name = strim(ssub(a, 6));
      if (!name) return false;
      if (e.removeAttributeNS) { e.removeAttributeNS(Xlink, name); return true; }
    }
    if (e.removeAttribute) { e.removeAttribute(a); return true; }
    logE("removeAttribute", EDEF);
    return false;
  }
  function ParseAttrs(s) {
    var s = strim(s), a = [];
    if (s.length == 0) return a;
    if (s[0] != "[") s = [s];
    else s = ssplit(s, "[]");
    if (s.length == 0) return a;
    var i = 0, opidx = 0, name = "", op = "", value = "";
    for (; i < s.length; ++i) {
      opidx = sidx(s[i], "~=");
      if (opidx == -1) opidx = sidx(s[i], "^=");
      if (opidx == -1) opidx = sidx(s[i], "$=");
      if (opidx == -1) opidx = sidx(s[i], "*=");
      if (opidx == -1) opidx = sidx(s[i], "|=");
      if (opidx == -1) opidx = sidx(s[i], "=");
      if (opidx == -1) { a.push({name:s[i]}); continue; }
      name = strim(ssub(s[i], 0, opidx));
      if (!name) continue;
      op = s[i][opidx];
      op = (op == "=" ? "=" : (op + "="));
      value = strim(ssub(s[i], opidx+op.length));
      if (!value) a.push({name:name});
      else a.push({name:name, op:op, value:value});
    }
    return a;
  }
  function Has_Attr(e, a) {
    var e = RawElem(e), a = strim(a);
    if (!e || !a || !IsElemNode(e)) return false;
    if (sstarts(a, "xlink:")) {
      var name = strim(ssub(a, 6));
      if (!name) return false;
      if (e.hasAttributeNS) return e.hasAttributeNS(Xlink, name);
      if (e.getAttributeNodeNS) return !!e.getAttributeNodeNS(Xlink, name);
    }
    if (e.hasAttribute) return e.hasAttribute(a);
    if (e.getAttributeNode) return !!e.getAttributeNode(a);
    logE("hasAttribute/getAttributeNode", EDEF);
    return false;
  }
  function HasAttr(e, attrs) {
    var e = RawElem(e), attrs = (isatype(attrs) ? attrs : ParseAttrs(attrs)), j = 0;
    if (!e || !IsElemNode(e) || !attrs.length) return false;
    for (; j < attrs.length; ++j) {
      if (!attrs[j].op) { if (Has_Attr(e, attrs[j].name)) continue; else break; }
      var ea = GetAttr(e, attrs[j].name), op = attrs[j].op, value = attrs[j].value, has = false;
      if (op == "^=") { if (!sstarts(ea, value)) break; }
      else if (op == "$=") { if (!strends(ea, value)) break; }
      else if (op == "*=") { if (!contains(ea, value)) break; }
      else if (op == "~=") {
        ForEach(ssplit(ea, " "), function(si) { if (si == value) { has = true; return "break"; } });
        if (!has) break;
      }
      else if (op == "|=") {
        if (!sstarts(ea, value)) break;
        if (ea.length > value.length && isletter(ea[value.length])) break;
      }
      else { if (ea != value) break; }
    }
    return (j == attrs.length);
  }
  function GetClass(e) {
    var e = RawElem(e);
    if (e && e.className) return e.className;
    return "";
  }
  function HasClass(e, name) {
    var e = RawElem(e), name = strim(name), cls = GetClass(e);
    if (!e || !name || !cls) return false;
    var a = ssplit(name, "."), i = 0;
    for (; i < a.length; ++i) {
      if (!contains(cls, a[i])) return false;
    }
    return true;
  }
  function NextElems(e, hint) {
    var e = RawElem(e), dest = [];
    ForNext(e, hint == "back" ? "previousSibling" : "nextSibling", function(ei) {
      if (IsElemNode(ei)) dest.push(ei);
    });
    return dest;
  }
  function ChildNodes(pa) {
    var pa = RawElem(pa);
    if (!pa || ntotype(pa.childNodes) || !pa.childNodes.length) return [];
    return pa.childNodes;
  }
  function ElemNodes(pa) {
    var pa = RawElem(pa), dest = [];
    if (!pa || ntotype(pa.childNodes)) return dest;
    ForEach(pa.childNodes, function(ei) {
      if (IsElemNode(ei)) dest.push(ei);
    });
    return dest;
  }
  function NodeHtml(e, s) {
    var e = RawElem(e);
    if (!e || !isstype(e.innerHTML)) return "";
    if (s === null) { e.innerHTML = ""; return ""; }
    var s = isntype(s) ? "" + s : s;
    if (isstype(s)) { e.innerHTML = s; return s; }
    return e.innerHTML;
  }
  function NewNode(name, content) {
    var tn = ssplit(":" + name);
    if (tn.length == 0) return null;
    var type = slower(tn[0]), e = null;
    if (tn.length == 1) {
      var attr = [];
      if (type[0] == "@") {
        if (type == "@tree") return document.createDocumentFragment();
        if (type == "@text") return document.createTextNode(isstype(content) ? content : "");
        if (type == "@comm") return document.createComment(isstype(content) ? content : "");
        if (type == "@input") { type = "input"; attr.push("type=text"); }
        else if (type == "@mtext") { type = "textarea"; }
        else if (type == "@passs") { type = "input"; attr.push("type=password"); }
        else if (type == "@check") { type = "input"; attr.push("type=checkbox"); }
        else if (type == "@radio") { type = "input"; attr.push("type=radio"); }
        else if (type == "@submit") { type = "input"; attr.push("type=submit"); }
        else if (type == "@reset") { type = "input"; attr.push("type=reset"); }
        else if (type == "@button") { type = "input"; attr.push("type=button"); }
      }
      e = document.createElement(type);
      if (attr.length) SetAttr(e, attr);
      if (isstype(content) && content) NodeHtml(e, content);
      return e;
    }
    if (type == "nse" && isstype(content) && content && document.createElementNS) {
      return document.createElementNS(content, tn[1]);
    }
    if (type == "svg" && document.createElementNS) {
      return document.createElementNS(Svgns, tn[1]);
    }
    return null;
  }
  // #id tag#id .class .class.class tag.class tag.class.class
  // [attr] [attr][attr] tag[attr] tag[attr][attr]
  // [attr] [attr=value] [attr~=value] [attr^=value] [attr$=value] [attr*=value] [attr|=value]
  function ElemsOfTag(name, nodes) {
    var name = strim(name), a = [], e = null;
    if (!name) return a;
    if (ntalike(nodes)) return document.getElementsByTagName(name);
    ForEach(nodes, function(ai) {
      e = RawElem(ai);
      if (NodeName(e) == supper(name)) a.push(e);
    });
    return a;
  }
  function Select(s, nodes) {
    var s = strim(s);
    if (!s) return [];
    if (s[0] == "#") {
      s = strim(ssub(s, 1));
      if (!s) return [];
      var e = document.getElementById(s);
      if (!e) return [];
      if (ntalike(nodes)) return [e];
      for (var i = 0; i < nodes.length; ++i) {
        if (IsElemNode(nodes[i]) && nodes[i] === e) return [e];
      }
      return [];
    }
    if (s[0] == ".") {
      var a = [], i = 0;
      if (s.length == 1) return a;
      if (ntalike(nodes)) nodes = document.getElementsByTagName("*");
      for (; i < nodes.length; ++i) { if (HasClass(nodes[i], s)) a.push(nodes[i]); }
      return a;
    }
    if (s[0] == "[") {
      var attrs = ParseAttrs(s), a = [], i = 0;
      if (!attrs.length) return a;
      if (ntalike(nodes)) nodes = document.getElementsByTagName("*");
      for (; i < nodes.length; ++i) { if (HasAttr(nodes[i], attrs)) a.push(nodes[i]); }
      return a;
    }
    var idx = sidx(s, "#");
    if (idx == -1) idx = sidx(s, ".");
    if (idx == -1) idx = sidx(s, "[");
    if (idx == -1) return ElemsOfTag(s, nodes);
    var tag = strim(ssub(s, 0, idx));
    if (!tag) return Select(ssub(s, idx), nodes);
    return Select(ssub(s, idx), ElemsOfTag(tag, nodes));
  }
  function RealArray(a) {
    if (ntalike(a)) return [];
    if (isatype(a)) return a;
    var dest = [];
    ForEach(a, function(ai) { dest.push(ai); });
    return dest;
  }
  function UnionArray(a, elem) {
    if (ntatype(a)) {
      if (ntnull(a)) logE("UnionArray", EARG);
      if (isalike(elem)) return RealArray(elem);
      var elem = RawElem(elem);
      return elem ? [elem] : null;
    }
    if (ntotype(elem)) return a;
    if (ntalike(elem)) {
      var elem = RawElem(elem);
      if (!elem) return a;
      if (!contiains(a, elem)) a.push(elem);
      return a;
    }
    ForEach(elem, function(e) {
      e = RawElem(e);
      if (!e || contains(a, e)) return;
      a.push(e);
    });
    return a;
  }
  function SelectAllSubElems(dest, op) {
    if (ntalike(dest)) return null;
    var a = [], ai = null;
    if (op == "+" || op == ">") {
      ForEach(dest, function(ai) {
        ai = RawElem(ai);
        a = UnionArray(a, op == "+" ? NextElems(ai) : ElemNodes(ai));
      });
      return a;
    }
    ForEach(dest, function(ai) {
      ai = RawElem(ai);
      if (isotype(ai) && ai.getElementsByTagName) {
        a = UnionArray(a, ai.getElementsByTagName("*"));
      }
    });
    return a;
  }
  function SelectPart(s, nodes) {
    var s = strim(s), i = 0, start = 0, end = 0, dest = null, op = "", subElemsSelected = false;
    if (!s) return dest;
    dest = UnionArray(dest, nodes);
    while (i < s.length) {
      if (s[i] == "*") { start = i; end = (++i); }
      else if (s[i] == "#" || s[i] == ".") {
        start = i;
        while (i < s.length && !isblank(s[i]) && s[i] != "+" && s[i] != ">") ++i;
        end = i;
      } else if (s[i] == "[") {
        start = i;
        while (s[i] == "[") {
          while (i < s.length && s[i] != "]") ++i;
          if (i == s.length) return dest; // no ] find
          ++i;
        }        // [...][...][........]
        end = i; // current pos is here ^
      } else {
        start = i;
        while (i < s.length && ntblank(s[i]) && s[i] != "+" && s[i] != ">" && s[i] != "[") ++i;
        if (i == s.length) return Select(ssub(s, start), dest);
        while (s[i] == "[") {
          while (i < s.length && s[i] != "]") ++i;
          if (i == s.length) return Select(ssub(s, start), dest);
          ++i;
        }
        end = i;
      }
      if (s[start] == "*") {
        if (!dest && !subElemsSelected) dest = document.getElementsByTagName("*");
      } else {
        dest = Select(ssub(s, start, end), dest);
      }
      while (i < s.length && ntblank(s[i]) && s[i] != "+" && s[i] != ">") ++i; // find blank/+/>
      if (i == s.length) return dest;
      while (isblank(s[i]) && i < s.length) ++i; // skip blanks
      if (i == s.length) return dest;
      op = " "; if (s[i] == "+" || s[i] == ">") op = s[i];
      while (i < s.length && (isblank(s[i]) || s[i] == "+" || s[i] == ">")) ++i; // skip extra blank/+/>
      if (i == s.length) return dest;
      dest = SelectAllSubElems(dest, op);
      subElemsSelected = true;
    }
    return dest;
  }
  function SelectElems(s, pa) {
    var s = ssplit(s, ","), nodes = null;
    if (!s.length) return [];
    nodes = isalike(pa) ? pa : (((pa = RawElem(pa)) && pa.getElementsByTagName) ? pa.getElementsByTagName("*") : null);
    var a = null, part = null, i = 0;
    a = RealArray(SelectPart(s[0], nodes));
    for (i = 1; i < s.length; ++i) {
      part = SelectPart(s[i], nodes);
      if (ntalike(part)) continue;
      ForEach(part, function(ai) {
        if (!contains(a, ai)) a.push(ai);
      });
    }
    return a;
  }
  function SelectElem(s, pa) {
    var nodes = SelectElems(s, pa);
    if (nodes.length > 0) return nodes[0];
    return null;
  }
  function ForChild(pa, f) {
    var pa = RawElem(pa);
    if (!pa || ntotype(pa.childNodes)) return;
    ForEach(pa.childNodes, function(ei, i) { return f(ei, i); });
  }
  function AddChild(pa, c, i) {
    if (isatype(c)) {
      if (isntype(i)) ForEach(c, function(ci, idx) { AddChild(pa, ci, i+idx); });
      else ForEach(c, function(ci) { AddChild(pa, ci); });
      return;
    }
    var pa = RawElem(pa), c = RawElem(c);
    if (!pa || ntotype(pa.childNodes) || !c) return;
    var nodes = pa.childNodes, i = nparse(i);
    if (isnan(i) || i < 0 || i >= nodes.length) { if (pa.appendChild) pa.appendChild(c); return; }
    if (pa.insertBefore) pa.insertBefore(c, nodes[i]);
  }
  function SelectionText(w) {
    var w = w || window;
    if (w.getSelection) { // H5 standard
      var s = w.getSelection().toString();
      if (s.length > 0) return s;
      ForEach(SelectElems("textarea,input[type=text]"), function(ei) {
        if (ntntype(ei.selectionStart) || !ei.value) return;
        if (ei.selectionStart >= ei.selectionEnd) return;
        s = ei.value.substring(ei.selectionStart, ei.selectionEnd);
        return "break";
      });
      return s;
    }
    if (document.selection) return document.selection.createRange().text || ""; // for IE
    return "";　
  }
  function Html() { return document.documentElement || SelectElem("html"); }
  function Body() { return document.body || SelectElem("body"); }
  function Head() { return document.head || SelectElem("head"); }
  var Elem = function(e) {
    this.$node = RawElem(e);
  }
  Elem.Empty = new Elem();
  Elem.prototype.reset = function(e) { this.$node = RawElem(e); return this; }
  Elem.prototype.addEvent = function(n, h) {
    if (!n || ntstype(n) || ntftype(h)) { logE("addEvent", EARG); return this; }
    if (ntdefined(this.ehmap)) this.ehmap = {}; // such as click:[main,h1,h2,...]
    var that = this;
    function EventHandler(e) {
      var e = GetEvent(e), en = EventName(e);
      if (ntdefined(that.ehmap) || ntatype(that.ehmap[en])) return;
      ForEach(that.ehmap[en], function(hi, i) {
        if (i == 0) return;
        try { hi(e, that); } catch (e) { logE(en + " handler " + i + " exception", e); throw e; }
      });
    }
    if (ntatype(this.ehmap[n])) this.ehmap[n] = [EventHandler];
    this.ehmap[n].push(h);
    AddEvent(this.$node, n, EventHandler);
    return this;
  }
  Elem.prototype.delEvent = function(n, h) {
    if (!n || ntstype(n) || ntftype(h)) { logE("delEvent", EARG); return this; }
    if (ntdefined(this.ehmap) || ntatype(this.ehmap[n])) return this;
    var a = this.ehmap[n];
    if (h != a[0]) a.pop(h);
    if (a.length == 1) { DelEvent(this.$node, n, a[0]); this.ehmap[n] = undefined; }
    return this;
  }
  Elem.prototype.clrEvent = function(n) {
    if (!n || ntstype(n)) { logE("clrEvent", EARG); return this; }
    if (ntdefined(this.ehmap) || ntatype(this.ehmap[n])) return this;
    DelEvent(this.$node, n, this.ehmap[n][0]);
    this.ehmap[n] = undefined;
    return this;
  }
  Elem.prototype.getBound = function() {
    var rect = {x:0, y:0, w:0, h:0, vx:0, vy:0}, e = this.$node;
    if (e.getClientRects && !e.getClientRects().length) return rect;
    if (!e.getBoundingClientRect) { logE("getBoundingClientRect", EDEF); return rect; }
    var r = e.getBoundingClientRect(); // returns an element's size and position relative to the viewport
    var width = r.right - r.left, height = r.bottom - r.top;
    if (!width && !height) return rect;
    var scroll = ScrollPos();
    var client = ClientPos();
    return {vx:r.left, vy:r.top, w:width, h:height, x:r.left+scroll.x-client.x, y:r.top+scroll.y-client.y};
  }
  Elem.prototype.isDocFrag = function() {
    return this.nodeType() == 11;
  }
  Elem.prototype.isDocNode = function() {
    return this.nodeType() == 9;
  }
  Elem.prototype.isDOCTYPE = function() {
    return this.nodeType() == 10;
  }
  Elem.prototype.ownerdoc = function() {
    var e = this.$node;
    if (!e) return null;
    if (this.isDocNode()) return e;
    return e.ownerDocument || null;
  }
  Elem.prototype.isElemNode = function() { return IsElemNode(this.$node); }
  Elem.prototype.isTextNode = function(hint) { return IsTextNode(this.$node, hint); }
  Elem.prototype.isPureText = function() { return this.isTextNode("pure"); }
  Elem.prototype.isCommNode = function() { return this.isTextNode("comm"); }
  Elem.prototype.isCdatNode = function() { return this.isTextNode("cdat"); }
  Elem.prototype.getAttr = function(a) { return GetAttr(this.$node, a); }
  Elem.prototype.setAttr = function(a, v) { SetAttr(this.$node, a, v); return this; }
  Elem.prototype.delAttr = function(a) { DelAttr(this.$node, a); return this; }
  Elem.prototype.hasAttr = function(a) { return HasAttr(this.$node, a); }
  Elem.prototype.attr = function(a, v) { if (v) return this.setAttr(a, v); return this.getAttr(a); }
  Elem.prototype.id = function(v) { return this.attr("id", v); }
  Elem.prototype.clrClass = function() { this.delAttr("class"); return this; }
  Elem.prototype.hasClass = function(c) { return HasClass(this.$node, c); }
  Elem.prototype.getClass = function() { return GetClass(this.$node); }
  Elem.prototype.value = function(v) {
    var e = this.$node;
    if (!e || ntdefined(e.value)) return ntdefined(v) ? this : "";
    if (ntdefined(v)) return strim(e.value);
    if (isstype(v)) e.value = strim(v);
    return this;
  }
  Elem.prototype.setClass = function(classes) {
    if (classes === "" || classes === null) return this.clrClass();
    var classes = isatype(classes) ? sjoin(classes, " ") : classes;
    return this.setAttr("class", classes);
  }
  Elem.prototype.addClass = function(name) {
    var name = strim(name);
    if (!name) return this;
    if (this.hasClass(name)) return this;
    return this.setClass(this.getClass() + " " + name);
  }
  Elem.prototype.delClass = function(name) {
    var name = strim(name), classes = this.getClass();
    if (!name || !classes) return this;
    var a = ssplit(classes, " "), s = "", i = 0;
    for (; i < a.length; ++i) { if (a[i] !== name) s += a[i] + " "; }
    if (!s.length) this.clrClass();
    else this.setClass(s);
    return this;
  }
  Elem.prototype.clrStyle = function() { this.delAttr("style"); return this; }
  Elem.prototype.setStyle = function(styles) {
    if (styles === "" || styles === null) return this.clrStyle();
    return this.setAttr("style", styles);
  }
  function ElemStyle(e, hint) {
    if (hint != "computed") return slower(e.style.cssText);
    if (!window.getComputedStyle) { logE("getComputedStyle", EDEF); return ""; }
    var s = window.getComputedStyle(e, null), css = "";
    if (!s) return "";
    css = strim(s.cssText);
    if (css) return slower(css);
    if (!s.getPropertyValue) return "";
    ForEach(s, function(si) { css += si + ":" + s.getPropertyValue(si) + ";" });
    return slower(css);
  }
  Elem.prototype.getStyle = function(name, hint) {
    var e = this.$node, styles = "", parts = null, css;
    if (!e || !e.style) return "";
    styles = strim(ElemStyle(e, hint));
    if (ntdefined(name)) return styles;
    name = strim(name);
    var sa = ssplit(styles, ";"), parts = null, i = 0;
    for (; i < sa.length; ++i) {
      parts = ssplit(sa[i], ":");
      if (parts.length >= 2 && parts[0] == name) return parts[1];
    }
    return "";
  }
  Elem.prototype.hasStyle = function(name, hint) {
    var name = strim(name), a = ssplit(this.getStyle(undefined, hint), ";"), has = true;
    if (!name || !a.length) return false;
    ForEach(ssplit(name, ";"), function(ni) {
      var ni = ssplit(ni, ":"), i = 0;
      for (; i < a.length; ++i) {
        if (ntatype(a[i])) a[i] = ssplit(a[i], ":");
        if (ni.length >= 2) {
          if (a[i].length >= 2 && a[i][0] == ni[0] && a[i][1] == ni[1]) break;
        } else {
          if (a[i].length >= 1 && a[i][0] == ni[0]) break;
        }
      }
      if (i == a.length) { has = false; return "break"; }
    });
    return has;
  }
  Elem.prototype.getCStyle = function(name) { return this.getStyle(name, "computed"); }
  Elem.prototype.hasCStyle = function(name) { return this.hasStyle(name, "computed"); }
  Elem.prototype.delStyle = function(name) {
    var name = strim(name), a = ssplit(this.getStyle(), ";");
    if (!name || !a.length) return this;
    var s = "", parts = null;
    ForEach(a, function(ai) {
      parts = ssplit(ai, ":");
      if (parts.length >= 1 && parts[0] == name) return;
      s += ai + ";";
    });
    return this.setStyle(s);
  }
  Elem.prototype.addStyle = function(style, v) {
    var style = strim(style);
    if (ntdefined(v)) { if (style) this.setStyle(this.getStyle() + ";" + style); return this; }
    var s = style + ";", v = strim(v), a = ssplit(this.getStyle(), ";"), parts = null;
    if (!s || !v) return this;
    ForEach(a, function(ai) {
      parts = ssplit(ai, ":");
      if (parts.length >= 1 && parts[0] == style) return;
      s += ai + ";";
    });
    return this.setStyle(s);
  }
  Elem.prototype.parent = function() {
    var e = this.$node;
    if (e && e.parentNode) return new Elem(e.parentNode);
    return null;
  }
  Elem.prototype.hasChild = function() {
    var e = this.$node;
    if (e && e.hasChildNodes) return e.hasChildNodes();
    return false;
  }
  Elem.prototype.firstChild = function() {
    var e = this.$node;
    if (e && e.firstChild) return new Elem(e.firstChild);
    return null;
  }
  Elem.prototype.lastChild = function() {
    var e = this.$node;
    if (e && e.lastChild) return new Elem(e.lastChild);
    return null;
  }
  Elem.prototype.nextSibling = function() {
    var e = this.$node;
    if (e && e.nextSibling) return new Elem(e.nextSibling);
    return null;
  }
  Elem.prototype.prevSibling = function() {
    var e = this.$node;
    if (e && e.previousSibling) return new Elem(e.previousSibling);
    return null;
  }
  Elem.prototype.frontChild = function(idx) {
    var e = this.firstChild();
    if (ntdefined(idx)) return e;
    var idx = nparse(idx);
    if (isnan(idx) || idx < 0) return null;
    for (var i = 0; i < idx; ++i) {
      var nextChild = (e ? e.nextSibling() : null);
      if (!nextChild) return null;
      e = nextChild;
    }
    return e;
  }
  Elem.prototype.forChild = function(f) {
    var pa = this.$node, nodes = null;
    if (!pa || !(nodes = pa.childNodes) || !nodes.length) return this;
    var e = new Elem();
    ForEach(nodes, function(ei, i) { e.reset(ei); return f(e, i); });
    return this;
  }
  Elem.prototype.childBack = function(f) {
    var pa = this.$node, nodes = null;
    if (!pa || !(nodes = pa.childNodes) || !nodes.length) return this;
    var e = new Elem();
    ForBack(nodes, function(ei, c) { e.reset(ei); return f(e, c); });
    return this;
  }
  Elem.prototype.forElems = function(f) {
    var pa = this.$node, nodes = null;
    if (!pa || !(nodes = pa.childNodes) || !nodes.length) return this;
    var e = new Elem(), ec = 0;
    ForEach(nodes, function(ei) {
      if (IsElemNode(ei)) { e.reset(ei); return f(e, ec++); }
    });
    return this;
  }
  Elem.prototype.elemsBack = function(f) {
    var pa = this.$node, nodes = null;
    if (!pa || !(nodes = pa.childNodes) || !nodes.length) return this;
    var e = new Elem(), ec = 0;
    ForBack(nodes, function(ei) {
      if (IsElemNode(ei)) { e.reset(ei); return f(e, ++ec); }
    });
    return this;
  }
  function ParseHint(pa, hint) {
    var pa = RawElem(pa), nodes = null;
    if (hint == ">") nodes = ChildNodes(pa);
    else if (hint == "+") nodes = NextElems(pa);
    else if (hint == "-") nodes = NextElems(pa, "back");
    else nodes = pa;
    return nodes;
  }
  Elem.prototype.elem = function(s) {
    var s = strim(s), hint = "";
    if (!s) return null;
    if (contains(">+-", s[0])) { hint = s[0]; s = strim(ssub(s, 1)); }
    var e = SelectElem(s, ParseHint(this.$node, hint));
    if (e) return new Elem(e);
    return null;
  }
  Elem.prototype.elems = function(s) {
    var s = strim(s), hint = "";
    if (!s) return [];
    if (contains(">+-", s[0])) { hint = s[0]; s = strim(ssub(s, 1)); }
    var i = 0, a = SelectElems(s, ParseHint(this.$node, hint));
    for (; i < a.length; ++i) {
      a[i] = new Elem(a[i]);
    }
    return a;
  }
  Elem.prototype.childNodes = function() {
    var e = this.$node, dest = [];
    if (!e || ntotype(e.childNodes)) return dest;
    ForEach(e.childNodes, function(ai) { dest.push(new Elem(ai)); });
    return dest;
  }
  Elem.prototype.childCount = function() {
    var e = this.$node;
    if (!e || ntotype(e.childNodes)) return 0;
    return e.childNodes.length || 0;
  }
  Elem.prototype.childIndex = function(c) {
    var pa = this.$node, c = isntype(c) ? c : RawElem(c), index = -1;
    if (!pa || ntotype(pa.childNodes)) return index;
    if (isntype(c)) { if (c >= 0 && c < this.childCount()) return c; else return index; }
    ForEach(pa.childNodes, function(ei, i) {
      if (ei === c) { index = i; return "break"; }
    });
    return index;
  }
  Elem.prototype.elemCount = function() {
    var e = this.$node, c = 0;
    if (!e || ntotype(e.childNodes)) return c;
    ForEach(e.childNodes, function(ei) { if (IsElemNode(ei)) ++c; });
    return c;
  }
  Elem.prototype.elemNodes = function() {
    var e = this.$node;
    if (!e || ntotype(e.childNodes)) return []
    var a = e.childNodes, dest = [];
    ForEach(a, function(ai) { if (IsElemNode(ai)) dest.push(new Elem(ai)); });
    return dest;
  }
  Elem.prototype.textNodes = function(hint) {
    var e = this.$node;
    if (!e || ntotype(e.childNodes)) return []
    var a = e.childNodes, dest = [], type = 0;
    ForEach(a, function(ai) {
      type = NodeType(ai);
      if (hint == "pure") { if (type == 3) dest.push(new Elem(ai)); }
      else if (sstarts(hint, "cdat")) { if (type == 4) dest.push(new Elem(ai)); }
      else if (sstarts(hint, "comm")) { if (type == 8) dest.push(new Elem(ai)); }
      else { if (type == 3 || type == 4 || type == 8) dest.push(new Elem(ai)); }
    });
    return dest;
  }
  Elem.prototype.nextElems = function() {
    var dest = [];
    ForNext(this.$node, "nextSibling", function(ei) {
      if (IsElemNode(ei)) dest.push(new Elem(ei));
    });
    return dest;
  }
  Elem.prototype.prevElems = function() {
    var dest = [];
    ForNext(this.$node, "previousSibling", function(ei) {
      if (IsElemNode(ei)) dest.push(new Elem(ei));
    });
    return dest;
  }
  Elem.prototype.forNext = function(f) {
    var ec = 0, e = new Elem();
    ForNext(this.$node, "nextSibling", function(ei) {
      if (IsElemNode(ei)) { e.reset(ei); return f(e, ec++); }
    });
    return this;
  }
  Elem.prototype.forPrev = function(f) {
    var ec = 0, e = new Elem();
    ForNext(this.$node, "previousSibling", function(ei) {
      if (IsElemNode(ei)) { e.reset(ei); return f(e, ec++); }
    });
    return this;
  }
  Elem.prototype.addChild = function(c, i) { AddChild(this.$node, c, i); return this; }
  Elem.prototype.addBefore = function(c, old) {
    var c = RawElem(c), old = RawElem(old), pa = this.$node;
    if (!c || !old || !pa) { logW("addBefore", EARG); return this; }
    if (pa.insertBefore) pa.insertBefore(c, old);
    return this;
  }
  Elem.prototype.addAfter = function(c, old) {
    var c = RawElem(c), old = RawElem(old), pa = this.$node;
    if (!c || !old || !pa) { logW("addAfter", EARG); return this; }
    var i = this.childIndex(old);
    if (i == -1) { logE("addAfter child unfound"); return this; }
    return this.addChild(c, i+1);
  }
  Elem.prototype.delChild = function(c) {
    var i = nparse(c), pa = this.$node;
    if (!isnan(i)) {
      var nodes = isotype(pa.childNodes) ? pa.childNodes : [];
      if (i < 0 || i >= nodes.length) return this;
      if (pa.removeChild) pa.removeChild(nodes[i]);
    } else {
      var c = RawElem(c);
      if (!c || !pa) { logW("delChild", EARG); return this; }
      if (pa.removeChild) pa.removeChild(c);
    }
    return this;
  }
  Elem.prototype.repChild = function(c, i) {
    var pa = this.$node, c = RawElem(c), i = this.childIndex(i);
    if (!pa || !c || i == -1) return this;
    this.delChild(i);
    this.addChild(c, i);
    return this;
  }
  Elem.prototype.clrNodes = function() {
    while (this.hasChild()) this.delChild(this.firstChild());
    return this;
  }
  Elem.prototype.clone = function() {
    // If the original node has an ID and the clone is to be placed in the same document,
    // the ID of the clone should be modified to be unique. Name attributes may need to be modified also,
    // depending on whether duplicate names are expected.
    var e = this.$node;
    if (!e || !e.cloneNode) return null;
    var node = e.cloneNode(true);
    if (!node) return null;
    return new Elem(node);
  }
  Elem.prototype.normalize = function() {
    var e = this.$node;
    if (!e) return this;
    if (e.normalize) e.normalize();
    return this;
  }
  Elem.prototype.nodeName = function() { return NodeName(this.$node); }
  Elem.prototype.nodeType = function() { return NodeType(this.$node); }
  Elem.prototype.nodeHtml = function(s) { return NodeHtml(this.$node, s); }
  Elem.prototype.nodeText = function(s) {
    var e = this.$node;
    if (!e) return "";
    if (isstype(s) || isntype(s) || s === null) {
      this.clrNodes();
      if (s === null) return "";
      s = isntype(s) ? "" + s : s;
      this.addChild(NewNode("@text", s));
      return s;
    }
    if (this.isTextNode()) return e.nodeValue || "";
    function getElemText(e) {
      var text = "";
      ForChild(e, function(c) {
        if (IsTextNode(c)) { text += (c.nodeValue || ""); return; }
        if (IsElemNode(c)) {
          // if (NodeName(c) == "SCRIPT") { text += (c.text || ""); return "continue"; }
          text += getElemText(c);
        }
      });
      return text;
    }
    if (this.isDocNode()) e = e.body;
    if (IsElemNode(e)) return getElemText(e);
    return "";
  }
  JS.prototype.xlink = Xlink;
  JS.prototype.svgns = Svgns;
  JS.prototype.seleText = SelectionText;
  JS.prototype.html = function() { return new Elem(Html()); }
  JS.prototype.body = function() { return new Elem(Body()); }
  JS.prototype.head = function() { return new Elem(Head()); }
  JS.prototype.newElem = function(name, text) {
    if (isotype(name)) return isdefined(name.$node) ? name : new Elem(name);
    var name = sfrags(name, "#|.|[]"), node = null;
    if (ntatype(name[""]) || !(name[""].length)) return null;
    node = NewNode(name[""][0], text);
    if (!node) return null;
    node = new Elem(node);
    if (!node.isElemNode()) return node;
    if (name["#"].length) node.id(name["#"][0]);
    if (name["."].length) node.setClass(name["."]);
    if (name["[]"].length) node.setAttr(name["[]"]);
    return node;
  }
  JS.prototype.forElems = function(pa, f) {
    if (isotype(pa)) {
      var pa = (pa && isdefined(pa.$node)) ? pa : (RawElem(pa) ? new Elem(pa) : null);
      if (!pa) return;
      pa.forElems(f);
    } else if (pa && isstype(pa)) {
      var e = new Elem();
      ForEach(SelectElems(pa), function(ei) { e.reset(ei); return f(e, i); });
    }
  }
  JS.prototype.elem = function(s) {
    var e = SelectElem(s);
    if (!e) return null;
    return new Elem(e);
  }
  JS.prototype.elems = function(s) {
    var dest = SelectElems(s), i = 0;
    for (; i < dest.length; ++i) { dest[i] = new Elem(dest[i]); }
    return dest;
  }
  JS.prototype.newSvg = function(w, h, box, keepRatio) {
    var that = this;
    function SetSvg(svg, w, h, box, keepRatio) {
      svg.$node = NewNode("svg:svg");
      if (!svg.$node) return; // svg unsupported
      svg.setSize(w, h);
      svg.setView(box, keepRatio);
    }
    var Svg = function() {
      this.$node = null;
      this.svgtr = that.newElem("@tree");
      this.svgpa = this.svgtr;
    }
    ReuseParentMethod(Elem, Svg);
    Svg.prototype.setSize = function(w, h, box, keepRatio) {
      if (isnparse(w) && isnparse(h)) {
        this.setAttr({width:nstrim(w), height:nstrim(h)});
      }
      this.setView(box, keepRatio);
      return this;
    }
    function ParseViewAttrs(box, keepRatio) {
      var attrs = {}, parts = ssplit(box), keepRatio = strim(keepRatio);
      if (parts.length == 0) return attrs;
      if (parts.length == 4 && isnparse(parts)) {
        attrs.viewBox = box;
        if (!keepRatio || ntstype(keepRatio)) return attrs;
        if (keepRatio == "smallMin") attrs.preserveAspectRatio = "xMinYMin meet";
        else if (keepRatio == "smallMid") attrs.preserveAspectRatio = "xMidYMid meet";
        else if (keepRatio == "smallMax") attrs.preserveAspectRatio = "xMaxYMax meet";
        else if (keepRatio == "largeMin") attrs.preserveAspectRatio = "xMinYMin slice";
        else if (keepRatio == "largeMid") attrs.preserveAspectRatio = "xMidYMid slice";
        else if (keepRatio == "largeMax") attrs.preserveAspectRatio = "xMaxYMax slice";
        else attrs.preserveAspectRatio = keepRatio;
        return attrs;
      }
      logW("setView", EARG + box);
      return attrs;
    }
    Svg.prototype.setView = function(box, keepRatio) {
      var attrs = ParseViewAttrs(box, keepRatio);
      if (attrs.viewBox) this.setAttr(attrs);
      return this;
    }
    Svg.prototype.title = function(s) {
      if (this.nodeName() != "SVG") return Elem.Empty;
      if (ntstype(s) || !s) return Elem.Empty;
      var t = this.svgpa.elem(">title");
      if (t) { t.nodeText(s); return t; }
      t = that.newElem("svg:title");
      if (!t) return Elem.Empty;
      t.nodeText(s);
      this.svgpa.addChild(t, 0);
      var child = "";
      ForEach(ChildNodes(this.svgpa), function(ni) { child += ni.nodeName + " "; });
      return t;
    }
    Svg.prototype.desc = function(s) {
      if (this.nodeName() != "SVG") return Elem.Empty;
      if (ntstype(s) || !s) return Elem.Empty;
      var d = this.svgpa.elem(">desc");
      if (d) { d.nodeText(s); return d; }
      d = that.newElem("svg:desc");
      if (!d) return Elem.Empty;
      d.nodeText(s);
      var t = this.svgpa.elem(">title");
      if (t) this.svgpa.addAfter(d, t); else this.svgpa.addChild(d, 0);
      return d;
    }
    var Shape = function() {}
    ReuseParentMethod(Elem, Shape);
    Shape.prototype.stroke = function(s) {
      // stroke stroke-width stroke-opacity stroke-dasharray stroke-dashoffset
      // stroke-linecap stroke-linejoin stroke-miterlimit
    }
    Shape.prototype.fill = function(s) {
      // fill fill-opacity fill-rule
    }
    var Line = function() {
      this.$node = NewNode("svg:line");
    }
    ReuseParentMethod(Shape, Line);
    Line.prototype.set = function(x1, y1, x2, y2, cls) {
      return this.setAttr({x1:x1, y1:y1, x2:x2, y2:y2, "class":cls||"frame2d"});
    } // w h bound len len2 angle
    Line.prototype.x1 = function(v) { return this.attr("x1", v); }
    Line.prototype.x2 = function(v) { return this.attr("x1", v); }
    Line.prototype.y1 = function(v) { return this.attr("y1", v); }
    Line.prototype.y2 = function(v) { return this.attr("y2", v); }
    Line.prototype.from = function(x, y) { return this.setAttr({x1:x,y1:y}); }
    Line.prototype.to = function(x, y) { return this.setAttr({x2:x,y2:y}); }
    Svg.prototype.line = function(x1, y1, x2, y2, cls) {
      var l = new Line();
      l.set(x1, y1, x2, y2, cls);
      this.svgpa.addChild(l);
      return l;
    }
    Svg.prototype.hline = function(x1, x2, y, cls) { return this.line(x1, y, x2, y, cls); }
    Svg.prototype.vline = function(x, y1, y2, cls) { return this.line(x, y1, x, y2, cls); }
    var Polyline = function() {
      this.$node = NewNode("svg:polyline");
    }
    ReuseParentMethod(Shape, Polyline);
    Polyline.prototype.set = function(points, cls) {
      return this.setAttr({points:points, "class":cls||"frame2d"});
    }
    Svg.prototype.polyline = function(points, cls) {
      var pl = new Polyline();
      pl.set(points, cls);
      this.svgpa.addChild(pl);
      return pl;
    }
    var Rect = function() {
      this.$node = NewNode("svg:rect");
    }
    ReuseParentMethod(Shape, Rect);
    Rect.prototype.set = function(minX, minY, w, h, cls) {
      return this.setAttr({x:minX, y:minY, width:w, height:h, "class":cls||"solid2d"});
    } // bound len len2
    Rect.prototype.x = function(v) { return this.attr("x", v); }
    Rect.prototype.y = function(v) { return this.attr("y", v); }
    Rect.prototype.w = function(v) { return this.attr("w", v); }
    Rect.prototype.h = function(v) { return this.attr("h", v); }
    Rect.prototype.rx = function(v) { return this.attr("rx", v); }
    Rect.prototype.ry = function(v) { return this.attr("ry", v); }
    Rect.prototype.setRound = function(rx, ry) {
      if (!isstype(rx) && !isntype(rx)) return this;
      var ry = (ry && (isstype(ry) || isntype(ry))) ? ry : rx;
      return this.setAttr({rx:rx, ry:rx});
    }
    Svg.prototype.rect = function(minX, minY, w, h, cls) {
      var r = new Rect();
      r.set(minX, minY, w, h, cls);
      this.svgpa.addChild(r);
      return r;
    }
    var Circle = function() {
      this.$node = NewNode("svg:circle");
    }
    ReuseParentMethod(Shape, Circle);
    Circle.prototype.set = function(cx, cy, r, cls) {
      return this.setAttr({cx:cx, cy:cy, r:r, "class":cls||"solid2d"});
    } // bound area
    Circle.prototype.r = function(v) { return this.attr("r", v); }
    Circle.prototype.cx = function(v) { return this.attr("cx", v); }
    Circle.prototype.cy = function(v) { return this.attr("cy", v); }
    Circle.prototype.co = function(x, y) { return this.setAttr({cx:x,cy:y}); }
    Svg.prototype.circle = function(cx, cy, r, cls) {
      var c = new Circle();
      c.set(cx, cy, r, cls);
      this.svgpa.addChild(c);
      return c;
    }
    var Ellipse = function() {
      this.$node = NewNode("svg:ellipse");
    }
    ReuseParentMethod(Shape, Ellipse);
    Ellipse.prototype.set = function(cx, cy, rx, ry, cls) {
      return this.setAttr({cx:cx, cy:cy, rx:rx, ry:ry, "class":cls||"solid2d"});
    }
    Ellipse.prototype.cx = function(v) { return this.attr("cx", v); }
    Ellipse.prototype.cy = function(v) { return this.attr("cy", v); }
    Ellipse.prototype.rx = function(v) { return this.attr("rx", v); }
    Ellipse.prototype.ry = function(v) { return this.attr("ry", v); }
    Ellipse.prototype.co = function(x, y) { return this.setAttr({cx:x,cy:y}); }
    Svg.prototype.ellipse = function(cx, cy, rx, ry, cls) {
      var e = new Ellipse();
      e.set(cx, cy, rx, ry, cls);
      this.svgpa.addChild(e);
      return e;
    }
    var Polygon = function() {
      this.$node = NewNode("svg:polygon");
    }
    ReuseParentMethod(Shape, Polygon);
    Polygon.prototype.set = function(points, cls) {
      return this.setAttr({points:points, "class":cls||"solid2d"});
    }
    Svg.prototype.polygon = function(points, cls) {
      var p = new Polygon();
      p.set(points, cls);
      this.svgpa.addChild(p);
      return p;
    }
    var Path = function() {
      this.$node = NewNode("svg:path");
    }
    ReuseParentMethod(Shape, Path);
    Path.prototype.set = function(d, cls) {
      return this.setAttr({d:d, "class":cls||(contains(slower(d),"z")?"solid2d":"frame2d")});
    }
    Svg.prototype.path = function(d, cls) {
      var p = new Path();
      p.set(d, cls);
      this.svgpa.addChild(p);
      return p;
    }
    Svg.prototype.rectFr = function(minX, minY, w, h) { return this.rect(minX, minY, w, h, "frame2d"); }
    Svg.prototype.circleFr = function(cx, cy, r) { return this.circle(cx, cy, r, "frame2d"); }
    Svg.prototype.ellipseFr = function(cx, cy, rx, ry) { return this.ellipse(cx, cy, rx, ry, "frame2d"); }
    Svg.prototype.polygonFr = function(points) { return this.polygon(points, "frame2d"); }
    Svg.prototype.pathFr = function(d) { return this.path(d, "frame2d"); }
    var Defs = function() {
      this.$node = NewNode("svg:defs");
    }
    ReuseParentMethod(Elem, Defs);
    Svg.prototype.defsBegin = function() {
      var d = new Defs();
      this.svgpa.addChild(d);
      this.svgpa = d;
      return d;
    }
    var G = function() {
      this.$node = NewNode("svg:g");
    }
    ReuseParentMethod(Elem, G);
    Svg.prototype.gBegin = function() {
      var g = new G();
      this.svgpa.addChild(g);
      this.svgpa = g;
      return g;
    }
    Svg.prototype.svgBegin = function(w, h, box, keepRatio) {
      var s = new Svg();
      SetSvg(s, w, h, box, keepRatio);
      s.setPos = function(x, y) {
        if (isnparse(x) && isnparse(y)) {
          s.setAttr({x:nstrim(x), y:nstrim(y)});
        }
      }
      this.svgpa.addChild(s);
      this.svgpa = s;
      return s;
    }
    Svg.prototype.setEnd = function() {
      if (this.svgpa == this.svgtr) return this;
      this.svgpa = this.svgpa.parent() || this.svgtr;
      return this;
    }
    Svg.prototype.defsEnd = function() { return this.setEnd(); }
    Svg.prototype.gEnd = function() { return this.setEnd(); }
    Svg.prototype.svgEnd = function() { return this.setEnd(); }
    Svg.prototype.render = function(pa, i) {
      this.addChild(this.svgtr);
      if (isotype(pa)) AddChild(pa, this, i);
    }
    var svg = new Svg();
    if (sstarts(w, "#")) svg.$node = that.elem(w);
    if (svg.$node) return svg;
    SetSvg(svg, w, h, box, keepRatio);
    svg.setAttr({"xmlns":Svgns, "xmlns:xlink":Xlink});
    return svg;
  }
  JS.prototype.encodeParam = function(name, value) {
    if (isotype(name)) {
      var params = 0, result = "", p = "";
      ForEach(name, function(v, n) {
        if (!(p = this.encodeParam(n, v))) return;
        if (params == 0) { result = p; params = 1; }
        else { result += "&" + p; }
      });
      return result;
    }
    if (ntstype(name) || (ntstype(value) && ntntype(value))) return "";
    name = strim(name); // TODO: name.replace("%20", "+") ???
    value = isntype(value) ? "" + value : strim(value);
    if (!name || !value) return "";
    return encodeURIComponent(name) + "=" + encodeURIComponent(value);
  }
  JS.prototype.encodeUrl = function(url, params) {
    if (!isstype(url) || !strim(url)) return "";
    if (!isstype(params) || !strim(params)) return url;
    return strim(url) + "?" + strim(params); // check.cgi?item=1&color=red
  }
  JS.prototype.newAjax = function(h) {
    function CreateRequest() { // ajax can also be implemented by setting img/iframe/script's url
      if (isdefined(XMLHttpRequest)) return new XMLHttpRequest();
      if (ntdefined(ActiveXObject)) { logE("XMLHttpRequest/ActiveXObject", EDEF); return null; }
      if (this.store.xmlhttpver) return new ActiveXObject(this.store.xmlhttpver);
      var v = ["MSXML2.XMLHttp.6.0", "MSXML2.XMLHttp.3.0", "MSXML2.XMLHttp"], i = 0, ax = null;
      for (; i < v.length; ++i) {
        try { ax = new ActiveXObject(v[i]); this.store.xmlhttpver = v[i]; return ax; } catch (ex) {}
      }
      logE("ActiveXObject", EDEF);
      return null;
    }
    var Ajax = function(ax, h) {
      this.ajax = ax;
      this.handler = isftype(h) ? h : null;
      this.timeout = null;
      this.method = "GET";
      this.body = null;
      this.reqHeaders = {};
      this.rspStatus = "";
      this.rspStatusText = "";
      this.rspHeaders = {};
      this.rspData = "";
    }
    Ajax.prototype.setHeader = function(name, value) {
      if (isotype(name)) {
        ForEach(name, function(v, n) { this.setHeader(n, v); });
        return;
      }
      if (ntstype(name) || ntstype(value)) return;
      name = strim(name);
      value = strim(value);
      if (!this.reqHeaders[name]) {
        this.reqHeaders[name] = value;
      }
    }
    Ajax.prototype.setBody = function(body, contentType) {
      if (ntstype(body) || ntstype(contentType)) { logW("setBody", EARG); return; }
      body = strim(body);
      contentType = strim(contentType);
      if (!body || !contentType) { logW("setBody", EARG); return; }
      this.method = "POST";
      this.body = body;
      if (contentType == "form") contentType = "application/x-www-form-urlencoded";
      this.reqHeaders["Content-Type"] = contentType;
    }
    Ajax.prototype.abort = function() { this.ajax.abort(); }
    Ajax.prototype.send = function(url, to) {
      if (!this.ajax || !url || ntstype(url)) { logW("ajax url", EARG); return; }
      var ajax = this.ajax, that = this;
      ajax.onreadystatechange = function() {
        if (ajax.readyState != 4) return; // 4 is DONE
        if (that.timeout) { that.timeout.clear(); that.timeout = null; }
        that.rspStatus = ajax.status;
        that.rspStatusText = ajax.statusText;
        if (ajax.status < 200 || (ajax.status >= 300 && ajax.status != 304)) {
          if (that.handler) that.handler(that, "ajax error: status " + ajax.status + " " + ajax.statusText);
          return;
        }
        // that.rspHeaders ajax.getAllResponseHeaders();
        var type = strim(ajax.getResponseHeader("Content-Type"));
        if (contains(type, "json")) {
          var json = null;
          try { json = JSON.parse(ajax.responseText); } // result is a object
          catch (ex) { logE("JSON.parse responseText exception", ex); json = null; }
          that.rspData = json || "";
        } else { // responseXML is a document object for xml/xhtml
          if (type && !sstarts(type, "text")) logW("ajax unsupported Content-Type " + type);
          that.rspData = ajax.responseText || ""; // text/plain text/html text/css
        }
        if (that.handler) that.handler(that);
      }
      ajax.open(this.method, url, true); // open(method, url, async, username, password)
      ForEach(this.reqHeaders, function(v, n) { ajax.setRequestHeader(n, v); });
      if (ajax.overrideMimeType) { ajax.overrideMimeType("text/plain;charset=utf-8"); }
      ajax.send(this.method == "GET" ? null : this.body);
      var to = isntype(to) ? to : 60000; // 1min default
      if (!to || this.timeout) return;
      this.timeout = SetTimeout(to, function() {
        ajax.abort();
        if (that.handler) that.handler(that, "ajax timeout: " + to + "ms url(" + url + ")");
      });
    }
    return new Ajax(CreateRequest(), h);
  }
  JS.prototype.ready = function(h) {
    if (ntftype(h)) return;
    if (!this.isReady) { this.readyHandlers.push(h); return; }
    try { h(); } catch (e) { logE("readyHandler exception", e); throw e; }
  }
  window.J = new JS();
  (function Html5Shim() {
    var newElems = ["article","aside","footer","header","nav","section","main","figcaption","figure","details","menu","mark"];
    ForEach(newElems, function(ei) { NewNode(ei); });
  }());
  function CreateDebugElements() {
    var aside = J.elem("#page-console");
    if (!aside) return;
    var jslog = J.newElem("div#debug-jslog"), jsevt = J.newElem("div#debug-jsevt"), jstst = J.newElem("div#debug-jstst"),
    jsipt = J.newElem("@input#debug-jsipt"), jsbtn = J.newElem("@button#debug-jsbtn[value=RUN]");
    jsevt.addChild([jsipt, jsbtn]);
    aside.addChild([jslog, jsevt, jstst]);
  }
  function ReadyHandler(e, s) {
    var ready = J.isReady;
    if (!ready) {
      J.isReady = true;
      window.onerror = function(msg, url, line, colno) {
        var errstr = (url || location.href) + " ln" + line;
        if (isntype(colno)) errstr += " col" + colno;
        if (msg) errstr += ": " + msg;
        logE("onerror", errstr);
      }
      CreateDebugElements();
      ForEach(J.readyHandlers, function(hi, i) {
        if (ntftype(hi)) return;
        try { hi(); } catch (e) { logE("readyHandler " + (i+1) + " exception", e); throw e; }
      });
    }
    logD(s, "already ready? " + ready);
    EOMTEST(e);
  }
  StdAddEvent(document, "DOMContentLoaded", function(e) { ReadyHandler(e, "document.DOMContentLoaded"); });
  AddEvent(window, "load", function(e) { ReadyHandler(e, "window.onload"); });
}());
