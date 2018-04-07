@@HTML
-- @param LANG the language attribute for html element, default is chinese
-- @param CHARSET the charset of the document, defaultis utf-8
-- @param METAS a array of meta elements, default is null
-- @param title a text for the title of the document, default is "Untitled Page"
-- @param CSSS a array of style sheet elements, default is null
-- @param JSS a array of script elements, default is null
-- @param body the body of the documents, default is null
<!DOCTYPE html>
<html(@space+LANG.cn)>
  <head>
    <meta charset="(@CHARSET.utf8)">
    <meta http-equiv="X-UA-Compatible" content="IE=Edge">(@break+METAS)
    <title>(@title.Untitled Page)</title>(@break+CSSS)(@break+JSS)
  </head>
  <body>(@break+body)
  </body>
</html>

@@SMPLGRID
<div id="root-container">
  <header id="page-header">(@break+header)</header>
  <main id="page-main">(@break+main)</main>
  <nav id="page-nav">(@break+nav)</nav>
  <aside id="page-console" class="hidden"></aside>
  <footer id="page-footer">(@break+footer)</footer>
</div>

@@HTML.smplgrid
  @body = (@SMPLGRID)

@@LANG:
-- language code: http://xml.coverpages.org/iso639a.html
-- contry code: http://xml.coverpages.org/country3166.html
-- language subtag: http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
-- lang attrubute value format: primary-subcode-subcode-...
  .zh -- chinese
  .en -- english
  .fr -- french
  .it -- italian
  .ja -- japanese
  .ru -- russian
  .ar -- arabic
  .de -- german
  .es -- spanish
  .pt -- portugueses
  .he -- hebrew
  .hi -- hindi
  .cn = "zh-CN"
  .us = "en-US"

@@CHARSET:
  .utf8 = "utf-8"
  .latin = "ISO-8859-1" -- latin alphabet part 1

@@CSS
-- @param url the url of the style sheet
<link rel="stylesheet" href="(@url)">

@@JS
-- @param url the url of the script file
<script src="(@url)"></script>

