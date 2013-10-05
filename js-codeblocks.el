;; -*- emacs-lisp -*-
;;; package_boiler_plate
;;; gpl2_boiler_plate 
;;
;;; Description: js2-codeblocks is a library of basic gjs javascript
;;;              source file elements common to gjs scripts. The code
;;;              blocks are given a few basic meta properties for
;;;              reading by emacs and copying to the gjs-app-buffer.
;;;
;;; Discussion: Optionally, this file can contain code that pushes
;;;             each code-block into a list of structs as an index. Or
;;;             that code can be in gjs-mode.el. Dunno, yet. For now,
;;;             it's there.
;;;
;;; Hammer Time:

(defstruct js-block 
  (js-block-name) (app-skel-name) (app-skel-slot)
  (js-block) (js-block-meta))

(make-js-block
 :js-block-name                 'common-header
 :app-skel-name                 'gtk
 :app-skel-slot                 'nil
 :js-block                 
 '("// -*- javascript -*- gjs-app-script
    #!/usr/bin/gjs
                                                            "
   )
 :js-block-meta                 'nil)

(make-js-block
 :js-block-name                'cinn-config
 :js-skel-name                 'cinn
 :app-skel-slot                'imports
 :js-block
 '("/* The name of this package (not localized) */
const PACKAGE_NAME = 'cinnamon';
/* The version of this package */
const PACKAGE_VERSION = '1.8.8';
/* The version of GJS we're linking to */
const GJS_VERSION = '1.34.0';
/* 1 if gnome-bluetooth is available, 0 otherwise */
const HAVE_BLUETOOTH = 1;
/* The system TLS CA list */
const CINNAMON_SYSTEM_CA_FILE = '/etc/ssl/certs/ca-certificates.crt';
                                                            "
   )
   :js-block-meta              'nil)

;; I don't know if this block is generic enough for this file, perhaps
;; it can go in an ancilliary js-codeblocks-extras file.
(make-js-block
 :js-block-name                'cinn-search
 :app-skel-name                'cinn
 :app-skell-slot               'webkit
 '("// ==UserScript==

// @name           Mint Search Enhancer

// @namespace      linuxmint

// @description    Enhances the results given by Google CSE

// @include        http://*.google.*/cse*

// @include        http://google.*/cse*

// @include        http://*.google.*/custom*

// @include        http://google.*/custom*

// ==/UserScript==


function gup( name )
{
  name = name.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
  var regexS = "[\\?&]"+name+"=([^&#]*)";
  var regex = new RegExp( regexS );
  var results = regex.exec( window.location.href );
  if( results == null )
    return "";
  else
    return results[1];
}

function insertAfter(parent, node, referenceNode) {
  parent.insertBefore(node, referenceNode.nextSibling);
}

function selectNodes(doc, context, xpath)
{
   var nodes = doc.evaluate(xpath, context, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
   var result = new Array( nodes.snapshotLength );

   for (var x=0; x<result.length; x++)
   {
      result[x] = nodes.snapshotItem(x);
   }

   return result;
}

if (window.top != window.self)  //don't run on frames or iframes
    return;

var query = gup('q');

var logo = document.createElement("div");
logo.innerHTML = '<style>' +
'hr.thin {height: 1px;border: 0;color: #333;background-color: #CCDDff;width: 100%;}' +
'.menu{position:relative; margin:0 auto; padding: 0; height:30px; width:100%; display:block; background:url("http://search.linuxmint.com/search/images/topMenuImages.png") repeat-x;}' +
'.menu li{padding:0; margin:0; list-style:none; display:inline;}' +
'.menu li a{float:left; padding-left:15px; display:block; color:rgb(77,102,204); text-decoration:none; font:12px Verdana, Arial, Helvetica, sans-serif; cursor:pointer; background:url("http://search.linuxmint.com/search/images/topMenuImages.png") 0px -30px no-repeat;}' +
'.menu li a span{line-height:30px; float:left; display:block; padding-right:15px; background:url("http://search.linuxmint.com/search/images/topMenuImages.png") 100% -30px no-repeat;}' +
'.menu li a:hover{background-position:0px -60px; color:rgb(77,102,204);}' +
'.menu li a:hover span{background-position:100% -60px;}' +
'.menu li a.active, .menu li a.active:hover{line-height:30px; font:12px Verdana, Arial, Helvetica, sans-serif; background:url("http://search.linuxmint.com/search/images/topMenuImages.png") 0px -90px no-repeat; color:rgb(0,0,0);}' +
'.menu li a.active span, .menu li a.active:hover span{background:url("http://search.linuxmint.com/search/images/topMenuImages.png") 100% -90px no-repeat;}' +
' </style>' +
'<table><tr><td><ul class="menu">'+
'  <li><a href="#" class="active"><span><b>Web<b/></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=images&query=' + query + '"><span>Images</span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=video&query=' + query + '"><span>Videos</span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=maps&query=' + query + '"><span>Maps</span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=news&query=' + query + '"><span>News</span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=translate&query=' + query + '"><span><font color="#969aab">Translate</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=wikipedia&query=' + query + '"><span><font color="#000000" face="times, serif" size="3">Wikipedia</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=youtube&query=' + query + '"><span><font color="#000000">You</font><font color="#ff3434">Tube</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=ebay&query=' + query + '"><span><font color="#ff0000">e</font><font color="#000099">b</font><font color="#ffcc00">a</font><font color="#99cc00">Y</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=amazon&query=' + query + '"><span><font color="#000000" face="sans-serif" size="2">Amazon</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=imdb&query=' + query + '"><span><font color="#dfc93c">IMDB</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=flickr&query=' + query + '"><span><font color="#0063dc">flick</font><font color="#ff0084">r</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=yahoo&query=' + query + '"><span><font color="#7b0099">Yahoo</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=answers&query=' + query + '"><span><font color="#003399">Answers</font><font color="#000000">.</font><font color="#7ebe00">com</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=account"><span><font color="#969aab">My Account</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=gmail"><span><font color="#416adc">G</font><font color="#e65a41">M</font><font color="#da9609">a</font><font color="#4169db">i</font><font color="#07851b">l</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=calendar"><span><font color="#969aab">Calendar</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=documents"><span><font color="#969aab">Documents</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=reader"><span><font color="#969aab">Reader</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=sites"><span><font color="#969aab">Sites</font></span></a></li>' +
'  <li><a href="http://search.linuxmint.com/search?link=photos"><span><font color="#809199">Picasa</font></span></a></li>' +
'</ul></td></tr>' +
'<tr><td>&nbsp;</td></tr></table>';

document.body.insertBefore(logo, document.body.firstChild);

var doc = window.document;

// Get a list of all A tags that have an href attribute containing the start and stop key strings.
var googLinks = selectNodes(doc, doc.body, "//A[@class='l']");
var googResults = selectNodes(doc, doc.body, "//DIV[@class='g']");

for (var x=0; x<googLinks.length; x++)
{
    var glink = googLinks[x].href;
    var gtitle = googLinks[x].innerHTML.replace(/<[^>]+>/ig,"");
    gtitle = gtitle.replace(/\'/ig,"\\'");
    var postText = " - <a href=\"http://search.linuxmint.com/search?query=cache:" + encodeURIComponent(glink) + "\"><font size=2 color=\"#7777CC\">Cached</font></a>" +
         " - <a href=\"http://search.linuxmint.com/search?query=related:" + encodeURIComponent(glink) + "\"><font size=2 color=\"#7777CC\">Similar pages</font></a>";
    var postLink = document.createElement('font');
    postLink.setAttribute('size','-1');
    postLink.innerHTML = postText;
    if (googResults[x] != null) {
        if (googResults[x].getElementsByTagName('nobr') != null) {
            if (googResults[x].getElementsByTagName('nobr')[0] != null) {
                googResults[x].getElementsByTagName('nobr')[0].appendChild(postLink);
            }
        }
    }
}
                                                            "
	 )
 :js-block-meta               'nil)


(make-js-block
 :js-block-name                'common-imports
 :app-skel-name                 'gtk
 :app-skel-slot                 'imports
 :js-block
 '("const Gtk = imports.gi.Gtk;
    const Lang = imports.lang;                    
                                                            "
   )
 :js-block-meta                 'nil)

(make-js-block
 :js-block-name:                'common-application
 :app-skel-name                 'gtk
 :app-skel-slot                 'nil
 :js-block
 '("const gjsApplicationName = new Lang.Class({         
      Name: gjsApplicationTitle,                      
        _init: function() {
          this.application = new Gtk.Application();
        this.application.connect('activate', 
          Lang.bind(this, this._onActivate));
        this.application.connect('startup', 
          Lang.bind(this, this._onStartup));
    },
    _onActivate: function() {
      this._window.present();
    },
    _onStartup: function() {
      this._buildUI ();
    },
    _buildUI: function() {
      this._window = new Gtk.ApplicationWindow({
        application: this.application,
        window_position: Gtk.WindowPosition.CENTER,
        border_width: 10,
        title: gjsApplicationTitle});                  
                                                            "
  )
 :js-block-meta                 'nil)

(make-js-block
 :js-block-name                 'gtk-grid
 :app-skel-name                 'gtk
 :app-skel-slot                 'grid
 :js-block
 '("this._grid = new Gtk.Grid ();                             
        this._grid.attach (this._image, 0, 0, 1, 1);        
        this._grid.attach (this._label, 0, 1, 1, 1);        
      this._window.add (this._grid);
                                                            "
   )
 :js-block-meta                 'nil)

(make-js-block
 :js-block-name                 'gtk-image
 :app-skel-name                 'gtk
 :app-skel-slot                 'image
 :js-block                
 '("this._image = new Gtk.Image ({ file: gjsImage });       "
   )
 :js-block-meta                 'nil)        

(make-js-block
 :js-block-name                 'gtk-label
 :app-skel-name                 'gtk
 :app-skel-slot                 'label
 :js-block                
 '("this._label = new Gtk.Label ({ label: gjsLabel});       "
   )
 :js-block-meta                 'nil)        

(make-js-block
 :js-block-name                 'common-footer
 :app-skel-name                 'nil
 :app-skel-slot                 'nil
 :js-block                
 '("// end gjs-app-script                                   "
   )
 :js-block-meta                 'nil)
