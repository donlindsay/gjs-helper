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
 :js-block-name:                'common-imports
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
