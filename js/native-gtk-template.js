// native-gtk-template.js
#!/usr/bin/gjs

// Variables set by gjs-mode at runtime

// gjsImports
// gjsApplicationName
// gjsApplicationTitle
// gjsImage
// gjsLabel
// gjsGrid

const Gtk = imports.gi.Gtk;                         // Widgets
const Lang = imports.lang;                          // Lang.bind
// gjsImports go here

const gjsApplicationName = new Lang.Class({         // gjsApplicationName
    Name: gjsApplicationTitle,                      // gjsApplicationTitle
    // Create the application itself
    _init: function() {
        this.application = new Gtk.Application();
        // Connect 'activate' and 'startup' signals to the callback functions
        this.application.connect('activate', Lang.bind(this, this._onActivate));
        this.application.connect('startup', Lang.bind(this, this._onStartup));
    },
    // Callback function for 'activate' signal presents windows when active
    _onActivate: function() {
        this._window.present();
    },
    // Callback function for 'startup' signal builds the UI
    _onStartup: function() {
        this._buildUI ();
    },
    // Build the application's UI
    _buildUI: function() {
        // Create the application window
        this._window = new Gtk.ApplicationWindow({
            application: this.application,
            window_position: Gtk.WindowPosition.CENTER,
            border_width: 10,
            title: gjsApplicationTitle});

        // Images
        this._image = new Gtk.Image ({ file: gjsImage });   // gjsImage

        // Labels
        this._label = new Gtk.Label ({ label: gjsLabel});   // gjsLabel

        // Create the Grid
        this._grid = new Gtk.Grid ();                       // gjsGrid       

        // Attach the image and label to the grid
        this._grid.attach (this._image, 0, 0, 1, 1);        // gjsImage
        this._grid.attach (this._label, 0, 1, 1, 1);        // gjsLabel

        // Add the grid to the window
        this._window.add (this._grid);

        // Show the window and all child widgets
        this._window.show_all();
    }

});

// Run the application
let app = new gjsApplicationName ();
app.application.run (ARGV);


