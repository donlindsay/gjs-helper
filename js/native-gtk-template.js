/* -*- Mode: js2; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-  */
/*
 * native-gtk-template.js
 * Copyright (C) 2013 Donald Lindsay <don.lindsay@gmail.com>
 * 
 * native-gtk-template is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * js-gs-ext is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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


