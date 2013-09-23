/* -*- Mode: js2; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-  */
/*
 * welcome_to_the_grid_1.js
 * Copyright (C) 2013 Donald Lindsay <don.lindsay@gmail.com>
 * 
 * welcome_to_the_grid_1 is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * welcome_to_the_grid_1 is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
 
// This tutorial involves two parts. The first part,
// welcome_to_the_grid_1.js, demonstrates the native gtk
// application. The second part, welcome_to_the_grid_2.js, details the
// code specific to creating the grid.

// From the GJS tutorial located at:
// https://developer.gnome.org/gnome-devel-demos/ 

// Welcome to the Grid

// This tutorial will show you how to create basic widgets, or parts
// of the GNOME user interface, like Images and Labels. You'll then
// learn how to arrange them all in a Grid, which lets you put widgets
// exactly where you want them.

// Have you taken the first tutorial in this series already? You'll
// want to do so before continuing.

// Going native

// In the last tutorial, we created what was basically a GNOME window
// frame for a web app. All the GNOME-specific code we needed to learn
// revolved around putting the WebView -- the widget containing our
// application -- into an ApplicationWindow, and telling it to
// display. The application itself was written in HTML and JavaScript,
// just like most pages on the web.

// This time, we're going to use only native GNOME widgets. A widget
// is just a thing, like a checkbox or picture, and GNOME has a wide
// variety of them to choose from. We call them "native" widgets to
// distinguish them from things like the button and header in the web
// app we wrote. Because instead of using web code, these are going to
// be 100 percent GNOME, using GTK+.

// GTK+ stands for "GIMP Toolkit". It's like a toolbox of widgets that
// you can reach into, while building your applications. It was
// originally written for the GIMP, which is a free software image
// editor.

// Setting up our application

// Before we dig out any widgets from the GTK+ toolbox, we first need
// to write the basic boilerplate code that our application requires.

#!/usr/bin/gjs

const Gtk = imports.gi.Gtk;
const Lang = imports.lang;

// This part always goes at the start of your code. Depending on what
// you'll be doing with it, you may want to declare more imports
// here. What we're writing today is pretty basic, so these are all we
// need; Gtk for the widgets, and Lang so we can use Lang.bind to
// connect our application's activate and startup signals to the
// requisite functions.

// Speaking of which:

const WelcomeToTheGrid = new Lang.Class({
    Name: 'Welcome to the Grid',

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

// This is the start of the application itself, and the _init function
// which creates it. It tells _buildUI to create an ApplicationWindow,
// which we're going to call _window, and it tells our window to
// present itself whenever needed.

// This part, again, is pretty much copy-and-paste, but you always
// want to give your application a unique name.

    // Build the application's UI
    _buildUI: function() {

        // Create the application window
        this._window = new Gtk.ApplicationWindow({
            application: this.application,
            window_position: Gtk.WindowPosition.CENTER,
            border_width: 10,
            title: "Welcome to the Grid"});

// Finally, we start off the _buildUI function by creating a new
// ApplicationWindow. We specify that it goes with this application,
// that it should appear in the center of the screen, and that there
// should be at least 10 pixels between the outside edge and any
// widgets inside of it. We also give it a title, which will appear at
// the top of the window.

// Reaching into the GTK+ toolbox

// What widgets should we use? Well, let's say we want to write an
// application that looks like this:

// ./02_jsgrid_01.png

// We're going to need, at the very least, a picture and a text label
// to go with it. Let's start with the picture:

        // Create an image
        this._image = new Gtk.Image ({ file: "gnome-image.png" });

// You can download the image file used in this example here. 

// ./gnome-image.png

// Be sure to put it in the same directory as the code you're writing.

        // Create a label
        this._label = new Gtk.Label ({ label: "Welcome to GNOME, too!" });

// That code adds in the label beneath. You can see how we create
// widgets, here; each one is a part of Gtk, and we can give it
// properties that customize how it behaves. In this case, we set the
// Image's file property to be the filename of the picture we want,
// and the Label's label property to be the sentence that we want
// beneath the picture.

// Yes, it sounds redundant for a Label to have a label property, but
// it's not. Other widgets that contain text have a label property, so
// it's consistent for the Label widget to have one too.

// We can't just add these widgets to our window in order, though, the
// same way HTML elements appear in the order you write them. That's
// because an ApplicationWindow can only contain one widget.

// How do we get around that? By making that one widget a container
// widget, which can hold more than one widget and organize them
// inside it. Behold: The Grid.

        // Create the Grid
        this._grid = new Gtk.Grid ();

// We're not giving it any properties yet. Those will come later, as
// we learn how to use the Grid's powers. First, let's attach the
// Image and Label we made to our Grid.

        // Attach the image and label to the grid
        this._grid.attach (this._image, 0, 0, 1, 1);
        this._grid.attach (this._label, 0, 1, 1, 1);

// This code looks awfully complicated, but it's not. Here's what
// those numbers mean:

// The first number is what left-to-right position to put things in,
// starting from 0. Any widget that uses a 0 here goes all the way to
// the left.

// The second number is what top-to-botton position to put a given
// widget in, starting from 0. The Label goes beneath the Image, so we
// give the Image a 0 and the Label a 1 here.

// The third and fourth numbers are how many columns and rows a widget
// should take up. We'll see how these work in a minute.

        // Add the grid to the window
        this._window.add (this._grid);

        // Show the window and all child widgets
        this._window.show_all();
    }

});

// Run the application
let app = new WelcomeToTheGrid ();
app.application.run (ARGV);

// Now that we've created the Grid and attached all our widgets to it,
// we add it to the window and tell the window to show itself, as the
// last part of the _buildUI function. As always, to finish up we
// create a new instance of the application's class and tell it to
// run.
