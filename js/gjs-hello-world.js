#!/usr/bin/gjs
// Initialize GTK+
var Gtk = imports.gi.Gtk;
Gtk.init(null, 0);

// Create your window, name it, and connect the “click x to quit” function.
var mywindow = new Gtk.Window({type: Gtk.WindowType.TOPLEVEL});
mywindow.title = 'Hello World!';
mywindow.connect('destroy', function(){Gtk.main_quit()});

// Add some text to your window
var label = new Gtk.Label({label: 'Hello World'});
mywindow.add(label);

// Make the label and the window itself visible to the user
label.show();
mywindow.show();

// Let the user run the app
Gtk.main();
