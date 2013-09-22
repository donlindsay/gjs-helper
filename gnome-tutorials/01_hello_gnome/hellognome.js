// First, we need to tell GNOME that this is a JavaScript application,
// which uses gjs. Gjs is GNOME's way of turning your JavaScript code
// into instructions it understands, so this line always has to go at
// the start of your applications.

#!/usr/bin/gjs

// After that, we need to tell GNOME which libraries we want to
// import.

const GLib = imports.gi.GLib;
const Gtk = imports.gi.Gtk;
const Lang = imports.lang;
const Webkit = imports.gi.WebKit;

// Just like how add-on libraries like jQuery let us do extra things
// with JavaScript, each of these libraries gives us new capabilities
// for our GNOME apps:

// Gtk and Lang are basic parts of any GNOME application, which let
// you create windows and widgets and tie them together.

// GLib is a helper library, which lets us do things like tell GNOME
// where the hellognome.html file we created is.

// And Webkit is a web rendering engine, which we'll use to basically
// create a browser window to open our HTML file with.

// Now we create the application itself:

const HelloGNOME = new Lang.Class ({
    Name: 'Hello GNOME',

// This will look familiar to you if you've worked with
// object-oriented JavaScript before. That's right; our whole
// application is a class called HelloGNOME. And as you can see, we've
// given it a property that says what its name is.

    // Create the application itself
    _init: function () {
        this.application = new Gtk.Application ();

        // Connect 'activate' and 'startup' signals to the callback functions
        this.application.connect('activate', Lang.bind(this, this._onActivate));
        this.application.connect('startup', Lang.bind(this, this._onStartup));
    },

    // Callback function for 'activate' signal presents windows when active
    _onActivate: function () {
        this._window.present ();
    },

    // Callback function for 'startup' signal builds the UI
    _onStartup: function () {
        this._buildUI ();
    },

// Here's some code you will more or less copy-and-paste for every
// JavaScript application you build. It creates a new Application, and
// then binds its activate and startup signals to functions that make
// the window show itself and build its user interface, respectively.

// What does that mean? Well, everything in a GNOME application sends
// out a signal when something important happens. A button might send
// out the clicked signal when you click on it, for instance. Our job
// is to connect the signals to functions which handle them, and make
// the things that we want to have happen occur. We do this using each
// object's connect method, which takes two arguments: The signal we
// want to handle, and the Lang.bind function, which we have to use to
// tell connect which function we want to have handle the signal.

// In this case, we want _onActivate to handle the activate signal,
// and _onStartup to handle the startup signal. _onActivate just tells
// the window to present itself; so basically, whenever you Alt+Tab to
// the application it appears, like you would expect it to. _onStartup
// calls _buildUI, which is the function that creates our user
// interface and is the next part that we will look at.

// When you copy and paste the above code for your own applications,
// be sure to change the name to a unique one each time.  

// Designing our window's UI

// In the _buildUI function, we're going to tell GNOME about our
// window and the things inside it, one at a time. After that, we're
// going to connect everything together and put it all on display.

    // Build the application's UI
    _buildUI: function () {

        // Create the application window
        this._window = new Gtk.ApplicationWindow  ({
            application: this.application,
            title: "Welcome to GNOME",
            default_height: 200,
            default_width: 400,
            window_position: Gtk.WindowPosition.CENTER });

// The first object we create is an ApplicationWindow. It needs a
// title to go in the title bar, and its application property needs to
// be the application that we created, above. Beyond that, there are
// various ways of customizing how it looks, which the
// ApplicationWindow reference page will go into more detail about. As
// you can see here, we gave it a default height and width (measured
// in pixels), and told GNOME we want our window to appear in the
// center of the screen.

        // Create a webview to show the web app
        this._webView = new Webkit.WebView ();

        // Put the web app into the webview
        this._webView.load_uri (GLib.filename_to_uri (GLib.get_current_dir() +
            "/hellognome.html", null));

// Remember how we imported Webkit right at the start? Here we're
// creating a new instance of a Webkit class called a WebView, which
// is more or less a browser window you can put inside of your
// app. After that, we then give it the URI that we want it to load
// when the application starts up.

// We could just give it a web URI, like http://gnome.org. Instead,
// here we use a couple of GLib helper functions to tell the WebView
// where our hellognome.html file is. GLib.get_current_dir returns the
// directory that our app's running in, and GLib.filename_to_uri turns
// our file's path and filename into a URI that the WebView's load_uri
// function understands. (filename_to_uri's second parameter should be
// null unless you know what it's used for and have a reason for
// changing it.)

        // Put the webview into the window
        this._window.add (this._webView);

        // Show the window and all child widgets
        this._window.show_all();
    },

});

// Each window can hold one, and only one, widget. Normally, we'd use
// a container widget like a Grid to put multiple widgets into, then
// use the window's add function to add the Grid to it. Here, we just
// need the WebView, so that's all we add to the window. After that,
// as the last part of the _buildUI function that creates our window,
// we tell the window to show itself and its contents.

// Run the application
let app = new HelloGNOME ();
app.application.run (ARGV);

// Finally, we create a new instance of our HelloGNOME class, and tell
// GNOME to run it.
