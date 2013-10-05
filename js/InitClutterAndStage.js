//Import the clutter class form the gi repository(the object introspection repository)
const Clutter = imports.gi.Clutter;
// Initialize clutter
Clutter.init(null);
/*
 * Create a stage. This function returns a new default stage, with its own
 * window. ClutterStage is derived from the ClutterActor object so many of that
 * object's functions are useful for the stage. For instance, call
 * Clutter.Stage.get_default().show() to make the stage visible.
 */
let stage = new Clutter.Stage();

// We connect the destroy event to quit from the mainloop when we close the
// window.
stage.connect("destroy", Clutter.main_quit);
// Put some title
stage.title = "Test";
// Set a color to the stage to show that it is working
stage.set_background_color(new Clutter.Color({
    red : 150,
    blue : 0,
    green : 0,
    alpha : 255
}));
// As we say, the stage is also an actor, so we shoe it to make visible
stage.show();
// Start a main loop so that the stage can animate its contents and respond to
// user interaction.
Clutter.main();
