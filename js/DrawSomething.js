//Import the clutter class from the gi repository (the object introspection repository).
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

// We connect the destroy event to quit from the main loop when we close the
// window.
stage.connect("destroy", Clutter.main_quit);
// Put some tittle
stage.title = "Test";
// Set a color to the stage to show that it is working
stage.set_background_color(new Clutter.Color({
    red : 10,
    blue : 10,
    green : 10,
    alpha : 255
}));

// Create a new actor
let actorRectangle = new Clutter.Actor();
// Make it like a rectangle
actorRectangle.set_size(100, 100);
actorRectangle.set_position(100, 100);
/*
 * Colors are made in RGBA http://en.wikipedia.org/wiki/RGBA_color_space
 * Basically, Red, Green, Blue, Alpha(transparency). Each factor, between 0-255
 */
actorRectangle.set_background_color(new Clutter.Color({
    red : 100,
    blue : 100,
    green : 100,
    alpha : 255
}));

stage.add_actor(actorRectangle);
// As we say, the stage is also an actor, so we show it to make visible
stage.show();
// Start a main loop so that the stage can animate its contents and respond to
// user interaction.
Clutter.main();
