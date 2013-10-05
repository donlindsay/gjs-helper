//Import the clutter class from the gi repository(the object introspection repository)
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
let
actorRectangle = new Clutter.Actor();
// Put his x, y size, position and background color
actorRectangle.set_size(100, 100);
actorRectangle.set_position(100, 100);
actorRectangle.set_background_color(new Clutter.Color({
    red : 100,
    blue : 100,
    green : 100,
    alpha : 255
}));

// Sets actor as reactive. Reactive actors will receive events.
actorRectangle.set_reactive(true);
/*
 * Connect the actor to a event. When you hover it, function in the second
 * parameter will be called. So we are passing a reference to a function.
 */
actorRectangle.connect('enter-event', changeRectanglePosition);

stage.add_actor(actorRectangle);
// As we say, the stage is also an actor, so we show it to make visible
stage.show();
// Start a main loop so that the stage can animate its contents and respond to
// user interaction.
Clutter.main();

function changeRectanglePosition()
{
    // We get the size of the stage (our window)
    let [sizex, sizey] = stage.get_size();
    /*
     * Math.random returns a float between 0 and 1, so we multiply by the size of
     * the stage and we acomplish a number between 0-sizeStage
     */
    let newx = Math.floor(Math.random() * sizex);
    let newy = Math.floor(Math.random() * sizey);
    /*
     * We can access to that because it is a global variable. Also, remember that
     * with "let" the scope is the block and with "var" the scope is all the
     * environment. In this case,the block is all the environment We put the new
     * random position
     */
    actorRectangle.set_position(newx, newy);
}
