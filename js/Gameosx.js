/*
 * Code from http://townx.org/blog/elliot/introduction-sorts-javascript-desktop-application-development-gjs-and-clutter
 * Commented and update to clutter 1.10 by Carlos Soriano
 */

/*
 * TODO:
 * - use a layout to put rectangles on the stage, rather than absolute positioning
 * - fix file searchPath so it works when run from anywhere
 */

//X is always the first player
//NB relies on a patched gjs with a 'print' function (used to show the winner)

//import local file - NB this means the script only works from this directory at the moment
imports.searchPath.push('.');
const Ox = imports.GameBoard;
//Import clutter from gi repository (GObjectintrospection)
const Clutter = imports.gi.Clutter;
//This class help you to enclosure the "this"
const Lang = imports.lang;

const OX_PLAYERS = ["X", "O"];
const OX_SQUARE_SIZE_PX = 100;
const OX_FONT_SIZE = OX_SQUARE_SIZE_PX * 0.95;
const OX_LINE_WIDTH_PX = 10;
const OX_SIDE_SIZE = 3;
const OX_TEXT_COLOR = new Clutter.Color( {'red':255, 'blue':255, 'green':255, 'alpha':255} );
const OX_STRIKE_COLOR = new Clutter.Color( {'red':255, 'blue':0, 'green':0, 'alpha':255} );

/*
    Class: BoardView

        Class to define the view of the board, it means, to define the visual of our
        program. There is where Clutter have the action

    Parameters:

        players - number of players
        sideSize - number of squares to play
 */
function BoardView(players, sideSize) {
    this._init(players, sideSize);
};

BoardView.prototype = {
        _init : function (players, sideSize) {
            this.board = new Ox.Board(players, sideSize);
            /*
             The board size will be the side size per the size of the square
             to play plus the lines between squares to separate squares to
             make the visual more clear
             */
            this.boardSizePx = (sideSize * OX_SQUARE_SIZE_PX) + (OX_LINE_WIDTH_PX * (sideSize + 1));
            // Initialize clutter
            Clutter.init (null);
            // Create new stage for our window and our actors
            this.stage = new Clutter.Stage();
            // We connect the destroy event to quit from the mainloop when we close the
            // window.
            this.stage.connect("destroy", Clutter.main_quit);
            //Set the title
            this.stage.title = "3 en raya";
            // The size of the visual board
            this.stage.set_size(this.boardSizePx, this.boardSizePx);

            let colorOfSquare = new Clutter.Color( {'red':50, 'blue':50, 'green':50, 'alpha':255} );

            /*
             We create a square actor for each place to play. Also, we connect
             each square actor to a mouse event for when we click in the
             square actor to play with this square. This event will cause that
             the actor will be painted as played, and check at the same time,
             the actor will check if someone win the game.
             */
            for (let i = 0; i < sideSize; i++)
            {                       
                for (let j = 0; j < sideSize; j++)
                {
                    let xpos = ((i + 1) * OX_LINE_WIDTH_PX) + (i * OX_SQUARE_SIZE_PX);
                    let ypos = ((j + 1) * OX_LINE_WIDTH_PX) + (j * OX_SQUARE_SIZE_PX);

                    let squareActor = new Clutter.Actor();
                    squareActor.set_size(OX_SQUARE_SIZE_PX, OX_SQUARE_SIZE_PX);
                    squareActor.set_background_color(colorOfSquare);
                    squareActor.set_position(xpos, ypos);
                    /*
                     We make the actor reactive, because we want to make the
                     square interactive and responsible of the mouse events.
                     */                                 
                    squareActor.set_reactive(true);

                    let x = i;
                    let y = j;
                    /*
                     We connect the press event of the mouse to a function.
                     The function is defined inside. It is called "anonymous
                     function" because, as you see it hasn't got a name. It is
                     very useful to define functions quickly that is not
                     needed outside there.

                     Also you can see that we use Lang.bind(). As you can see
                     in this tutorial, in Knowing javascript->introducing
                     javascript, we need this because we will use the "this"
                     outside of his scope. This is a real example of this. If
                     you think a little, when a event is happens, the funtion
                     in the second parameter is called. BUT, we called a
                     function that inside it uses the "this"...and the "this"
                     at the moment of the calling is not THIS object, it is
                     the object that do the callback. So, we need to "close"
                     the this inside a virtual closure. We acomplish this
                     using Lang.bind.

                     In summary, we need Lang.bind when we will use the "this"
                     outside of his scope. A example is in callbacks. All
                     callbacks that we use with the "this" word inside we will
                     need the Lang.bind.

                     If you don't know what a callback is, see
                     http://en.wikipedia.org/wiki/Callback_(computer_programming)

                     Basically is a function as a parameter to another
                     function.
                     */ 
                    squareActor.connect('button-press-event',
                            Lang.bind(this, function(actor, event)
                                    {
                                // Make sure that the we didn't play in this
                                // square before
                                if (this.board.canMove(x, y))
                                {
                                    let nextPlayer = this.board.getNextPlayer();

                                    // Get the line if some player won
                                    let line = this.board.makeMove(x, y, nextPlayer);

                                    this.markMove(squareActor, nextPlayer);
                                    // If line returned is a valid win play
                                    if (line.winsFor())
                                    {
                                        this.strikeThrough(line);
                                    }
                                    // If nobody win, swicth player
                                    this.board.switchPlayer();
                                }
                                    }));
                    // We add each square to the stage
                    this.stage.add_actor(squareActor);
                }
            }
        },

        /*
             Function:
             
                 Draw the line when some player won.
                 
             Parameters:
             
                 line - The line that we will draw.
                 
             See Also:
             
                 <Line>
         */
        strikeThrough : function (line)
        {
            let first = line.first();
            let last = line.last();

            let height = 0;
            let width = 0;
            let x = 0;
            let y = 0;
            let rotate = 0;
            let straight_line_length = this.boardSizePx * 0.95;

            if (first.getX() == last.getX())
            {
                // column
                width = OX_LINE_WIDTH_PX  / 2;
                height = straight_line_length;
                x = ((first.getX() + 0.5) * OX_SQUARE_SIZE_PX) + ((first.getX() + 0.75) * OX_LINE_WIDTH_PX);
                y = (this.boardSizePx - straight_line_length) / 2;
            }
            else if (first.getY() == last.getY())
            {
                // row
                width = straight_line_length;
                height = OX_LINE_WIDTH_PX / 2;
                x = (this.boardSizePx - straight_line_length) / 2;
                y = ((first.getY() + 0.5) * OX_SQUARE_SIZE_PX) + ((first.getY() + 0.75) * OX_LINE_WIDTH_PX);
            }
            else
            {
                // diagonal, length calculated aplying Pitagoras theorem
                width = Math.sqrt(straight_line_length * straight_line_length * 2);
                height = OX_LINE_WIDTH_PX / 2;
                x = (this.boardSizePx - width) / 2;
                y = (this.boardSizePx / 2) - (height / 2);

                if (first.getX() == first.getY()) {
                    rotate = 45;
                }
                else {
                    rotate = -45;
                }
            }
            // Create a new rectangle to draw a line
            let strike = new Clutter.Actor ();
            strike.set_background_color (OX_STRIKE_COLOR);
            strike.set_position (x, y);
            strike.set_size (width, height);

            /*
             If the line is in diagonal, we have to rotate the actor. To
             rotate it we have to indicate the axis to rotate and the center
             of the rotation. The axis will be z (trougth the screen), and the
             center of rotation will be the center of the square actor. The
             last 0 is the depth, that as you can imagine, it haven't got
             depth.
             http://docs.clutter-project.org/docs/clutter/stable/ClutterActor.html#clutter-actor-set-rotation
             */
            if (rotate != 0) {
                strike.set_rotation (Clutter.RotateAxis.Z_AXIS, rotate, width / 2, height / 2, 0);
            }
            // add the line actor to the stage to sow it
            this.stage.add_actor(strike);
        },
        /*
            Function: markMove

                This function allow you to show a move from one player. The function
                will draw a "X" or a "O" depending of the player.

            Parameters:
                clickedSquare - The square that is been clicked by the user.
                player - wich player click the square.         
         */
        markMove : function(clickedSquare, player)
        {
            //Put the letter associated with this player("X" or "O") in a text actor of clutter
            let letterToDraw = new Clutter.Text( {"text":player, "color":OX_TEXT_COLOR} );
            //Set the font, and size of the text
            letterToDraw.set_font_name("Sans Bold " + OX_FONT_SIZE + "px");
            //Get the position of the rectangle
            let [r_x, r_y] = clickedSquare.get_position();
            let offset_x = (clickedSquare.get_width() / 2) - (letterToDraw.get_width() / 2); 
            let offset_y = (clickedSquare.get_height() / 2) - (letterToDraw.get_height() / 2);

            letterToDraw.set_position(r_x + offset_x, r_y + offset_y);
            letterToDraw.move_anchor_point_from_gravity (Clutter.Gravity.CENTER);

            this.stage.add_actor(letterToDraw);
            /*
             WARNING: The next code is deprecated. Instead of this we have to use a simple
             code. The code of the new version will be:
              actorRectangle.animate(Clutter.AnimationMode.EASE_OUT_ELASTIC, 500,
             "scale-x", 0.5, "scale-y", 0.5, NULL);
             */

            /*
             Create a new timeline as we see in the animation headland(its function it's like the time bar of youtube
              videos) with duration 500ms
             */
            let timeline = new Clutter.Timeline( {'duration':500} );
            /*
             Create a new function to control the timeline and the properties of the actor.
             This will use the ease out elastic type of animation, resulting in a animation that cause the letter
             seems an elastic thing
             */
            let alpha = new Clutter.Alpha ( {'timeline':timeline, 'mode':Clutter.AnimationMode.EASE_OUT_ELASTIC} );
            //Create a new behaviour to the actor. We want that the actor scale to 0.5 of its size.
            let behaviour = new Clutter.BehaviourScale( {'alpha':alpha, 'x_scale_start':1.0, 'y_scale_start':1.0, 
                'x_scale_end':0.5, 'y_scale_end':0.5} );
            //Apply the behaviour to the letter actor
            behaviour.apply (letterToDraw);
            //Start the animation
            timeline.start ();
        },

        show : function()
        {
            //Show the stage and his actors
            this.stage.show();
            //Start our program with the GUI we created
            Clutter.main();
        }
};

//Create our GUI
let view = new BoardView(OX_PLAYERS, OX_SIDE_SIZE);
//Start our GUI
view.show();
