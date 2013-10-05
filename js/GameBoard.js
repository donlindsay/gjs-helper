/*
    Code from http://townx.org/blog/elliot/introduction-sorts-javascript-desktop-application-development-gjs-and-clutter
    Commented and update to clutter 1.10 by Carlos Soriano
 */

/*
    Define what is a square in the game
    We use this way to define a class,
    using the prototype.
    Visit <Ways to define a "class" in JS at http://www.phpied.com/3-ways-to-define-a-javascript-class>
 */

/*
  Class: Square

  Define a square to play in the board This "class" use the method
  prototype to do define a "class" in javascript

  Parameters:
       x - position x of the square. For normal use, 0 to 2
       y - position y of the square. for normal use, 0 to 2
 */
function Square(x, y, player) {
    this.player = player;
    this.x = x;
    this.y = y;
};

Square.prototype = {
        getPlayer : function () {
            return this.player;
        },

        setPlayer : function (player) {
            this.player = player;
        },

        getX : function () {
            return this.x;
        },

        getY : function () {
            return this.y;
        }
};


/*
  Class: Line

       Class to define the line drawn when some player won
 */
function Line () {
    this.squares = [];
};

Line.prototype = {
        first : function () {
            return this.squares[0];
        },

        last : function () {
            return this.squares[this.squares.length - 1];
        },

        addSquare : function (square) {
            this.squares.push(square);
        },

        winsFor : function () {
            let current = null;
            let last = null;

            for (let i = 0; i < this.squares.length; i++) {
                current = this.squares[i].getPlayer();
                if (i > 0 && current != last) {
                    last = null;
                    break;
                }
                else {
                    last = current;
                }
            }
            return last;
        },

        clear : function () {
            this.squares = [];
        }
};

/*
  Class: Board

        Define the board of the game

  Parameters:

        players - The count of the players
        sideSize - The quantity of squares can have the board in a side
 */

function Board(players, sideSize) {
    this._init(players, sideSize);
};

Board.prototype = {

        _init : function(players, sideSize) {
            this.squares = [];
            this.sideSize = sideSize;
            this.winner = null;
            this.playerPos = 0;
            this.players = players;

            for (var i = 0; i < sideSize; i++) {
                this.squares[i] = [];
                for (var j = 0; j < sideSize; j++) {
                    this.squares[i][j] = new Square(i, j, null);
                }
            }
        },

        switchPlayer : function() {
            this.playerPos = this.playerPos + 1;
            if (this.playerPos >= this.players.length) {
                this.playerPos = 0;
            }
        },

        getNextPlayer : function() {
            return this.players[this.playerPos];
        },

        makeMove : function(x, y, player) {
            this.squares[x][y].setPlayer(player);
            let line = this.getWinningLine();
            return line;
        },

        canMove : function(x, y) {
            return !this.winner && this.squares[x][y].getPlayer() == null;
        },

        getWinningLine : function () {
            let line = new Line();
            let x = 0;
            let y = 0;

            // check columns
            while (this.winner == null && x < this.sideSize) {
                line.clear();
                for (y = 0; y < this.sideSize; y++) {
                    line.addSquare(this.squares[x][y]);
                }
                this.winner = line.winsFor();
                x++;
            }

            // check rows
            y = 0;
            while (this.winner == null && y < this.sideSize) {
                line.clear();
                for (x = 0; x < this.sideSize; x++) {
                    line.addSquare(this.squares[x][y]);
                }
                this.winner = line.winsFor();
                y++;
            }

            // check bottom right to top left diagonal
            if (this.winner == null) {
                line.clear();
                for (x = 0, y = 0; x < this.sideSize && y < this.sideSize; x++, y++) {
                    line.addSquare(this.squares[x][y]);
                }
                this.winner = line.winsFor();
            }

            // check bottom left to top right diagonal
            if (this.winner == null) {
                line.clear();
                for (x = 0, y = this.sideSize - 1; x < this.sideSize && y >= 0; x++, y--) {
                    line.addSquare(this.squares[x][y]);
                }
                this.winner = line.winsFor();
            }

            return line;
        }

};
