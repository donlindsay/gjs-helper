gjs-mode.el
===========

I like the idea of using Gnome's GJS javascript shell for
coding. js2-mode and js-comint provide a solid foundation for
developing a mode for using gjs with emacs. My main concept so far is
to lift the better ideas out of js-comint.el and add features like app
templates and maybe even some of the Looking Glass stuff.

note: gjs-mode is in no way ready for end users, yet.

Dependencies
------------

js2-mode:   https://github.com/mooz/js2-mode?source=c
            
js-comint:  http://sourceforge.net/projects/js-comint-el/?source=directory

note: js2-mode exists in the melpa emacs developer package repository, see:
http://melpa.milkbox.net/#/

And, of course, GJS. GJS exists in the Debian and Ubuntu repositories
as 'gjs' and can be installed with: 'sudo apt-get install gjs' Arch
users can find GJS in the 'Extra' repository as 'gjs'. If this doesn't
cover it for you, check your distribution's repositories, or perhaps
you can build GJS from source.

GJS Documentation and Tutorials
-------------------------------

GJS documentation can be found here: https://wiki.gnome.org/Gjs

There are some GJS tutorial materials:
https://developer.gnome.org/gnome-devel-demos/unstable/hellognome.js.html.en

Some of these materials can be found in the /gnome-tutorial directory.

Thanks
------

I'm grateful for the excellent work done by the authors and maintainers
of js2-mode, js-comint and of course the Gnome development community
for their provision, and of course, the Emacs development community
without whom this project would not exist.

This is my first try at an Emacs mode. Anyone interested in contributing ideas 
or code is welcome. Github facilitates this, or I can sometimes be found
as 'xk05' on irc.freenode.net in the #emacs channel. 
