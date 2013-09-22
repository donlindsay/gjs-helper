gjs-mode.el
===========

I like the idea of using Gnome's GJS javascript shell for
coding. js2-mode and js-comint provide a solid foundation for
developing a mode for using gjs with emacs.

Currently, gjs-mode.el depends on both js2-mode and js-comint which
can be found at:

js2-mode:   https://github.com/mooz/js2-mode?source=c
            note: js2-mode exists in the melpa emacs package repository 
            see: http://melpa.milkbox.net/#/

js-comint:  http://sourceforge.net/projects/js-comint-el/?source=directory

GJS documentation can be found here:
https://wiki.gnome.org/Gjs

There is a GJS tutorial which I will include in the /hellognome dir:
https://developer.gnome.org/gnome-devel-demos/unstable/hellognome.js.html.en

GJS exists in the Debian and Ubuntu repositories as 'gjs' and can be
installed with: 'sudo apt-get install gjs'

Arch users can find GJS in the 'Extra' repository as 'gjs'.

My main concept so far is to lift the better ideas out of js-comint.el
and add features like app templates and maybe even some of the Looking
Glass stuff.

This is my first try at an Emacs mode. Anyone interested in contributing ideas 
or code is welcome. Github facilitates this, or I can sometimes be found
as 'xk05' on irc.freenode.net in the #emacs channel. 
