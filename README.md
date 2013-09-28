gjs-mode
========

I like the idea of using Gnome's Gjs javascript shell for
coding. js2-mode provides a solid foundation for development using gjs
with emacs. My main concept is to learn from the better ideas of
js-comint and add things like app templates and maybe even some of the
Looking Glass stuff.

Note: gjs-mode is in no way ready for end users, yet

Note: new preliminary code does not require js-comint.el

Suggested
---------

js2-mode:   https://github.com/mooz/js2-mode?source=c
            
Note: js2-mode is installable from GNU Elpa M-x list-packages RET

And, of course, Gjs. Gjs exists in the Debian and Ubuntu repositories
as 'gjs' and can be installed with: 'sudo apt-get install gjs' Arch
users can find Gjs in the 'Extra' repository as 'gjs'. If this doesn't
cover it for you, check your distribution's repositories, or perhaps
you can build Gjs from source.

Gjs Documentation and Tutorials
-------------------------------

Gjs documentation can be found here: https://wiki.gnome.org/Gjs

There are some Gjs tutorial materials:
https://developer.gnome.org/gnome-devel-demos/unstable/hellognome.js.html.en

Some of these materials can be found in the /gnome-tutorial directory
of this project.

Thanks
------

I'm grateful for the excellent work done by the authors and
maintainers of js2-mode and js-comint, the Gnome development
community, and of course, the Emacs development community 
without whom this project would not exist.

This is my first try at an Emacs mode. 

Anyone interested in contributing ideas or code is welcome. Github
facilitates this, or I can sometimes be found as 'xk05' on
irc.freenode.net in the #emacs channel.
