; -*- Mode: emacs-lisp; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- 
;;; gjs-mode.el --- A gjs mode for working with gjs and gtk+ templates.
;;;
;;; Copyright (C) 2013 Donald Lindsay
;;;     
;;; Author:           Donald Lindsay <don.lindsay@gmail.com>
;;; Maintainer:       Donald Lindsay <don.lindsay@gmail.com>
;;; Created:          21 September 2013
;;; Version:          0.0.1
;;; Package-Requires: (js2-mode, js-comint, gjs)
;;; Keywords:         javascript, inferior-mode, gtk+, gjs
;;
;; This file is *not* part of GNU Emacs
;;
;; gjs-mode is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; gjs-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details. (type 'C-h C-c')
;;
;; Description: A mode for working with gjs, a javascript shell with
;; gtk+ bindings.
;;
;; Compatibility: Targeted for Emacs 24
;;
;; Dependencies:  js2-mode js-comint gjs
;; 
;; Code:

;(require 'js2-mode) ;uncomment this if you aren't using (m)elpa
(require 'cl)
(require 'js-comint)

(setq gjs-inferior-js-program "/usr/bin/gjs") 

(define-derived-mode gjs-mode 
  js-comint "*gjs-repl*"
  "A mode for the gjs javascript shell\\{js-comint-map}"
  (set (make-local-variable 'inferior-js-program-command)
       gjs-inferior-js-program))

;; gjs-repl

; This section for setting up, creating the gjs-repl window, and
; starting the gjs shell. These tasks are currently being performed by
; js-comint.

; make-comint-in-buffer 
; gjs-inferior-js-program
; get-buffer
; get-buffer-create
; copy-to-buffer
; process-adaptive-read-buffering
; process-kill-buffer-query-function

;; gjs-app-template

; The gjs-app-template will be combined with a javascript source file
; to produce the gjs-app-script.

(defstruct app-template
  template-name
  imports
  app-name
  app-title
  headerbar
  tabs
  grid
  popover
  style
  effect
  image
  label
  webview)

(defvar *app-templates* nil)

(defun add-template (template) (push template *app-templates*))

(add-template (make-app-template))

; The basic templates are: native-gtk, webkit-gtk, library,
; simple-webapp, cinnamon, unity and mate. The last three are mainly
; for including/excluding appropriate imports. The options determine
; which code blocks will be included, and if so, what they will
; contain.

;<bpalmer> xk05: needs more mario
;<xk05> flaming barrels or hammers?
;<ijp> mushrooms, turtles, and green designers
;<bpalmer> your defstruct seems like its field accessors will be insanely long.
;<bpalmer> (defstruct gjs-app-template  gjs-app-template-name...  )  <-- do you really want to type (gjs-app-template-gjs-app-template-name o) ?
;<bpalmer> I guess you're not really using accessors, though. hmm.
;<bpalmer> I'd still probably suggest template-name instead of gjs-app-template-name, etc.
;<xk05> accessors, template-name
;<kprav33n> Hello! Does anyone use the `magit-interactive-resolve-item'?
;<bpalmer> xk05: (defstruct foo a b)  makes functions so that you can do (foo-a (make-foo :a 3 :b 6)) => 3
;<kprav33n> I tried to use it to resolve a merge conflict that I got. It opens up ediff and three frames. After addressing the conflict, how do I save and mark the item as resolved?
;<xk05> ok
;<bpalmer> you can also set them with (setf (foo-a o) 5)  to set a
;<xk05> ok

(add-template (make-app-template
	       :template-name     'native-gtk
	       :imports           'default
	       :app-name          'app-name
	       :app-title         'app-title
	       :headerbar         'default
	       :tabs              'default
	       :grid              'default
	       :popover           'default
	       :style             'default
	       :effect            'default
	       :image             'default
	       :label             'default
	       :webview           'false))

(add-template (make-app-template
	       :template-name     'webkit-gtk
	       :imports           'webkit-gtk
	       :app-name          'app-name
	       :app-title         'app-title
	       :headerbar         'default
	       :tabs              'default
	       :grid              'default
	       :popover           'default
	       :style             'default
	       :effect            'default
	       :image             'default
	       :label             'default
	       :webview           'true))

(add-template (make-app-template
	       :template-name     'library
	       :imports           'library
	       :app-name          'app-name
	       :app-title         'false
	       :headerbar         'false
	       :tabs              'false
	       :grid              'false
	       :popover           'false
	       :style             'false
	       :effect            'false
	       :image             'false
	       :label             'false
	       :webview           'false))

(add-template (make-app-template
	       :template-name     'simple-webapp
	       :imports           'default
	       :app-name          'app-name
	       :app-title         'app-title
	       :headerbar         'true
	       :tabs              'false
	       :grid              'false
	       :popover           'false
	       :style             'default
	       :effect            'default
	       :image             'default
	       :label             'default
           :webview           'true))

(add-template (make-app-template
	       :template-name     'cinnamon
	       :imports           'cinnamon
	       :app-name          'app-name
	       :app-title         'app-title
	       :headerbar         'true
	       :tabs              'default
	       :grid              'true
	       :popover           'default
	       :style             'default
	       :effect            'default
	       :image             'default
	       :label             'default
           :webview           'default))

(add-template (make-app-template
	       :template-name     'mate
	       :imports           'mate
	       :app-name          'app-name
	       :app-title         'app-title
	       :headerbar         'false
	       :tabs              'default
	       :grid              'true
	       :popover           'default
	       :style             'default
	       :effect            'default
	       :image             'default
	       :label             'default
           :webview           'default))

(add-template (make-app-template
	       :template-name     'unity
	       :imports           'unity
	       :app-name          'app-name
	       :app-title         'app-title
	       :headerbar         'true
	       :tabs              'default
	       :grid              'true
	       :popover           'default
	       :style             'default
	       :effect            'default
	       :image             'default
	       :label             'default
           :webview           'default))

;;; Hammer Time

;; gjs-template-engine

; The gjs-template-engine takes the template and list of options and
; combines them with the selected template to create a gjs-app-script.
; The selection of the template occurs when the user runs 
; 'M-x create-gjs-app-script RET'

(defun create-gjs-app-script ()
  "Presents user with a list of template choices, then runs
  gjs-template-engine with the template-selection as it's
  argument."
  (interactive 
   (pop-buffer (template-choices) ;; the recursive minibuffer later
			   (ask-user-for-template ()
									  template-selection)
			   (kill-buffer (with-current-buffer)))
   (gjs-template-engine (template-selection nil))))

(defun gjs-template-engine (template-selection app-template-buffer) 
  "Populate app-template-buffer with javascript code blocks
  according to the struct of template-selection"
  (evaluate-js-codeblocks-file ("./js-codeblocks.el"))
  (append-to-buffer (app-template-buffer)
					(do-list 
					 (for-each slot (template-selection)
							    (print (slot-default)
				               if :gtk-native
                               if :

; make-indirect-buffer
; clone-indirect-buffer-other-window
; make-variable-buffer-local

(defun select-app-template (*app-templates* gjs-minibuffer-select)
  "Select the template from the list of structs in *gjs-app-templates*."
  (interactive gjs-minibuffer-select) template-selection))

(defun create-app-template-buffer ()
  custom-buffer-create app-template-buffer)



;(defun function-name (arguments...)
;       "optional-documentation..."
;       (interactive argument-passing-info)     ; optional
;       body...)

; buffer-string
; buffer-substring
; buffer-substring-filters
; buffer-substring-no-properties

; filter-buffer-substring
; filter-buffer-substring-functions

; ediff-merge-buffers

; emerge-buffers
; emerge-buffers-with-ancestor
; view-buffer-other-window
; highlight-compare-buffers

; gjsImports
; gjsApplicationName
; gjsApplicationTitle
; gjsHeaderBar
; gjsTabs
; gjsGrid
; gjsPopover
; gjsStyle
; gjsEffect
; gjsImage
; gjsLabel
; gjsWebview

;; gjs-app-script
; The gjs-app-script is javascript produced by gjs-template-engine. It
; opens in it's own buffer and the user can save, load, run or edit.

; generate-new-buffer
; generate-new-buffer-name
; bury-buffer
; switch-to-buffer
; with-current-buffer
; pp-buffer
; set-buffer-major-mode
; checkdoc-current-buffer
; executable-make-buffer-file-executable-if-script-p
; revert-buffer
; switch-to-buffer-other-window
; buffer-offer-save

;; window scheme

; A window scheme is necessary and customizable. Although there are
; limitless ways of arranging windows, there are basically 2 main
; types of window to support, each with it's preferred species of
; buffer:
;      1. top    :  source files | 'merge' buffer  |  gjs-app-script
;      2. bottom :  gjs-repl     | pop-up messages |  doc, etc
; During the 'merge' operation, one of the windows can show an 'merge'
; buffer that can be edited before the 'final' gjs-app-script buffer
; is displayed in the top window. Ergo, the user can:
;      1. Just work with a source file and a repl. 
;      2. Generate a default gjs-app-script and use it with a repl. 
;      3. 'Merge' a source file with a template, edit the 'merge', and use
;         the product gjs-app-script with a repl.

; ido-display-buffer
; ido-insert-buffer
; ido-read-buffer
; ido-switch-buffer
; ido-switch-buffer-other-window
; pop-to-buffer-same-window
; display-message-or-buffer
; set-buffer
; set-window-buffer
; window-buffer
; show-buffer

; display-buffer
; display-buffer-fallback-action
; display-buffer-function
; display-buffer-pop-up-window
; display-buffer-same-window
; pop-to-buffer

;; minibuffer

; A recursive minibuffer can be used to make selections of templates,
; options, etc., thus reducing some of the window handling overhead.

; window-minibuffer-p
; enable-recursive-minibuffers
; eval-minibuffer
; file-cache-minibuffer-complete
; exit-minibuffer

(provide 'gjs-mode)
