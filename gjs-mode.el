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
;; Pause For The Cause: (defun function-name (arguments...)
;;                       "optional-documentation..."
;;                       (interactive argument-passing-info) ; optional
;;                        body...)
;; Hammer Time:

;(require 'js2-mode)
;(require 'js-comint)

;;; The gjs-repl-mode is based on js-comint repl.

(define-derived-mode gjs-mode 
  js-comint "*gjs-repl*"
  "A mode for the gjs javascript shell\\{js-comint-map}"
  (set (make-local-variable 'inferior-js-program-command)
       gjs-inferior-js-program))

(setq gjs-inferior-js-program "/usr/bin/gjs") 

(setq gjs-js-codeblocks-file "./js-codeblocks.el")

(setq js-codeblocks-index nil)

;;; gjs-template-engine
;; The gjs-template-engine takes the template and list of options and
;; combines them with the selected template to create a gjs-app-script.

(defun run-template-motor (app-skel-selection) 
  "Populate app-template-buffer with javascript code blocks
  according to the scheme of app-skel-selection"
  (evaluate-js-codeblocks-file)
  (create-app-template-buffer)
  (switch-to-buffer (app-template-buffer))
  (iterate-over (app-skel-selection) 
				(for (each-slot) (app-skel-selection)
					 (with-a-match-from 
					  (js-codeblocks-file)
					  (append-to-buffer (app-template-buffer) 
										(matching-codeblock))
					  ))))

(defun create-app-template-buffer ()
  "Create a new buffer to write the codeblocks to."
  custom-buffer-create (app-template-buffer))

(defun create-js-codeblocks-index (gjs-js-codeblocks-file)
  "Read the list of js-codeblocks into a nested array for
   matching against app-skel-selection slots."
  (load-file ()
			 (do-once 
			  (push js-codeblock-ident 
					(app-skel-name (app-skel-slot))
					(js-codeblocks-index)))))

;;; Request information from user
;; Popup window, or minibuffer.
(defun request-template-name ()
  "Request the name of the template from user."
  (interactive 
   (pop-buffer (template-choices) ;; the recursive minibuffer later
			   (ask-user-for-template ()
									  template-selection)
			   (kill-buffer (with-current-buffer)))
   (gjs-template-engine (template-selection nil))))

;;; app-skel
;; The struct, app-skel, used to set options for the template
;; generator.  The basic templates are: 
;; gtk, webkit, library, cinn(amon), and unity 

(defstruct app-skel
  (name) (imports) (headerbar) (popover) (grid) (webkit)
  (tabs) (label) (image) (style))
  
(defun create-gtk-skel (app-skel)
  "Fill an app-skel struct with gtk app values."
  (setf (app-skel-name      'gtk)
		(app-skel-imports   'gtk)
		(app-skel-headerbar (headerbar-p))
		(app-skel-popover   (popover-p))
		(app-skel-grid      (grid-p))
		(app-skel-webkit    (webkit-p))
		(app-skel-tabs      (tabs-p))
		(app-skel-label     'true)
		(app-skel-image     'true) 
		(app-skel-style     'true)
		))

(defun create-webkit-skel (app-skel)
  "Fill an app-skel struct with webkit app values."
  (setf (app-skel-name      'webkit)
		(app-skel-imports   ('gtk 'webkit))
		(app-skel-headerbar 'true)
		(app-skel-popover   'false)
		(app-skel-grid      'true)
		(app-skel-webkit    'true)	
		(app-skel-tabs      (tabs-p))
		(app-skel-label     'true)
		(app-skel-image     'true)
		(app-skel-style     'true)
		))
  
(defun create-library-skel (app-skel)
  "Fill an app-skel struct with library values."
  (setf (app-skel-name      'library)
		(app-skel-imports   'gtk)
		(app-skel-headerbar 'false)
		(app-skel-popover   'false)
		(app-skel-grid      'false)
		(app-skel-webkit    'false)					  
		(app-skel-tabs      'false)
		(app-skel-label     'false)
		(app-skel-image     'false)				  
		(app-skel-style     'false)
		))
  
(defun create-cinn-skel (app-skel)
  "Fill an app-skel struct with cinn values."
  (setf (app-skel-name      'cinn)
		(app-skel-imports   ('gtk 'cinn))
		(app-skel-headerbar 'true)
		(app-skel-popover   'false)
		(app-skel-grid      'true)
		(app-skel-webkit    'false)					  
		(app-skel-tabs      (tabs-p))
		(app-skel-label     'true)
		(app-skel-image     'true)				  
		(app-skel-style     'true)
		))
  
(defun create-unity-skel (app-skel)
  "Fill an app-skel struct with unity values."
  (setf (app-skel-name      'unity)
		(app-skel-imports   ('gtk 'unity))
		(app-skel-headerbar 'true)
		(app-skel-popover   (popover-p))
		(app-skel-grid      'true)
		(app-skel-webkit    'false)					  
		(app-skel-tabs      (tabs-p))
		(app-skel-label     'true)
		(app-skel-image     'true)				  
		(app-skel-style     'true)
		))

(provide 'gjs-mode)

;;; Errata & TODO

;; <bpalmer> xk05: needs more mario
;;           (defstruct foo a b)
;;           (foo-a (make-foo :a 3 :b 6)) => 3
;;           (setf (foo-a o) 5)
;; (defvar mario '(flaming-barrels hammers mushrooms turtles green-designers))

;;; REPL
;; Setting up, creating the gjs-repl window, and starting the gjs
;; shell. These tasks are currently being performed by js-comint.

; make-comint-in-buffer 
; gjs-inferior-js-program
; get-buffer
; get-buffer-create
; copy-to-buffer
; process-adaptive-read-buffering
; process-kill-buffer-query-function

;;; Minibuffer
;; A recursive minibuffer can be used to make selections of templates,
;; options, etc., thus reducing some of the window handling overhead.

;(defun select-app-skel (gjs-minibuffer-select)
;  "Select the app-skel."
;  (interactive minibuffer-select) app-skel-selection))

; window-minibuffer-p
; enable-recursive-minibuffers
; eval-minibuffer
; file-cache-minibuffer-complete
; exit-minibuffer

;;; Window scheme
;; A window scheme is necessary and customizable. Although there are
;; limitless ways of arranging windows, there are basically 2 main
;; types of window to support, each with it's preferred species of
;; buffer:
;;      1. top    :  source files | 'merge' buffer  |  gjs-app-script
;;      2. bottom :  gjs-repl     | pop-up messages |  doc, etc
;; During the 'merge' operation, one of the windows can show an 'merge'
;; buffer that can be edited before the 'final' gjs-app-script buffer
;; is displayed in the top window. Ergo, the user can:
;;      1. Just work with a source file and a repl. 
;;      2. Generate a default gjs-app-script and use it with a repl. 
;;      3. 'Merge' a source file with a template, edit the 'merge', and use
;;         the product gjs-app-script with a repl.


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

; make-indirect-buffer
; clone-indirect-buffer-other-window
; make-variable-buffer-local
; buffer-string
; buffer-substring
; buffer-substring-filters
; buffer-substring-no-properties
; filter-buffer-substring
; filter-buffer-substring-functions
; view-buffer-other-window
; highlight-compare-buffers

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

;; Emacs keeps complaining about multiple constructors, I dunno why,
;; but ok. The LT sez dog one is closed. Time to setf lam-mode and
;; find another way off the friggin beach. swyaoywu.
;;
;; SWYAOYWU: Someday, When You Are Older, You Will Understand 
;;
;; (defstruct
;;                 (person
;;                  (:constructor nil)   ; no default constructor
;;                  (:constructor new-person (name sex &optional (age 0)))
;;                  (:constructor new-hound (&key (name "Rover")
;;                                                (dog-years 0)
;;                                           &aux (age (* 7 dog-years))
;;                                                (sex 'canine))))
;;                 name age sex)
