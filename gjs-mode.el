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
;; gtk+ bindings, and an application template system for creating gjs
;; applications. Intended to compliment js2-mode.
;;
;; Compatibility: Targeted for Emacs 24
;;
;; Dependencies:  gjs
;; 
;; Pause For The Cause: (defun function-name (arguments...)
;;                       "optional-documentation..."
;;                       (interactive argument-passing-info) ; optional
;;                        body...)
;; Hammer Time:

;;; REPL
;; Setting up, creating the gjs-repl window, and starting the gjs
;; shell.

(require 'comint)

(defvar gjs-file-path "/usr/bin/gjs"
  "Path to the program used by `run-gjs'")
 
(defvar gjs-arguments '()
  "Commandline arguments to pass to `gjs'")
 
(defvar gjs-mode-map
   (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
     ;; example definition
     (define-key map "\t" 'completion-at-point)
     map)
   "Basic mode map for `run-gjs'")
 
(defvar gjs-prompt-regexp "^gjs>$"
   "Prompt for `run-gjs'.")

(defun run-gjs ()
  "Run an inferior instance of `gjs' inside Emacs."
  (interactive)
  (let* ((gjs-program gjs-file-path)
         (buffer (comint-check-proc "gjs")))
    ;; pop to the "*gjs*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'gjs-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*gjs*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "gjs" buffer
             gjs-program gjs-arguments)
      (gjs-mode))))

(defun gjs--initialize ()
  "Helper function to initialize gjs"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))
 
(define-derived-mode gjs-mode comint-mode "gjs"
  "Major mode for `run-gjs'.
 
\\<gjs-mode-map>"
  nil "gjs"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp gjs-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
;  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(gjs-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) gjs-prompt-regexp))
 
(add-hook 'gjs-mode-hook 'gjs--initialize)

(set (make-local-variable 'font-lock-defaults) '(gjs-font-lock-keywords t))

(defconst gjs-keywords
  '())
 
(defvar gjs-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt gjs-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `gjs-mode'.")

;;; Template Engine
;; The template engine takes an app-skel-selection and a list of js
;; code blocks and selectively combines them to create a
;; gjs-app-script.

;; request info by window or, later, a minibuffer.
(setq app-skel-names '(gtk webkit library cinn unity))

(defun request-skel-name ()
  "Request the name of the app-skel from user."
  (interactive 
   (pop-buffer (template-choices)
			   (ask-user-for-template ()
									  app-skel-selection)
			   (kill-buffer (with-current-buffer)))
   (run-template-motor (app-skel-selection nil))))

(setq js-blocks-file "./js-codeblocks.el")

(defun run-template-motor (app-skel-selection) 
  "Populate app-template-buffer with javascript code blocks
  according to the scheme of app-skel-selection"
  (create-js-blocks-index)  ; consider doing this independently if its slow
  (create-app-template-buffer)
  (switch-to-buffer (app-template-buffer))
  (iterate-over (app-skel-selection) 
				(for (each-slot) (app-skel-selection)
					 (with-a-match-from 
					  (js-blocks-index)
					  (append-to-buffer (app-template-buffer) 
										(matching (js-block-js-block))
					  )))))

(defun create-app-template-buffer ()
  "Create a new buffer to write the js-blocks to."
  custom-buffer-create (app-template-buffer))

(setq js-blocks-index nil)

(defun create-js-blocks-index (js-blocks-file)
  "Read the list of js-blocks into a nested array for
   matching against app-skel-selection slots."
  (load-file ()
			 (do-once 
			  (push js-block-js-block-name 
					(js-block-app-skel-name (js-block-app-skel-slot))
					(js-blocks-index)))))

;;; Skeleton Closet
;; The struct, app-skel, used to set options for run-template-motor
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

;;; Minibuffer
;; A recursive minibuffer can be used to make selections of templates,
;; options, etc., thus reducing some of the window handling overhead.

;(defun select-app-skel (gjs-minibuffer-select)
;  "Select the app-skel."
;  (interactive minibuffer-select) app-skel-selection))

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



 
