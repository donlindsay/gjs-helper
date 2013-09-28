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

(setq js-blocks-file "./js-codeblocks.el")

(setq js-blocks-index nil)

(setq app-skel-names '(gtk webkit library cinn unity))

;;; REPL
;; Setting up, creating the gjs-repl window, and starting the gjs
;; shell. These tasks are currently being performed by js-comint.

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

;;; gjs-template-engine
;; The gjs-template-engine takes the template and list of options and
;; combines them with the selected template to create a gjs-app-script.
(defun run-template-motor (app-skel-selection) 
  "Populate app-template-buffer with javascript code blocks
  according to the scheme of app-skel-selection"
  (evaluate-js-blocks-file)
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

(defun create-js-blocks-index (js-blocks-file)
  "Read the list of js-blocks into a nested array for
   matching against app-skel-selection slots."
  (load-file ()
			 (do-once 
			  (push js-block-js-block-name 
					(js-block-app-skel-name (js-block-app-skel-slot))
					(js-blocks-index)))))

;;; Request information from user
;; Popup window, or minibuffer.
(defun request-template-name ()
  "Request the name of the template from user."
  (interactive 
   (pop-buffer (template-choices) ;; the recursive minibuffer later
			   (ask-user-for-template ()
									  template-selection)
			   (kill-buffer (with-current-buffer)))
   (run-template-motor (template-selection nil))))

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

;comint-dynamic-complete-functions        ;	List of functions called to perform completion.
;comint-input-filter-functions 	          ; Abnormal hook run before input is sent to the process.
;comint-output-filter-functions 	      ; Functions to call after output is inserted into the buffer.
;comint-preoutput-filter-functions 	      ; List of functions to call before inserting Comint output into the buffer.
;comint-redirect-filter-functions 	      ; List of functions to call before inserting redirected process output.
;comint-redirect-original-filter-function ; The process filter that was in place when redirection is started
;comint-input-sender ; lets you alter the input string mid-stream.

;; http://www.masteringemacs.org/articles/2013/07/31/comint-writing-command-interpreter/

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
