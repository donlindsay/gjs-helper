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

(defstruct gjs-app-template
  gjs-app-template-name
  gjs-imports
  gjs-app-name
  gjs-app-title
  gjs-headerbar
  gjs-tabs
  gjs-grid
  gjs-popover
  gjs-style
  gjs-effect
  gjs-image
  gjs-label
  gjs-webview)

(defvar *gjs-app-templates* nil)

(defun add-template (template) (push template *gjs-app-templates*))

(add-template (make-gjs-app-template))

; The basic templates are: native-gtk, webkit-gtk, library,
; simple-webapp, cinnamon, unity and mate. The last three are mainly
; for including/excluding appropriate imports. The options determine
; which code blocks will be included, and if so, what they will
; contain.

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'native-gtk
	       :gjs-imports           'default
	       :gjs-app-name          'app-name
	       :gjs-app-title         'app-title
	       :gjs-headerbar         'default
	       :gjs-tabs              'default
	       :gjs-grid              'default
	       :gjs-popover           'default
	       :gjs-style             'default
	       :gjs-effect            'default
	       :gjs-image             'default
	       :gjs-label             'default
	       :gjs-webview           'false))

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'webkit-gtk
	       :gjs-imports           'webkit-gtk
	       :gjs-app-name          'app-name
	       :gjs-app-title         'app-title
	       :gjs-headerbar         'default
	       :gjs-tabs              'default
	       :gjs-grid              'default
	       :gjs-popover           'default
	       :gjs-style             'default
	       :gjs-effect            'default
	       :gjs-image             'default
	       :gjs-label             'default
	       :gjs-webview           'true))

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'library
	       :gjs-imports           'library
	       :gjs-app-name          'app-name
	       :gjs-app-title         'false
	       :gjs-headerbar         'false
	       :gjs-tabs              'false
	       :gjs-grid              'false
	       :gjs-popover           'false
	       :gjs-style             'false
	       :gjs-effect            'false
	       :gjs-image             'false
	       :gjs-label             'false
	       :gjs-webview           'false))

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'simple-webapp
	       :gjs-imports           'default
	       :gjs-app-name          'app-name
	       :gjs-app-title         'app-title
	       :gjs-headerbar         'true
	       :gjs-tabs              'false
	       :gjs-grid              'false
	       :gjs-popover           'false
	       :gjs-style             'default
	       :gjs-effect            'default
	       :gjs-image             'default
	       :gjs-label             'default
           :gjs-webview           'true))

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'cinnamon
	       :gjs-imports           'cinnamon
	       :gjs-app-name          'app-name
	       :gjs-app-title         'app-title
	       :gjs-headerbar         'true
	       :gjs-tabs              'default
	       :gjs-grid              'true
	       :gjs-popover           'default
	       :gjs-style             'default
	       :gjs-effect            'default
	       :gjs-image             'default
	       :gjs-label             'default
           :gjs-webview           'default))

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'mate
	       :gjs-imports           'mate
	       :gjs-app-name          'app-name
	       :gjs-app-title         'app-title
	       :gjs-headerbar         'false
	       :gjs-tabs              'default
	       :gjs-grid              'true
	       :gjs-popover           'default
	       :gjs-style             'default
	       :gjs-effect            'default
	       :gjs-image             'default
	       :gjs-label             'default
           :gjs-webview           'default))

(add-template (make-gjs-app-template
	       :gjs-app-template-name 'unity
	       :gjs-imports           'unity
	       :gjs-app-name          'app-name
	       :gjs-app-title         'app-title
	       :gjs-headerbar         'true
	       :gjs-tabs              'default
	       :gjs-grid              'true
	       :gjs-popover           'default
	       :gjs-style             'default
	       :gjs-effect            'default
	       :gjs-image             'default
	       :gjs-label             'default
           :gjs-webview           'default))

;;; Hammer Time

;; gjs-template-engine

; The gjs-template-engine takes the template and list of options and
; combines them with the selected template to create a gjs-app-script.

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

(defun select-app-template (*gjs-app-templates* gjs-minibuffer-select)
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
