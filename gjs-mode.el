; -*- Mode: emacs-lisp; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- 
;;; gjs-mode.el ---

;;; Copyright (C) 2013 Donald Lindsay
     
;;; Author:           Donald Lindsay <don.lindsay@gmail.com>
;;; Maintainer:       Donald Lindsay <don.lindsay@gmail.com>
;;; Created:          21 September 2013
;;; Version:          0.0.1
;;; Package-Requires: (js2-mode, js-comint, gjs)
;;; Keywords:         javascript, inferior-mode, gtk+, gjs

;; This file is *not* part of GNU Emacs

;; gjs-mode is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; gjs-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:
;; Free Software Foundation, 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301, USA
;;
;; Description:
;; A mode for working with gjs, a javascript shell with gtk+ bindings.
;;
;; Compatibility:
;; Targeted for Emacs 24
;;
;; Dependencies: js2-mode js-comint gjs

;(require 'js2-mode) ;uncomment this if you aren't using (m)elpa
(require 'cl)
(require 'js-comint)

; bpalmer: "so, your mode should define-derived-mode, and probably use
; a separate variable, and maybe do a (set (make-local-variable 'foo)
; gjs-inferior-js-program) so that it's a self-contained thing" 

; so not this, 
;(setq inferior-js-program-command "gjs")
; but this:
(setq gjs-inferior-js-program "/usr/bin/gjs") 
(define-derived-mode gjs-mode 
  js-comint "GJS"
  "A mode for the gjs javascript shell\\{js-comint-map}"
  (set (make-local-variable 'inferior-js-program-command)
       gjs-inferior-js-program))

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

; These are the basic templates and their options. The options
; determine which code blocks will be included, and if so, what they
; will contain. The basic templates are: native-gtk, webkit-gtk,
; library, simple-webapp, cinnamon, unity and mate. The last three
; templates are mainly so that appropriate imports will be included in
; the gjs-app-script.

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

;; gjs-template-engine
; The gjs-template-engine takes the list of template variables above
; and combines them with the selected template to create a
; gjs-app-script.

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
; The gjs-app-script is the runnable script produced by
; gjs-template-engine. It opens in it's own buffer and the user can
; save, load, run or edit.



(provide 'gjs-mode)
