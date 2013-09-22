;;; gjs-mode.el ---

;;; Copyright (C) 2013 Donald Lindsay
     
;;; Author:           Donald Lindsay <don.lindsay@gmail.com>
;;; Maintainer:       Donald Lindsay <don.lindsay@gmail.com>
;;; Created:          21 September 2013
;;; Version:          0.0.1
;;; Package-Requires: (js2-mode, js-comint, gjs)
;;; Keywords:         javascript, inferior-mode, gtk+, gjs

;; This file is *not* part of GNU Emacs

;; gjs-mode.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; gjs-mode.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:
;; Free Software Foundation, 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301, USA

;; Description:
;; A mode for working with gjs, a javascript shell with gtk+ bindings.

;; Compatibility:

;;; Code:

;; js2-mode gjs-mode requires js2-mode as a dependency Here we require
;; js2-mode and load the keybindings. If you have js2-mode via melpa,
;; then the next line must be commented out.
;(require 'js2-mode)
;; add this to your .emacs
;(add-hook 'js2-mode-hook 
;	  '(lambda () 
;	     (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;	     (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;	     (local-set-key "\C-cb" 'js-send-buffer)
;	     (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;	     (local-set-key "\C-cl" 'js-load-file-and-go)
;	     ))

;; js-comint
;; gjs-mode requires js-comint as a dependency
;; here we require js-comint and set the
;; inferior-js-program-command to gjs
(require 'js-comint)


; bpalmer: "so, your mode should define-derived-mode, and probably use
; a separate variable, and maybe do a (set (make-local-variable 'foo)
; gjs-inferior-js-program) so that it's a self-contained thing" 

; so not this, (setq inferior-js-program-command "gjs"), but this:
(setq gjs-inferior-js-program "/usr/bin/gjs") 
(define-derived-mode gjs-mode 
  js-comint "GJS"
  "Major mode for gjs javascript shell\\{js-comint-map}"
  (set (make-local-variable 'inferior-js-program-command)
       gjs-inferior-js-program))


(provide 'gjs-mode)
