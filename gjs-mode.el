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
;; {at your option} any later version.

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
;; A major mode for working with gjs, a javascript shell with gtk+
;; bindings.

;; Compatibility:

;;; Code:
(require 'js2-mode)
(require 'js-comint)
