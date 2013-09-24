;; -*- emacs-lisp -*-
;;; package_boiler_plate
;;; gpl2_boiler_plate 
;;
;;; Description: js2-codeblocks is a library of basic gjs javascript
;;;              source file elements common to gjs scripts. The code
;;;              blocks are given a few basic meta properties for
;;;              reading by emacs and copying to the gjs-app-buffer.
;;;
;;; Discussion: Optionally, this file can contain code that pushes
;;;              each code-block into a list of structs, but I don't
;;;              know that this will be helpful as the stack won't
;;;              really be used. idk(y)

;;; Hammer Time:

(defstruct code-block 
  code-block-name 
  code-block-template-slot
  template-slot-option
  js-code-block
  code-block-meta-meta)

(make-code-block
 :code-block-name              'common-header
 :code-block-template-slot     'all
 :template-slot-option         'default
 :js-code-block                 
 '("// -*- js2 -*- gjs-app-script
    #!/usr/bin/gjs
                              ")
 :code-block-meta-meta          'nil)

(make-code-block
 :code-block-name              'common-footer
 :code-block-template-slot     'all
 :template-slot-option         'default
 :js-code-block                
 '("// end gjs-app-script")
 :code-block-meta-meta         'nil)
