;;; sly-named-readtables.el --- Support named readtables in Common Lisp files  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/capitaomorte/sly-named-readtables
;; Keywords: languages, lisp, sly
;; Package-Requires: ((sly "1.0.0-beta2"))
;; Author: João Távora <joaotavora@gmail.com>
;; 
;; Copyright (C) 2015 João Távora
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;; 
;; An external contrib for SLY that enables different readtables to be
;; active in different parts of the same file. 
;;
;; SLY lives at https://github.com/capitaomorte/sly.
;; 
;;; Installation:
;;
;; Since this is an external contrib with both Elisp and Lisp parts,
;; merely loading the Elisp will have little effect. The contrib has
;; to be registered in SLY's `sly-contribs' variable for SLY to take care
;; of loading the Lisp side on demand.
;;
;; For convenience, the `sly-named-readtables-autoloads.el' Elisp file
;; takes care of this automatically. So in your `~/.emacs' or
;; `~/.emacs.d/init/el' init file:
;; 
;; (setq inferior-lisp-program "/path/to/your/preferred/lisp")
;; (add-to-list 'load-path "/path/to/sly")
;; (require 'sly-autoloads)
;;  
;; (add-to-list 'load-path "/path/to/sly-named-readtables")
;; (require 'sly-named-readtables-autoloads)
;;
;; In case you already have SLY loaded and/or running, you might have to
;; `M-x sly-setup' and `M-x sly-enable-contrib' to enable it.
;;  
;; `sly-named-readtables' should now kick in in Lisp buffers. You must
;; have `named-readtables` setup in your Lisp before it takes any actual
;; effect though. That's easy, just `(ql:quickload :named-readtables)'.
;; 
;;; Code:

(require 'sly)

(define-sly-contrib sly-named-readtables
  "Automatically parse in-readtable forms in Lisp buffers"
  (:slynk-dependencies slynk-named-readtables)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-named-readtables-mode))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-named-readtables-mode)))

(defun sly-named-readtable--pretty-name (name)
  ;; Let's leave this abstraction in place for now...
  name)

(define-minor-mode sly-named-readtables-mode
  "Use EDITOR-HINTS.NAMED-READTABLES if available."
  nil nil nil
  (cond (sly-named-readtables-mode
         (add-to-list 'sly-extra-mode-line-constructs
                      'sly-named-readtables--mode-line-construct
                      t)
         (add-to-list 'sly-rex-extra-options-functions
                      'sly-named-readtables--pass-readtable
                      t))
        (t
         (setq sly-extra-mode-line-constructs
               (delq 'sly-named-readtables--mode-line-construct
                     sly-extra-mode-line-constructs)
               sly-rex-extra-options-functions
               (delq 'sly-named-readtables--pass-readtable
                     sly-rex-extra-options-functions)))))

(defun sly-named-readtables--grok-current-table ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(named-readtables:\\)?in-readtable\\>[ \t\n]*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (re-search-backward regexp nil t)
        (match-string-no-properties 2)))))

(defun sly-named-readtables--mode-line-construct ()
  (let ((readtable-name (sly-named-readtables--grok-current-table)))
    `(:propertize ,(or (and readtable-name
                            (sly-named-readtable--pretty-name readtable-name))
                       "*")
                  face ,(if readtable-name 'hi-pink 'sly-mode-line)
                  mouse-face mode-line-highlight
                  help-echo ,(if readtable-name
                                 (format "Special NAMED-READTABLE %s" readtable-name)
                               "Default readtable"))))

(defun sly-named-readtables--pass-readtable ()
  (list :named-readtable (sly-named-readtables--grok-current-table)))

;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-named-readtables 'append))

(provide 'sly-named-readtables)
;;; sly-named-readtables.el ends here

