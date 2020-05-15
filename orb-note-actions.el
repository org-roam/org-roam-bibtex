;;; orb-note-actions.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex, org-ref
;; Version: 0.2.3

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library offers a way to run actions associated with the note
;; file the current buffer is visiting, such as locate the
;; corresponding bibtex record or open a PDF file associated with it.
;;
;; To use it:
;;
;; call interactively `org-roam-bibtex-note-actions' from a notes buffer
;; containing the #+ROAM_KEY: keyword.
;;
;; Default actions are driven by bibtex-completion but users can install
;; their own actions in `org-roam-bibtex-note-actions'.

;;; Code:
;; * Library requires

(require 'org-roam-bibtex)
(require 'warnings)
(require 'cl-lib)
;; TODO: get rid of after we have our own format function
(require 'org-ref)

(declare-function ido-completing-read "ido")
(declare-function helm "helm")
(declare-function ivy-read "ivy")
(declare-function defhydra "hydra")

;; * Customize definitions

(defcustom orb-note-actions-frontend 'default
  "Interface frontend for `orb-note-actions'."
  :type '(radio
          (const :tag "Default" default)
          (const :tag "Ido" ido)
          (const :tag "Hydra" hydra)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (function :tag "Custom function"))
  :group 'org-roam-bibtex)

(defcustom orb-note-actions-extra
  '(("Save citekey to kill-ring and clipboard" . orb-note-actions-copy-citekey)
    ("Show record in the bibtex file" . bibtex-completion-show-entry)
    ("Scrap pdf file for references" . orb-note-actions-scrap-pdf))
  "Extra actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :type '(alist
          :tag "Extra actions for `orb-note-actions'"
          :key-type (string :tag "Prompt")
          :value-type (symbol :tag "Function name (unquoted)"))
  :group 'org-roam-bibtex)

(defcustom orb-note-actions-user nil
  "User actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :type '(alist
          :tag "User actions for `orb-note-actions'"
          :key-type (string :tag "Prompt")
          :value-type (symbol :tag "Function name (unquoted)"))
  :group 'org-roam-bibtex)


;; * Helper functions

(defvar orb-note-actions-default
  '(("Open PDF file(s)" . bibtex-completion-open-pdf)
    ("Add PDF to library" . bibtex-completion-add-pdf-to-library)
    ("Open URL or DOI in browser" . bibtex-completion-open-url-or-doi))
  "Default actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION.")

(defmacro orb-note-actions--frontend! (frontend &rest body)
  "Return a function definition for FRONTEND.
Function name takes a form of orb-note-actions--FRONTEND.
A simple docstring is constructed and BODY is injected into a
`let' form, which has two variables bound, NAME and
CANDIDATES.  NAME is a string formatted with
`org-ref-format-entry' and CANDIDATES is a cons cell alist
constructed from `orb-note-actions-default',
`orb-note-actions-extra', and `orb-note-actions-user."
  (declare (indent 1) (debug (symbolp &rest form)))
  (let* ((frontend-name (symbol-name (eval frontend)))
         (fun-name (intern (concat "orb-note-actions--" frontend-name))))
    `(defun ,fun-name (citekey)
       ,(format "Provide note actions using %s interface.
CITEKEY is the citekey." (capitalize frontend-name))
       (let ((name (org-ref-format-entry citekey)) ;; TODO: make a native format function
             ;; TODO: this throws an error for an unclear reason
             ;; (name (bibtex-completion-format-entry
             ;;        (bibtex-completion-get-entry citekey)
             ;;        (window-body-width)))
             (candidates
              ,(unless (string= frontend-name "hydra")
                 '(append  orb-note-actions-default
                           orb-note-actions-extra
                           orb-note-actions-user))))
         ,@body))))

(orb-note-actions--frontend! 'default
  (let ((f (cdr (assoc (completing-read name candidates) candidates))))
    (funcall f (list citekey))))

(orb-note-actions--frontend! 'ido
  (let* ((c (cl-map 'list 'car candidates))
         (f (cdr (assoc (ido-completing-read name c) candidates))))
    (funcall f (list citekey))))

(declare-function orb-note-actions-hydra/body "orb-note-actions" nil t)
(orb-note-actions--frontend! 'hydra
  (let ((n ?a)
        actions)
    (dolist (type (list "Default" "Extra" "User"))
      (let ((actions-var (intern (concat "orb-note-actions-" (downcase type)))))
        (dolist (action (symbol-value actions-var))
          (cl-pushnew
           `(,(format "%c" n) (,(cdr action) (list ,citekey)) ,(car action) :column ,(concat type " actions"))
           actions)
          (setq n (1+ n)))))            ; TODO: figure out a way to supply mnemonic keys
    (setq actions (nreverse actions))
    (eval
     `(defhydra orb-note-actions-hydra (:color blue :hint nil)
        ,(format  "^\n  %s \n\n^"  (s-word-wrap (- (window-body-width) 2) name))
        ,@actions)))
  (orb-note-actions-hydra/body))

(orb-note-actions--frontend! 'ivy
  (if (fboundp 'ivy-read)
      (ivy-read name
                candidates
                :require-match t
                :caller #'orb-note-actions
                :action (lambda (c)
                          (funcall (cdr c) (list citekey))))
    (display-warning :warning "You must have Ivy installed to use it! Falling back to default.")
    (orb-note-actions--default citekey)))

(orb-note-actions--frontend! 'helm
  (if (fboundp 'helm)
      (helm :sources
            `(((name . ,name)
               (candidates . ,candidates)
               (action . (lambda (f)
                           (funcall f (list ,citekey)))))))
    (display-warning :warning "You must have Helm installed to use it! Falling back to default.")
    (orb-note-actions--default citekey)))

(defun orb-note-actions--run (frontend citekey )
  "Run note actions on CITEKEY with FRONTEND."
  (let ((fun (intern (concat "orb-note-actions--" (symbol-name frontend)))))
    (funcall fun citekey)))

;; * Note actions

(defun orb-note-actions-copy-citekey (citekey)
  "Save note's citekey to kill-ring and copy it to clipboard.
Since CITEKEY is actually a list of one element, the car of the list is used."
  (with-temp-buffer
    (insert (car citekey))
    (copy-region-as-kill (point-min) (point-max))))

(defun orb-note-actions-scrap-pdf (citekey)
  "Wrapper around `orb-reference-scrapper-insert'."
  (orb-reference-scrapper-insert (car citekey)))


;; * Main functions

;;;###autoload
(defun orb-note-actions ()
  "Run an interactive prompt to offer note-related actions.
The prompt frontend can be set in `orb-note-actions-frontend'.
In addition to default actions, which are not supposed to be
modified, there is a number of prefined extra actions
`orb-note-actions-extra' that can be customized.  Additionally,
user actions can be set in `orb-note-actions-user'."
  (interactive)
  (let ((non-default-frontends (list 'hydra 'ido 'ivy 'helm))
        (citekey (cdr (assoc "ROAM_KEY"
                             (org-roam--extract-global-props
                              '("ROAM_KEY"))))))
    ;; remove format from citekey
    (when orb-citekey-format
      (setq citekey (orb--unformat-citekey citekey)))
    (if citekey
        (cond ((member
                orb-note-actions-frontend
                non-default-frontends)
               (orb-note-actions--run
                orb-note-actions-frontend
                citekey))
              ((functionp
                orb-note-actions-frontend)
               (funcall
                orb-note-actions-frontend
                citekey))
              (t
               (orb-note-actions--run
                'default
                citekey)))
      (message "#+ROAM_KEY is not found in this buffer."))))

(provide 'orb-note-actions)
;;; orb-note-actions.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
