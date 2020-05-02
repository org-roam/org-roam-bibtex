;;; org-roam-bibtex-note-actions.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+org@gmail.com>

;; Author: Leo Vivier <leo.vivier+org@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; URL: https://github.com/zaeph/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex, org-ref
;; Version: 0.1

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

(declare-function ido-completing-read "ido" (prompt choices &optional predicate require-match initial-input hist def inherit-input-method))
(declare-function helm "helm")
(declare-function ivy-read "ivy")
(declare-function defhydra "hydra")

;; * Customize definitions

(defcustom org-roam-bibtex-note-actions-frontend 'helm
  "Interface frontend for `org-roam-bibtex-note actions'."
  :type '(radio
          (const :tag "Default" default)
          (const :tag "Ido" ido)
          (const :tag "Hydra" hydra)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (function :tag "Custom function"))
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-note-actions-extra
  '(("Show record in the bibtex file" . bibtex-completion-show-entry))
  "Extra actions for `org-roam-bibtex-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :type '(alist
          :tag "Extra actions for `org-roam-bibtex-note-actions'"
          :key-type (string :tag "Prompt")
          :value-type (symbol :tag "Function name (unquoted)"))
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-note-actions-user nil
  "User actions for `org-roam-bibtex-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :type '(alist
          :tag "User actions for `org-roam-bibtex-note-actions'"
          :key-type (string :tag "Prompt")
          :value-type (symbol :tag "Function name (unquoted)"))
  :group 'org-roam-bibtex)

;; * Helper functions

(defvar org-roam-bibtex-note-actions--default
  '(("Open PDF file(s)" . bibtex-completion-open-pdf)
    ("Add PDF to library" . bibtex-completion-add-pdf-to-library)
    ("Open URL or DOI in browser" . bibtex-completion-open-url-or-doi))
  "Default actions for `org-roam-bibtex-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION.")

(defmacro org-roam-bibtex-note-actions--frontend! (frontend &rest body)
  "Return a function definition for FRONTEND.
Function name takes a form of org-roam-bibtex-note-action--FRONTEND.
A simple docstring is constructed and BODY is injected into a
`let' form, which has two variables bound, NAME and
CANDIDATES.  NAME is a string formatted with
`org-ref-format-entry' and CANDIDATES is a cons cell alist
constructed from `org-roam-bibtex-note-actions--default',
`org-roam-bibtex-note-actions-extra', and `org-roam-bibtex-note-actions-user."
  (declare (indent 1) (debug (symbolp &rest form)))
  (let* ((frontend-name (symbol-name (eval frontend)))
         (fun-name (intern (concat "org-roam-bibtex-note-actions--" frontend-name))))
    `(defun ,fun-name (citekey)
       ,(format "Run note actions in current buffer using %s interface.
CITEKEY is the citekey." (capitalize frontend-name))
       (let ((name (org-ref-format-entry citekey)) ;; TODO: make a native format function
             ;; TODO: this throws an error for andunclear reason
             ;; (name (bibtex-completion-format-entry
             ;;        (bibtex-completion-get-entry citekey)
             ;;        (window-body-width)))
             (candidates
              ,(if (string= frontend-name "hydra")
                    '(append (list :default org-roam-bibtex-note-actions--default)
                             (list :extra org-roam-bibtex-note-actions-extra)
                             (list :user org-roam-bibtex-note-actions-user))
                  '(append  org-roam-bibtex-note-actions--default
                            org-roam-bibtex-note-actions-extra
                            org-roam-bibtex-note-actions-user))))
         ,@body))))

(org-roam-bibtex-note-actions--frontend! 'default
  (let ((f (cdr (assoc (completing-read name candidates) candidates))))
    (funcall f (list citekey))))

(org-roam-bibtex-note-actions--frontend! 'ido
  (let* ((c (cl-map 'list 'car candidates))
         (f (cdr (assoc (ido-completing-read name c) candidates))))
    (funcall f (list citekey))))

(declare-function org-roam-bibtex-note-actions-hydra/body "org-roam-bibtex-note-actions" nil t)
(org-roam-bibtex-note-actions--frontend! 'hydra
  (let ((n ?a)
        actions)
    (dolist (action (plist-get candidates :default))
      (cl-pushnew
       `(,(format "%c" n) (,(cdr action) 'key) ,(car action) :column "Default actions")
       actions)
       (setq n (1+ n)))
    (dolist (action (plist-get candidates :extra))
      (cl-pushnew
       `(,(format "%c" n) (,(cdr action) 'key) ,(car action) :column "Extra actions")
       actions)
       (setq n (1+ n)))
    (dolist (action (plist-get candidates :user))
      (cl-pushnew
       `(,(format "%c" n) (,(cdr action) 'key) ,(car action) :column "User actions")
       actions)
       (setq n (1+ n)))
    (setq actions (nreverse actions))
    (eval
     `(defhydra org-roam-bibtex-note-actions-hydra (:color blue :hint nil)
        ,(format  "^\n   %s\n\n^"  (s-word-wrap (- (window-body-width) 2) name))
        ,@actions)))
  (org-roam-bibtex-note-actions-hydra/body))

(org-roam-bibtex-note-actions--frontend! 'ivy
  (if (fboundp 'ivy-read)
      (ivy-read name
                candidates
                :require-match t
                :caller #'org-roam-bibtex-note-actions
                :action (lambda (c)
                          (funcall (cdr c) (list citekey))))
    (display-warning :warning "You must have Ivy installed to use it! Falling back to default.")
    (org-roam-bibtex-note-actions--default citekey)))

(org-roam-bibtex-note-actions--frontend! 'helm
  (if (fboundp 'helm)
      (helm :sources
            `(((name . ,name)
               (candidates . ,candidates)
               (action . (lambda (f)
                           (funcall f (list ,citekey)))))))
    (display-warning :warning "You must have Helm installed to use it! Falling back to default.")
    (org-roam-bibtex-note-actions--default citekey)))

(defun org-roam-bibtex-note-actions--run (frontend citekey )
  "Run note actions on CITEKEY with FRONTEND."
  (let ((fun (intern (concat "org-roam-bibtex-note-actions--" (symbol-name frontend)))))
    (funcall fun citekey)))

;; * Main functions

;;;###autoload
(defun org-roam-bibtex-note-actions ()
  "Run an interactive prompt to offer note-related actions.
The prompt frontend can be set in
`org-roam-bibtex-note-actions-frontend'.  In addition to default
actions, which are not supposed to be modified, there is a number
of prefined extra actions `org-roam-bibtex-note-actions-extra'
that can be customized.  Additionally, user actions can be set in
`org-roam-bibtex-note-actions-user'."
  (interactive)
  (let ((non-default-frontends (list 'hydra 'ido 'ivy 'helm))
        (citekey (cdr (assoc "ROAM_KEY" (org-roam--extract-global-props '("ROAM_KEY"))))))
    ;; remove format from citekey
    (when org-roam-bibtex-citekey-format
      (string-match "\\(.*\\)%s\\(.*\\)" org-roam-bibtex-citekey-format)
      (let ((beg (match-end 1))
            (end (+ (length citekey)
                    (- (match-beginning 2)
                       (length org-roam-bibtex-citekey-format)))))
        (setq citekey (substring citekey beg end))))
    (if citekey
        (cond ((member
                org-roam-bibtex-note-actions-frontend
                non-default-frontends)
               (org-roam-bibtex-note-actions--run
                org-roam-bibtex-note-actions-frontend
                citekey))
              ((functionp
                org-roam-bibtex-note-actions-frontend)
               (funcall
                org-roam-bibtex-note-actions-frontend
                citekey))
              (t
               (org-roam-bibtex-note-actions--run
                'default
                citekey)))
      (message "#+ROAM_KEY is not found in this buffer."))))

(provide 'org-roam-bibtex-note-actions)
;;; org-roam-bibtex-note-actions.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
