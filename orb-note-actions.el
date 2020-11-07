;;; orb-note-actions.el --- Org Roam Bibtex: note actions -*- lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; URL: https://github.com/org-roam/org-roam-bibtex

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

(require 'orb-core)

(require 'warnings)
(require 'cl-lib)

(declare-function helm "helm")
(declare-function ivy-read "ivy")
(declare-function defhydra "hydra")

(declare-function org-ref-format-entry "org-ref-bibtex" (key))

(declare-function orb-pdf-scrapper-run "orb-pdf-scrapper" (key))

;; * Customize definitions

(defcustom orb-note-actions-frontend 'default
  "Interface frontend for `orb-note-actions'.
Supported values (frontends) are 'default, 'ido, 'hydra, 'ivy and 'helm.

Alternatively, it can be set to a function, in which case the
function should expect one argument CITEKEY, which is a list
whose car is the citation key associated with the org-roam note
the current buffer is visiting.  Also, it should ideally make use
of `orb-note-actions-default', `orb-note-actions-extra' and
`orb-note-actions-user' for providing an interactive interface,
through which the combined set of note actions is presented as a
list of candidates and the function associated with the candidate
is executed upon selecting it."
  :risky t
  :type '(radio
          (const :tag "Default" default)
          (const :tag "Ido" ido)
          (const :tag "Hydra" hydra)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (function :tag "Custom function"))
  :group 'orb-note-actions)

(defcustom orb-note-actions-default
  '(("Open PDF file(s)" . bibtex-completion-open-pdf)
    ("Add PDF to library" . bibtex-completion-add-pdf-to-library)
    ("Open URL or DOI in browser" . bibtex-completion-open-url-or-doi)
    ("Show record in the bibtex file" . bibtex-completion-show-entry))
  "Default actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :risky t
  :type '(alist
          :tag "Default actions for `orb-note-actions'"
          :key-type (string :tag "Description")
          :value-type (function :tag "Function"))
  :group 'orb-note-actions)

(defcustom orb-note-actions-extra
  '(("Save citekey to kill-ring and clipboard" . orb-note-actions-copy-citekey)
    ("Run Orb PDF Scrapper" . orb-note-actions-scrap-pdf))
  "Extra actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :risky t
  :type '(alist
          :tag "Extra actions for `orb-note-actions'"
          :key-type (string :tag "Description")
          :value-type (function :tag "Function"))
  :group 'orb-note-actions)

(defcustom orb-note-actions-user nil
  "User actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :risky t
  :type '(alist
          :tag "User actions for `orb-note-actions'"
          :key-type (string :tag "Description")
          :value-type (function :tag "Function"))
  :group 'orb-note-actions)


;; * Helper functions

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
  (let* ((frontend-name (symbol-name frontend))
         (fun-name (intern (concat "orb-note-actions--" frontend-name))))
    `(defun ,fun-name (citekey)
       ,(format "Provide note actions using %s interface.
CITEKEY is the citekey." (capitalize frontend-name))
       (let ((name (org-ref-format-entry citekey)) ;; TODO: make a native format function
             (candidates
              ,(unless (eq frontend 'hydra)
                 '(append  orb-note-actions-default
                           orb-note-actions-extra
                           orb-note-actions-user))))
         ,@body))))

(orb-note-actions--frontend! default
  (let ((f (cdr (assoc (completing-read name candidates) candidates))))
    (funcall f (list citekey))))

(orb-note-actions--frontend! ido
  (let* ((c (cl-map 'list 'car candidates))
         (f (cdr (assoc (ido-completing-read name c) candidates))))
    (funcall f (list citekey))))

(declare-function orb-note-actions-hydra/body "orb-note-actions" nil t)

(orb-note-actions--frontend! hydra
;; we don't use candidates here because for a nice hydra we need each
;; group of completions separately (default, extra, user), so just
;; silence the compiler
  (if (fboundp 'defhydra)
      (progn
        (ignore candidates)
        (let ((k ?a)
              actions)
          (dolist (type (list "Default" "Extra" "User"))
            (let ((actions-var
                   (intern (concat "orb-note-actions-" (downcase type)))))
              (dolist (action (symbol-value actions-var))
   ;; this makes defhydra HEADS list of form:
   ;; ("a" (some-action citekey-value) "Some-action description" :column "Type")
                (cl-pushnew
                 `(,(format "%c" k) (,(cdr action) (list ,citekey))
                   ,(car action) :column ,(concat type " actions"))
                 actions)
                ;; increment key a->b->c...
                (setq k (1+ k))))) ; TODO: figure out a way to supply
                                   ; mnemonic keys
          (setq actions (nreverse actions))
          ;; yes, we redefine hydra on every call
          (eval
           `(defhydra orb-note-actions-hydra (:color blue :hint nil)
              ;; defhydra docstring
              ,(format  "^\n  %s \n\n^"
                        (s-word-wrap (- (window-body-width) 2) name))
              ;; defhydra HEADS
              ,@actions)))
        (orb-note-actions-hydra/body))
    (display-warning :warning "You must have Hydra installed to use it!  \
Falling back to default.")
    (orb-note-actions--default citekey)))

(orb-note-actions--frontend! ivy
  (if (fboundp 'ivy-read)
      (ivy-read name
                candidates
                :require-match t
                :caller #'orb-note-actions
                :action (lambda (c)
                          (funcall (cdr c) (list citekey))))
    (display-warning :warning "You must have Ivy installed to use it!  \
Falling back to default.")
    (orb-note-actions--default citekey)))

(orb-note-actions--frontend! helm
  (if (fboundp 'helm)
      (helm :sources
            `(((name . ,name)
               (candidates . ,candidates)
               (action . (lambda (f)
                           (funcall f (list ,citekey)))))))
    (display-warning :warning "You must have Helm installed to use it!  \
Falling back to default.")
    (orb-note-actions--default citekey)))

(defun orb-note-actions--run (frontend citekey )
  "Run note actions on CITEKEY with FRONTEND."
  (let ((fun (intern (concat "orb-note-actions--" (symbol-name frontend)))))
    (funcall fun citekey)))

;; * Note actions

(defun orb-note-actions-copy-citekey (citekey)
  "Save note's citation key to `kill-ring' and copy it to clipboard.
CITEKEY is a list whose car is a citation key."
  (with-temp-buffer
    (insert (car citekey))
    (copy-region-as-kill (point-min) (point-max))))

(defun orb-note-actions-scrap-pdf (citekey)
  "Wrapper around `orb-pdf-scrapper-insert'.
CITEKEY is a list whose car is a citation key."
  (require 'orb-pdf-scrapper)
  (orb-pdf-scrapper-run (car citekey)))


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
        (citekey (cdr (org-roam--extract-ref))))
    (if citekey
        (cond ((member orb-note-actions-frontend non-default-frontends)
               (orb-note-actions--run orb-note-actions-frontend citekey))
              ((functionp orb-note-actions-frontend)
               (funcall orb-note-actions-frontend citekey))
              (t
               (orb-note-actions--run 'default citekey)))
      (user-error "No #+ROAM_KEY found in current buffer"))))

(provide 'orb-note-actions)
;;; orb-note-actions.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
