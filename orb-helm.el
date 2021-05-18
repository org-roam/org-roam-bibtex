;;; orb-helm.el --- ORB support form Helm -*- lexical-binding: t -*-

;; Copyright © 2020-2021 Mykhailo Shevchuk

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;; URL: https://github.com/org-roam/org-roam-bibtex

;; Soft dependencies: projectile, persp-mode, helm, ivy, hydra

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
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE.  If not, visit
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'orb-utils)

(require 'helm-bibtex)
(require 'helm-source)

(declare-function org-ref-format-entry "ext:org-ref-bibtex" (key))

(declare-function orb-insert-edit-note "org-roam-bibtex" (citekey))

(defvar orb-note-actions-default)
(defvar orb-note-actions-extra)
(defvar orb-note-actions-user)

;; ============================================================================
;;;; Note actions
;; ============================================================================

(orb-note-actions-defun helm
  (helm :sources
        `(((name . ,name)
           (candidates . ,candidates)
           (action . (lambda (f)
                       (funcall f (list ,citekey))))))))

;; ============================================================================
;;;; Orb insert
;; ============================================================================

(defvar helm-source-orb-insert
  (helm-build-sync-source "BibTeX entries"
    :header-name (lambda (name)
                   (format "%s: " name))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
             "Edit note & insert a link"  'helm-orb-insert-edit-note
             "Open PDF, URL or DOI"       'helm-bibtex-open-any
             "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
             "Insert citation"            'helm-bibtex-insert-citation
             "Insert reference"           'helm-bibtex-insert-reference
             "Insert BibTeX key"          'helm-bibtex-insert-key
             "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
             "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
             "Show entry"                 'helm-bibtex-show-entry
             "Add PDF to library"     'helm-bibtex-add-pdf-to-library))
  "Helm source to use with `orb-insert'.
A copy of `helm-source-bibtex', in which \"Edit notes\" is made
the first (default) action.  This action calls `helm-orb-insert-edit-note'.
Only relevant when `orb-insert-interface' is `helm-bibtex'.")

(helm-bibtex-helmify-action orb-insert-edit-note helm-orb-insert-edit-note)

(defun orb-helm-insert (&optional clear-cache)
  "Run `helm-bibtex'.
If optional CLEAR-CACHE is non-nil, re-create `bibtex-completion-cache'.

This is a simple wrapper to be run from `orb-insert'."
  (let ((helm-source-bibtex helm-source-orb-insert))
    (helm-bibtex clear-cache)))

(provide 'orb-helm)

;;; orb-helm.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
