;;; orb-ivy.el --- ORB support for Ivy -*- lexical-binding: t -*-

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

(require 'ivy-bibtex)

(declare-function org-ref-format-entry "ext:org-ref-bibtex" (key))

(declare-function orb-insert-edit-note "org-roam-bibtex" (citekey))

(defvar orb-note-actions-default)
(defvar orb-note-actions-extra)
(defvar orb-note-actions-user)

;; ============================================================================
;;;; Note actions
;; ============================================================================

(orb-note-actions-defun ivy
  (ivy-read name
            candidates
            :require-match t
            :caller 'orb-note-actions-ivy
            :action (lambda (c)
                      (funcall (cdr c) (list citekey)))))

;; ============================================================================
;;;; Orb insert
;; ============================================================================

(defvar orb-insert--ivy-actions
  '(("e" ivy-orb-insert-edit-note "Edit note & insert a link")
    ("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
    ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser")
    ("c" ivy-bibtex-insert-citation "Insert citation")
    ("r" ivy-bibtex-insert-reference "Insert reference")
    ("k" ivy-bibtex-insert-key "Insert BibTeX key")
    ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
    ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email")
    ("s" ivy-bibtex-show-entry "Show entry")
    ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library")
    ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options"))
  "Ivy actions to use with `orb-insert'.
A copy of Ivy-bibtex's alist defining Ivy actions, in which
\"Edit note & insert a link\" is made first (default) action.
This action calls `orb-insert-edit-note'.  Only relevant when
`orb-insert-interface' is `ivy-bibtex'.")

(ivy-bibtex-ivify-action orb-insert-edit-note ivy-orb-insert-edit-note)

(defun orb-ivy-insert (&optional clear-cache)
  "Run `ivy-bibtex'.
If optional CLEAR-CACHE is non-nil, re-create `bibtex-completion-cache'.

This is a simple wrapper to be run from `orb-insert'."
  (let* ((ivy-actions (copy-tree ivy--actions-list))
         (ivy--actions-list
          (plist-put ivy-actions 'ivy-bibtex orb-insert--ivy-actions))
         (ivy-bibtex-default-action 'ivy-orb-insert-edit-note))
    (ivy-bibtex clear-cache)))

(provide 'orb-ivy)

;;; orb-ivy.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
