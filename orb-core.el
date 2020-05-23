;;; orb-core.el --- Org Roam Bibtex: Core library -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
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
;; This file provides org-roam-bibtex' dependencies and thus should
;; normally be required by org-roam-bibtex feature libraries.  It
;; defines customize groups and provides general utility functions
;; that depend on extra features provided through org-roam,
;; bibtex-completion and their dependencies.

;;; Code:
;; * Library requires
;;
;; 1. org-roam requires org, org-ref (with many dependencies),
;; org-element, dash, f, s, emacsql, emacsql-sqlite;
;; 2. bibtex-completion additionally requires parsebib and biblio;
;;
;; So all these libraries are always at our disposal
;;
(require 'org-roam)
(require 'bibtex-completion)

(require 'orb-utils)

;; Customize groups
(defgroup org-roam-bibtex nil
  "Org-ref and Bibtex-completion integration for Org-roam."
  :group 'org-roam
  :prefix "orb-")

(defgroup orb-note-actions nil
  "Orb Note Actions - run actions useful in note's context."
  :group 'org-roam-bibtex
  :prefix "orb-note-actions-")

;;;###autoload
(defun orb-process-file-field (citekey)
  "Process the 'file' BibTeX field and resolve if there are multiples.
Search the disk for the document associated with this BibTeX
entry.  The disk matching is based on looking in the
`bibtex-completion-library-path' for a file with the
CITEKEY.

\(Mendeley, Zotero, normal paths) are all supported.  If there
are multiple files found the user is prompted to select which one
to enter"
  (let* ((entry (bibtex-completion-get-entry citekey))
         (paths (bibtex-completion-find-pdf entry)))
    (if (= (length paths) 1)
        (car paths)
      (completing-read "File to use: " paths))))

;;;###autoload
(defun orb-find-note-file (citekey)
  "Find note file associated from BibTeX’s CITEKEY.
Returns the path to the note file, or nil if it doesn’t exist."
  (let* ((completions (org-roam--get-ref-path-completions)))
    (plist-get (cdr (assoc citekey completions)) :path)))

(provide 'orb-core)
;;; orb-core.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
