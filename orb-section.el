;;; orb-section.el --- Org Roam BibTeX: Sections for org-roam-mode -*- lexical-binding: t -*-

;; Copyright © 2022 Samuel W. Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>

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
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE.  If not, visit
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides org-roam-bibtex' dependencies and thus should
;; normally be required by org-roam-bibtex feature libraries.  It
;; defines customize groups and provides general utility functions
;; that depend on extra features provided through org-roam,
;; bibtex-completion and their dependencies.

;;; Code:

;; ============================================================================
;;; Dependencies
;; ============================================================================

(require 'org-roam-node)
(require 'org-roam-utils)
(require 's)
(require 'magit-section)
(require 'bibtex-completion)

(require 'orb-utils)
(defvar orb-abbreviate-file-name)

;; ============================================================================
;;; Configuration Variables
;; ============================================================================

(defgroup orb-section nil
  "Org-roam buffer sections for BibTeX."
  :group 'org-roam-bibtex
  :prefix "orb-section-")

(defcustom orb-section-reference-format-method
  'bibtex-completion-apa-format-reference
  "How to format the ORB citation.
Either a function taking a bibtex key and returning a string, or
an alist from type to format string.  For formatting information,
see `bibtex-completion-display-formats'."
  :type '(choice (const :tag "Use BibTeX-Completion APA Format"
                        'bibtex-completion-apa-format-reference)
          (symbol :tag "Use a function")
          (alist :key-type   (choice (string :tag "Type Name    :")
                                     (const :tag "Default" t))
                 :value-type (string :tag "Format String:"))))

(defcustom orb-section-abstract-format-method :org-format
  "How to format ORB abstract.
A function taking a key and returning a string, or one of:

 - `:org-format' Assume that the content is org-formatted, and
   format accordingly.
 - `:pandoc-from-tex' Assume that the content is tex/latex
   formatted and use `pandoc' to format accordingly."
  :type '(choice (const :tag "Format as Org Text" :org-format)
          (const :tag "Format from LaTeX" :pandoc-from-tex)
          (symbol :tag "Use function.")))


;; ============================================================================
;;; Utility functions
;; ============================================================================

(defun orb-section-reference-format (key)
  "Format reference for KEY according to `orb-section-reference-format-method'."
  (if (functionp orb-section-reference-format-method)
      (funcall orb-section-reference-format-method key)
    (when-let ((entry (bibtex-completion-get-entry key))
               (format-string (cdr (or (assoc-string (bibtex-completion-get-value "=type=" entry) orb-section-reference-format-method)
                                       (assoc t orb-section-reference-format-method))))
               (formatted-reference (s-format format-string 'bibtex-completion-apa-get-value entry)))
      (replace-regexp-in-string "\\([.?!]\\)\\." "\\1" formatted-reference))))

(defun orb-section-unfill-region (beg end)
  "Unfill the region from BEG to END.
Joining text paragraphs into a single logical line.

Taken from https://www.emacswiki.org/emacs/UnfillRegion"
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun orb-section-abstract-format (key)
  "Format abstract for KEY per `orb-section-abstract-format-method'."
  (if (functionp orb-section-abstract-format-method)
      (funcall orb-section-abstract-format-method key)
    (when-let ((entry (bibtex-completion-get-entry key))
               (abstract (bibtex-completion-get-value "abstract" entry)))
      (pcase orb-section-abstract-format-method
        (:org-format
         (org-roam-fontify-like-in-org-mode
          (with-temp-buffer
            (insert abstract)
            (org-mode)
            (orb-section-unfill-region (point-min) (point-max))
            (save-match-data "\n\n" "\n" (string-trim (buffer-string))))))
        (:pandoc-from-tex
         (org-roam-fontify-like-in-org-mode
          (with-temp-buffer
            (insert abstract)
            (shell-command-on-region (point-min) (point-max)
                                     "pandoc -f latex -t org" (current-buffer) t)
            (org-mode)
            (orb-section-unfill-region (point-min) (point-max))
            (save-match-data "\n\n" "\n" (string-trim (buffer-string))))))))))


;; ============================================================================
;;; Section Implementation
;; ============================================================================

;;;###autoload
(defun orb-section-reference (node)
  "Show BibTeX reference for NODE if it exists."
  (when-let ((cite-key (orb-get-node-citekey node))
             (formatted-reference (orb-section-reference-format cite-key)))
    (magit-insert-section (orb-section-reference)
      (magit-insert-heading "Reference:")
      (insert formatted-reference)
      (insert "\n\n"))))

;;;###autoload
(defun orb-section-abstract (node)
  "Show BibTeX entry abstract for NODE if it exists."
  (when-let ((cite-key (orb-get-node-citekey node))
             (formatted-abstract (orb-section-abstract-format cite-key)))
    (magit-insert-section (orb-section-abstract)
      (magit-insert-heading "Abstract:")
      (insert formatted-abstract)
      (insert "\n\n"))))

;;;###autoload
(defun orb-section-file (node)
  "Show a link to entry file for NODE if it exists."
  (when-let ((cite-key (orb-get-node-citekey node))
             (file-name (let ((orb-abbreviate-file-name nil))
                          (orb-get-attached-file cite-key))))
    (magit-insert-section (orb-section-file)
      (magit-insert-heading "File:")
      (insert-text-button (file-name-nondirectory file-name)
                          'action (lambda (_button) (orb-open-attached-file cite-key)))
      (insert "\n\n"))))

(provide 'orb-section)
;;; orb-section.el ends here
;;
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
