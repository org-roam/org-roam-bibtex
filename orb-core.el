;;; orb-core.el --- Org Roam BibTeX: core library -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Mykhailo Shevchuk
;; Copyright © 2020 Leo Vivier

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
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

(require 'orb-utils)
(require 'orb-compat)

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x)
  (require 'rx))

(declare-function
 bibtex-completion-get-entry "bibtex-completion" (entry-key))
(declare-function
 bibtex-completion-get-value "bibtex-completion" (field entry &optional default))
(declare-function
 bibtex-completion-find-pdf (key-or-entry &optional find-additional))


;; ============================================================================
;;; Customize groups
;; ============================================================================
;;
;; All modules should put their `defgroup' definitions here
;; Defcustom definitions should stay in respective files

(defgroup org-roam-bibtex nil
  "Org-roam integration with BibTeX software."
  :group 'org-roam
  :prefix "orb-")

(defgroup orb-note-actions nil
  "Orb Note Actions - run actions in note's context."
  :group 'org-roam-bibtex
  :prefix "orb-note-actions-")


;; ============================================================================
;;; BibTeX fields and their special handling
;; ============================================================================

(defcustom orb-bibtex-field-aliases
  '(("=type=" . "entry-type")
    ("=key=" . "citekey")
    ("=has-pdf=" . "pdf?")
    ("=has-note=" . "note?")
    ("citation-number" . "#"))
  "Alist of ORB-specific field aliases of the form (FIELD . ALIAS).
The ALIAS can be used instead of the FIELD anywhere in ORB's
configuration.  This variable is useful to replace
`bibtex-completion''s internal '='-embraced virtual fields with
more casual alternatives."
  :group 'org-roam-bibtex
  :type '(repeat
          (cons (string :tag "Field name")
                (string :tag "Alias name"))))

(defcustom orb-attached-file-extensions '("pdf")
  "When retrieving an attached file, keep files with only these extensions.
This is a list of file extensions without a dot as case-insensitive
strings.

Set it to nil to keep all file names regardless of their extensions.

BibTeX entries are searched for attached files according to
`bibtex-completion-pdf-field' (default `file') and in
BibDesk-specific `Bdsk-File-N' fields."
  :group 'org-roam-bibtex
  :type '(repeat :tag "List of extensions" (string)))

(defcustom orb-abbreviate-file-name t
  "Non-nil to force abbreviation of file names by `orb-get-attached-file'.
When this option is set to a non-nil value, the filename returned
by `orb-get-attached-file' will get the home directory part
abbreviated to `~/'.  Symlinked directories will be abbreviated
according to `directory-abbrev-alist', see `abbreviate-file-name'
for details.

An as-is value will be used otherwise."
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom orb-use-bibdesk-attachments nil
  "Whether to look up BibDesk-specific file fields `Bdsk-File'.
If this is non-nil, attachments given in BibDesk-specific file
fields will be considered in addition to those found through the
`bibtex-completion-find-pdf' mechanism when performing a template
expansion, opening an attachment with `orb-note-actions' or
scraping a PDF with `refuse' (formerly `orb-pdf-scrapper').

Duplicates will be resolved, but since duplicate comparison is
performed using `file-truename', this will lead to expansion of
symlink paths if such are used in the normal BibTeX `file' field,
for example.  See also `orb-abbreviate-file-name' on how to
abbreviate the retrieved filenames.

Set this to symbol `only' to look up only BibDesk attachments and
do not use `bibtex-completion-find-pdf'."
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "BibDesk only" only)
          (const :tag "No" nil)))

(defsubst orb-resolve-field-alias (alias)
  "Return ALIAS association from `orb-bibtex-field-aliases'.
Return ALIAS if association was not found."
  (or (car (rassoc alias orb-bibtex-field-aliases)) alias))

(defun orb-get-bibdesk-filenames (entry)
  "Return filenames stored in BibDesk file fields \"Bdsk-File-N\".
ENTRY is a BibTeX entry as returned by `bibtex-completion-get-entry'.

The variable `orb-attached-file-extensions' is respected."
  ;; NOTE: Mac-specific, hard-coded
  (let* ((bdsk-file-fields
          (seq-filter (lambda (cell)
                        (string-match-p "Bdsk-File" (car cell)))
                      entry))
         (strip-value-rx
          (rx (seq (opt (in  "\"{"))
                   (group (* (not (in "\"{}"))))
                   (opt (in  "\"}")))))
         (filename-rx
          (concat
           (rx (seq "Users/" (* anychar)))
           (if orb-attached-file-extensions
               (regexp-opt orb-attached-file-extensions t)
             "pdf")))
         (bdsk-files
          (mapcar
           (lambda (cell)
             (let ((val (cdr cell))
                   file)
               (when (string-match strip-value-rx val)
                 (setq file (base64-decode-string (match-string 1 val)))
                 (when (string-match filename-rx file)
                   (concat "/" (match-string 0 file))))))
           bdsk-file-fields)))
    (seq-filter (lambda (val) val) bdsk-files)))

;;;###autoload
(defun orb-get-attached-file (citekey)
  "Look up files associated with a BibTeX entry identified by CITEKEY.
Files are searched for using `bibtex-completion-find-pdf',
meaning that Mendeley, Zotero and plain file paths are all
supported, and variables `bibtex-completion-pdf-field' and
`bibtex-completion-library-path' are respected.  Additionally,
the BibTeX entry is searched for BibDesk-specific file fields
`Bdsk-File-N'.

If `orb-attached-file-extensions' is non-nil, return only file paths
matching the respective extensions.

If `orb-abbreviate-file-name' is non-nil, force an abbreviated
file name.

Depending on the value of `orb-use-bibdesk-attachments', the
BibDesk-specific file fields `Bdsk-File-N' may or may not be used
for the lookup.

If multiple files have been found, the user will be prompted to
select one."
  (condition-case err
      (when-let* ((entry (bibtex-completion-get-entry citekey))
                  (paths
                   (--> (pcase orb-use-bibdesk-attachments
                          (`nil (bibtex-completion-find-pdf
                                 entry bibtex-completion-find-additional-pdfs))
                          (`only (orb-get-bibdesk-filenames entry))
                          (_
                           (-->
                            (nconc (bibtex-completion-find-pdf entry)
                                   (orb-get-bibdesk-filenames entry))
                            (-map #'file-truename it)
                            (-uniq it))))
                        (if (not orb-attached-file-extensions)
                            it          ; do not filter by extensions
                          ;; filter by extensions
                          (--filter
                           (when-let ((ext (file-name-extension it)))
                             (member-ignore-case ext orb-attached-file-extensions))
                           it))))
                  (path (if (cdr paths)
                            (completing-read "File to use: " paths)
                          (car paths))))
        (if orb-abbreviate-file-name
            (abbreviate-file-name path)
          path))
    ;; ignore any errors that may be thrown by `bibtex-completion-find-pdf'
    ;; don't stop the capture process
    (error
     (orb-warning
      (format "error in `orb-get-attached-file`: %s %s"
              (car err) (cdr err))))))

;;;###autoload
(defun orb-open-attached-file (citekey)
  "Open a file associated with CITEKEY.
CITEKEY must be a list for compatibility with `bibtex-completion'
functions, which also expect a list.

This is a modified and simplified version of `bibtex-completion-open-pdf',
which uses `orb-get-bibdesk-filenames' under the hood and is therefore
compatible with BibDesk.  The file is opened with the function set in
`bibtex-completion-pdf-open-function'.

The intended primary use is with `orb-note-actions'."
  (let* ((key (car citekey))
         (attachment (orb-get-attached-file key)))
    (if attachment
        (funcall bibtex-completion-pdf-open-function (file-truename attachment))
      (message "No PDF(s) found for this entry: %s" key))))

(provide 'orb-core)
;;; orb-core.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
