;;; orb-utils.el --- Org Roam BibTeX: utility macros and functions -*- lexical-binding: t -*-

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

;; N.B. This file contains code snippets adopted from other
;; open-source projects. These snippets are explicitly marked as such
;; in place. They are not subject to the above copyright and
;; authorship claims.

;;; Commentary:
;;
;; This file contains utility macros and helper functions used accross
;; different org-mode-bibtex modules.  This library may be required
;; directly or through orb-core.el.  Definitions in this file should
;; only depend on built-in Emacs libraries.

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================
;;
;; org-roam requires org,org-element, dash, f, s, emacsql, emacsql-sqlite,
;; so all these libraries are always at our disposal

(require 'org-roam)
(require 'bibtex-completion)

(require 'warnings)

(eval-when-compile
  (require 'subr-x))

(defvar org-ref-cite-types)

;; ============================================================================
;;;; Macros
;; ============================================================================

(defmacro orb-note-actions-defun (interface &rest body)
  "Return a function definition for INTERFACE.
Function name takes a form of orb-note-actions--INTERFACE.  A
simple docstring is constructed and BODY is injected into a `let'
form, which has two variables bound, NAME and CANDIDATES.  NAME
is a string formatted with `orb-format-entry' and CANDIDATES
is a cons cell alist constructed from `orb-note-actions-default',
`orb-note-actions-extra', and `orb-note-actions-user'."
  (declare (indent 1) (debug (symbolp &rest form)))
  (let* ((interface-name (symbol-name interface))
         (fun-name (intern (concat "orb-note-actions-" interface-name))))
    `(defun ,fun-name (citekey)
       ,(format "Provide note actions using %s interface.
CITEKEY is the citekey." (capitalize interface-name))
       (let ((name (orb-format-entry citekey)) ;; TODO: make a native format function
             (candidates
              ,(unless (eq interface 'hydra)
                 '(append  orb-note-actions-default
                           orb-note-actions-extra
                           orb-note-actions-user))))
         ,@body))))
 
;; ============================================================================
;;;; General utilities
;; ============================================================================

(defun orb-warning (warning &optional citekey)
  "Display a WARNING message.  Return nil.
Include CITEKEY if it is non-nil."
  (display-warning
   :warning (concat "ORB: " (when citekey (format "%s :" citekey)) warning))
  nil)

;; ============================================================================
;;;; Document properties
;; ============================================================================

(defvar orb-utils-citekey-re
  ;; NOTE: Not tested thoroughly
  (rx
   (or
    (seq (group-n 2 (regexp
                     ;; If Org-ref is available, use its types
                     ;; default to "cite"
                     (if (boundp 'org-ref-cite-types)
                         (regexp-opt
                          (mapcar
                           (lambda (el)
                             ;; Org-ref v3 cite type is a list of strings
                             ;; Org-ref v2 cite type is a plain string
                             (or (car-safe el) el))
                           org-ref-cite-types))
                       "cite")))
         ":"
         (or
          ;; Org-ref v2 style `cite:links'
          (group-n 1 (+ (any "a-zA-Z0-9_:.-")))
          ;; Org-ref v3 style `cite:Some&key'
          (seq (*? (not "&")) "&"
               (group-n 1 (+ (any "!#-+./:<>-@^-`{-~-" word))))))
    ;; Org-cite [cite/@citations]
    (seq "@" (group-n 1 (+ (any "!#-+./:<>-@^-`{-~-" word))))))
  "Universal regexp to match citations in `ROAM_REFS'.
Supports Org-ref v2 and v3 and Org-cite.")

(defun orb-get-db-cite-refs ()
  "Get a list of `cite` refs from Org Roam database."
  (let* ((types "cite")
         (refs (org-roam-db-query
                [:select [ref nodes:file id pos title type]
                 :from refs
                 :left-join nodes
                 :on (= refs:node-id nodes:id)
                 :where (= type $s1)]
                types))
         result)
    (dolist (ref refs result)
      (push (-interleave '(:ref :file :id :pos :title :type) ref) result))))

(defvar orb-notes-cache nil
  "Cache of ORB notes.")

(defun orb-make-notes-cache ()
  "Update ORB notes hash table `orb-notes-cache'."
  (let* ((db-entries (orb-get-db-cite-refs))
         (size (round (/ (length db-entries) 0.8125))) ;; ht oversize
         (ht (make-hash-table :test #'equal :size size)))
    (dolist (entry db-entries)
      (puthash (plist-get entry :ref)
               (org-roam-node-create
                :id (plist-get entry :id)
                :file (plist-get entry :file)
                :title (plist-get entry :title)
                :point (plist-get entry :pos))
               ht))
    (setq orb-notes-cache ht)))

(defun orb-find-note-file (citekey)
  "Find note file associated with CITEKEY.
Returns the path to the note file, or nil if it doesn’t exist."
  (when-let ((node (gethash citekey (or orb-notes-cache
                                        (orb-make-notes-cache)))))
    (org-roam-node-file node)))

(defun orb-get-buffer-keyword (keyword &optional buffer)
  "Return the value of Org-mode KEYWORD in-buffer directive.
The KEYWORD should be given as a string without \"#+\", e.g. \"title\".

If optional BUFFER is non-nil, return the value from that buffer
instead of `current-buffer'."
  ;; NOTE: does not work with `org-element-multiple-keywords' keywords
  ;; if that will somewhen be required, `org-element' should be used.
  (with-current-buffer (or buffer (current-buffer))
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward
         (format "^[ 	]*#\\+%s:[ 	]*\\(.*\\)$" keyword) nil t)
        (match-string-no-properties 1)))))

(defun orb-note-exists-p (citekey)
  "Check if a note exists whose citekey is CITEKEY.
Return Org Roam node or nil."
  ;; NOTE: This function can be made more general.
  (gethash citekey (or orb-notes-cache
                       (orb-make-notes-cache))))

(defun orb-get-node-citekey (&optional node assert)
  "Return citation key associated with NODE.
If optional NODE is nil, return the citekey for node at point.
ASSERT will be passed to `org-roam-node-at-point'.  If it is
non-nil, an error will be thrown if there is no node at point."
  (when-let ((node (or node (org-roam-node-at-point assert))))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (let* ((prop (org-entry-get (point) "ROAM_REFS"))
             (prop-list (when prop (split-string-and-unquote prop))))
        (catch 'found
          (dolist (p prop-list)
            (when (string-match orb-utils-citekey-re p)
              (throw 'found (match-string 1 p)))))))))

(defun orb-format-entry (citekey)
  "Format a BibTeX entry for display, whose citation key is CITEKEY.
Uses `bibtex-completion-format-entry' internally and so the
display can be tweaked in the `bibtex-completion-display-formats'
variable."
  ;; NOTE: A drop-in replacement for `org-ref-format-entry' which was removed
  ;; in Org-ref v3.  Still waiting for a native solution.
  (bibtex-completion-format-entry
   (bibtex-completion-get-entry citekey) (1- (frame-width))))

(provide 'orb-utils)
;;; orb-utils.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
