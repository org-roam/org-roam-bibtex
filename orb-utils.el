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

;; Adopted from `org-roam-version'
(defun orb-version (&optional message)
  "Return `orb-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area."
  (interactive)
  (let* ((toplib (or load-file-name buffer-file-name))
         gitdir topdir version)
    (unless (and toplib (equal (file-name-nondirectory toplib) "orb-utils.el"))
      (setq toplib (locate-library "orb-utils.el")))
    (setq toplib (and toplib (org-roam--straight-chase-links toplib)))
    (when toplib
      (setq topdir (file-name-directory toplib)
            gitdir (expand-file-name ".git" topdir)))
    (when (file-exists-p gitdir)
      (setq version
            (let ((default-directory topdir))
              (shell-command-to-string
               "git describe --tags --dirty --always"))))
    (unless version
      (setq version (with-temp-buffer
                      (insert-file-contents-literally
                       (locate-library "org-roam-bibtex.el"))
                      (goto-char (point-min))
                      (save-match-data
                        (if (re-search-forward
                             "\\(?:;; Version: \\([^z-a]*?$\\)\\)" nil nil)
                            (substring-no-properties (match-string 1))
                          "N/A")))))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

;; ============================================================================
;;;; Macros
;; ============================================================================

(defmacro orb--with-message! (message &rest body)
  "Put MESSAGE before and after BODY.
Append \"...\" to the first message and \"...done\" to the second.
Return result of evaluating the BODY."
  (declare (indent 1) (debug (stringp &rest form)))
  (let ((reporter (gensym "orb")))
    `(let ((,reporter (make-progress-reporter ,message)))
       ,@body
       (progress-reporter-done ,reporter))))

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

(defun orb-buffer-string (&optional start end)
  "Retun buffer (sub)string with no text porperties.
Like `buffer-substring-no-properties' but START and END are
optional and equal to (`point-min') and (`point-max'),
respectively, if nil."
  (buffer-substring-no-properties (or start (point-min))
                                  (or end (point-max))))

(defun orb-format (&rest args)
  "Format ARGS conditionally and return a string.
ARGS must be a plist, whose keys are `format' control strings and
values are `format' objects.  Thus only one object per control
string is allowed.  The result will be concatenated into a single
string.

In the simplest case, it behaves as a sort of interleaved `format':
==========

\(orb-format \"A: %s\" 'hello
            \" B: %s\" 'world
            \" C: %s\" \"!\")

  => 'A: hello B: world C: !'

If format object is nil, it will be formatted as empty string:
==========

\(orb-format \"A: %s\" 'hello
            \" B: %s\" nil
            \" C: %s\" \"!\")
  => 'A: hello C: !'

Object can also be a cons cell.  If its car is nil then its cdr
will be treated as default value and formatted as \"%s\":
==========

\(orb-format \"A: %s\" 'hello
            \" B: %s\" '(nil . dworl)
            \" C: %s\" \"!\")
  => 'A: hellodworl C: !'

Finally, if the control string is nil, the object will be formatted as \"%s\":
==========

\(orb-format \"A: %s\" 'hello
            \" B: %s\" '(nil . \" world\")
             nil \"!\")
=> 'A: hello world!'."
  (let ((res ""))
    (while args
      (let ((str (pop args))
            (obj (pop args)))
        (unless (consp obj)
          (setq obj (cons obj nil)))
        (setq res
              (concat res
                      (format (or (and (car obj) str) "%s")
                              (or (car obj) (cdr obj) ""))))))
    res))

;; ============================================================================
;;;; Temporary files
;; ============================================================================

;;;;; Code in this section was adopted from ob-core.el
;;
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
;;
;; Authors: Eric Schulte
;;          Dan Davison

(defvar orb--temp-dir)
(unless (or noninteractive (boundp 'orb--temp-dir))
  (defvar orb--temp-dir
    (or (and (boundp 'orb--temp-dir)
             (file-exists-p orb--temp-dir)
             orb--temp-dir)
        (make-temp-file "orb-" t))
"Directory to hold temporary files created during reference parsing.
Used by `orb-temp-file'.  This directory will be removed on Emacs
shutdown."))

(defun orb-temp-file (prefix &optional suffix)
  "Create a temporary file in the `orb--temp-dir'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of variable `temporary-file-directory' temporarily set to
the value of `orb--temp-dir'."
  (let ((temporary-file-directory
         (or (and (boundp 'orb--temp-dir)
                  (file-exists-p orb--temp-dir)
                  orb--temp-dir)
             temporary-file-directory)))
    (make-temp-file prefix nil suffix)))

(defun orb--remove-temp-dir ()
  "Remove `orb--temp-dir' on Emacs shutdown."
  (when (and (boundp 'orb--temp-dir)
             (file-exists-p orb--temp-dir))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
        (progn
          (mapc (lambda (file)
                  ;; This test is equivalent to
                  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
                  ;; but more efficient
                  (if (eq t (car (file-attributes file)))
                      (delete-directory file)
                    (delete-file file)))
                (directory-files orb--temp-dir 'full
                                 directory-files-no-dot-files-regexp))
          (delete-directory orb--temp-dir))
      (error
       (message "Failed to remove temporary Org-roam-bibtex directory %s"
                (if (boundp 'orb--temp-dir)
                    orb--temp-dir
                  "[directory not defined]"))))))

(add-hook 'kill-emacs-hook 'orb--remove-temp-dir)

;;;;; End of code adopted from ob-core.el

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
  "Return the value of Org KEYWORD in-buffer directive.
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
  (when-let ((node (or node
                       ;; try to get node at point only in Org mode;
                       ;; can brake other things if not in Org mode, see e.g.
                       ;; https://github.com/org-roam/org-roam-bibtex/issues/268
                       (and (derived-mode-p 'org-mode)
                            (org-roam-node-at-point assert))))
             (node-refs (cdr (assoc-string
                              "ROAM_REFS"
                              (org-roam-node-properties node) t))))
    (let* ((ref-list (split-string-and-unquote node-refs)))
      (catch 'found
        (dolist (ref ref-list)
          (when (string-match orb-utils-citekey-re ref)
            (throw 'found (match-string 1 ref))))))))

(defun orb-format-entry (citekey)
  "Format a BibTeX entry for display, whose citation key is CITEKEY.
Uses `bibtex-completion-format-entry' internally and so the
display can be tweaked in the `bibtex-completion-display-formats'
variable."
  ;; NOTE: A drop-in replacement for `org-ref-format-entry' which was removed
  ;; in Org-ref v3.  Still waiting for a native solution.
  (bibtex-completion-init)
  (bibtex-completion-format-entry
   (bibtex-completion-get-entry citekey) (1- (frame-width))))

(provide 'orb-utils)
;;; orb-utils.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
