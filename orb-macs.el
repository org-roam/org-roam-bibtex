;;; orb-macs.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex, org-ref
;; Version: 0.2.3
;; Package-Requires: ((emacs "26.1") (f "0.20.0") (s "1.12.0") (org "9.3") (org-roam "1.0.0") (bibtex-completion "2.0.0"))

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

;; N.B. This file contains code snippets adopted from other
;; open-source projects. These snippets are explicitly marked as such
;; in place. They are not subject to the above copyright and
;; authorship claims.

;;; Commentary:
;;
;; This file contains macros and helper functions used accross different
;; org-mode-bibtex modules.

;;; Code:
;; * Library requires
(require 'orb-compat)

(defvar orb-citekey-format)

;; * Macros

(defmacro orb-with-message (message &rest body)
  "Put MESSAGE before and after BODY.
Append \"...\" to the first message and \"...done\" to the second.
Return result of evaluating the BODY."
  (declare (indent 1) (debug (stringp &rest form)))
  `(prog2
       (message "%s..." ,message)
       (progn ,@body)
     (message "%s...done" ,message)))

;; * Functions

(defun orb-unformat-citekey (citekey)
  "Remove format from CITEKEY.
Format is `orb-citekey-format'."
  (string-match "\\(.*\\)%s\\(.*\\)" orb-citekey-format)
  (let ((beg (match-end 1))
        (end (+ (length citekey)
                (- (match-beginning 2)
                   (length orb-citekey-format)))))
    (substring citekey beg end)))

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


;;; Code in this section was adopted from ob-core.el
;;
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
;;
;; Authors: Eric Schulte
;;          Dan Davison

(defvar orb-temp-dir)
(unless (or noninteractive (boundp 'orb-temp-dir))
  (defvar orb-temp-dir
    (or (and (boundp 'orb-temp-dir)
             (file-exists-p orb-temp-dir)
             orb-temp-dir)
        (make-temp-file "orb-" t))
"Directory to hold temporary files created during reference parsing.
Used by `orb-temp-file'.  This directory will be removed on Emacs
shutdown."))

(defun orb-temp-file (prefix &optional suffix)
  "Create a temporary file in the `orb-temp-dir'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of variable `temporary-file-directory' temporarily set to
the value of `orb-temp-dir'."
  (let ((temporary-file-directory
         (or (and (boundp 'orb-temp-dir)
                  (file-exists-p orb-temp-dir)
                  orb-temp-dir)
             temporary-file-directory)))
    (make-temp-file prefix nil suffix)))

(defun orb-remove-temp-dir ()
  "Remove `orb-temp-dir' on Emacs shutdown."
  (when (and (boundp 'orb-temp-dir)
             (file-exists-p orb-temp-dir))
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
                (directory-files orb-temp-dir 'full
                                 directory-files-no-dot-files-regexp))
          (delete-directory orb-temp-dir))
      (error
       (message "Failed to remove temporary Org-roam-bibtex directory %s"
                (if (boundp 'orb-temp-dir)
                    orb-temp-dir
                  "[directory not defined]"))))))

(add-hook 'kill-emacs-hook 'orb-remove-temp-dir)

;;; End of code adopted from ob-core.el

(provide 'orb-macs)
;;; orb-macs.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
