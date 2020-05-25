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

;; * Automatic generation of citation keys

(defvar orb-autokey-format)
;; debug
(setq orb-autokey-format "%y-%e{(format \"%%y\")}-%a*[2][3][_]-%T[3][2][.]")

(defvar orb-autokey-titlewords-ignore bibtex-autokey-titleword-ignore
  "Patterns from title that will be ignored during key generation.
Every element is a regular expression to match parts of the title
that should be ignored during automatic key generation.  Case
sensitive.

Default value is set from `bibtex-autokey-titleword-ignore'.")

(defun orb-autokey-generate-key (entry)
  "Generate citation key from ENTRY according to orb-autokey-format.

%A*[2][5][_] - first two author's last names plus initials delimited with _, retain capitals
%a*[2][5][_] - first two author's last names plus initials delimited with _, downcase

%y - year xxxx
%Y - year xx (two last digits)
%p - first page
%p[-] first and last page delimited with -

%T*[5][2][:] - first five characters of first 2 words in title delimited with :, retain capitals
%t*[5][2][:] - first five characters of first 2 words in title delimited with :, downcase
starred version - ignore words from `orb-autokey

%F{field}[3][2][-] - 2 first characters of first 3 words in field field delimited with -, retain capitals
%f{field}[3][2][-] - 2 first characters of first 3 words in field field delimited with -, downcase

%e{(expr)} - elisp expression that should return string, possibly with
the above wildcards.  It has a variable entry bound which is the current bibtex entry."
  (let* ((case-fold-search nil)
         (str (copy-seq orb-autokey-format))
         (a-rx
          (rx (group-n 1
                       (or "%a" (group-n 2 "%A"))
                       (opt (group-n 3 "*"))
                       (opt (and "[" (group-n 4 digit) "]"))
                       (opt (and "[" (group-n 5 digit) "]"))
                       (opt (and "[" (group-n 6 (any alnum "_.:|-")) "]")))))
         (y-rx (rx (group-n 1 "%y")))
         (Y-rx (rx (group-n 1 "%Y")))
         (p-rx
          (rx (group-n 1 "%p"
                       (opt (and "[" (group-n 2 (any alnum "_.:|-")) "]")))))
         (t-rx
          (rx (group-n 1
                       (or "%t" (group-n 2 "%T"))
                       (opt (group-n 3 "*"))
                       (opt (and "[" (opt (group-n 4 digit)) "]"))
                       (opt (and "[" (opt (group-n 5 digit)) "]"))
                       (opt (and "[" (opt (group-n 6 (any alnum "_.:|-"))) "]")))))
         (f-rx (rx (group-n 1 "%f"
                            (opt (and "[" (group-n 2 digit) "]")))))
         (F-rx
          (rx (group-n 1 "%F"
                       (and "{" (group-n 2 (1+ letter)) "}")
                       (and "[" (group-n 3 digit) "]")
                       (opt (and "[" (group-n 4 digit) "]"))
                       (opt (and "[" (group-n 5 (any alnum "_.:|-")) "]")))))
         (e-rx (rx (group-n 1 "%e"
                            "{" (group-n 2 "(" (1+ ascii) ")") "}"))))
    ;; Evaluating sexps should go the first because they can produce
    ;; additional wildcards
    (while (string-match e-rx str)
      (setq str (replace-match
                 (eval (read (match-string 2 str)))
                 t nil str 1)))
    ;; Handle author wildcards
    (while (string-match a-rx str)
      (setq str (replace-match
                 (orb--autokey-format-field entry
                     :field "=name="
                     :capital (match-string 2 str)
                     :starred (match-string 3 str)
                     :words
                     (when (match-string 4 str)
                       (string-to-number (match-string 4 str)))
                     :characters
                     (when (match-string 5 str)
                       (string-to-number (match-string 5 str)))
                     :delimiter (match-string 6 str))
                 t nil str 1)))
    ;; Handle title wildcards
    (while (string-match t-rx str)
      (setq str (replace-match
                 (orb--autokey-format-field entry
                     :field "title"
                     :capital (match-string 2 str)
                     :starred (match-string 3 str)
                     :words
                     (when (match-string 4 str)
                       (string-to-number (match-string 4 str)))
                     :characters
                     (when (match-string 5 str)
                       (string-to-number (match-string 5 str)))
                     :delimiter (match-string 6 str))
                 t nil str 1)))
    ;; Handle year wildcards
    (while (string-match y-rx str)
      (setq str (replace-match
                 (format "%s" (bibtex-completion-get-value "date" entry))
                 t nil str 1)))
    ;; Handle pages wildcards
    ;; Handle custom field wildcards
    ;; debug
    (setq msorg-temp str)))

(defun orb--autokey-format-field (entry &rest specs)
  "Format field VALUE from BibTeX ENTRY FIELD according to SPECS.
%f{field}[3][2][-] - 2 first characters of first 3 words in field
field delimited with -, downcase.  This is."
  (-let* (((&plist :field field
                   :capital capital
                   :starred starred
                   :words words
                   :characters chars
                   :delimiter delim) specs)
          (separator "[ \n\t]")
          (delim (or delim ""))
          value result)
    ;; field "=name=" used internally in `orb-autokey-generate-key'
    ;; stands for author or editor
    (if (string= field "=name=")
        ;; in name fields the "punctuation" character
        ;; is the word "and"
        (progn
          (setq separator "and")
          (setq value (or (bibtex-completion-get-value "author" entry)
                          (bibtex-completion-get-value "editor" entry))))
      (setq value (bibtex-completion-get-value field entry)))
    (when value
      (save-match-data
        ;; split field into words
        (setq result (split-string value separator t "[ ,.;:-]+"))
        ;; only for title;
        ;; STARRED = include words from `orb-autokey-titlewords-ignore
        ;; "default" unstarred version filters the keywords
        (when (and (string= field "title")
                   (not starred))
          (let ((ignore-rx (concat "\\`\\(:?"
                                   (mapconcat #'identity
                                              orb-autokey-titlewords-ignore
                                              "\\|") "\\)\\'")))
            (setq result (-flatten (--map (if (string-match-p ignore-rx it)
                                              nil it)
                                          result)))))
        ;; take number of words equal to WORDS if that is set
        ;; or just the first word; also 0 = 1.
        (if (and words (> words 0))
            (setq result (if (> words (length result))
                             (-take (length result) result)
                           (-take words result) ))
          (setq result (list (car result))))
        ;; only for "=name=" field, i.e. author or editor
        ;; STARRED = include initials
        ;; NOTE: here we expect name field Doe, J. B.
        ;; should ideally be able to handle Doe, John M. Longname , Jr
        (if (and (string= field "=name=")
                 starred)
            (setq result (--map (s-replace-regexp "[ ,.\t\n]" "" it)
                                result))
          (setq result (--map (s-replace-regexp "\\(.*?\\),.*" "\\1" it)
                              result)))
        ;; take at most CHARS number of characters from every word
        (when chars
          (setq result (--map (substring it 0 (if (< chars (length it))
                                                  chars
                                                (length it)))
                              result)))
        ;; almost there: concatenate words, include DELIMiter
        (setq result (mapconcat #'identity result delim))
        ;; CAPITAL = preserve case
        (unless capital
          (setq result (downcase result)))))
    (or result "")))

;; debug
(orb-autokey-generate-key (gethash "paris1995a" msorg-key-hashtable))
(provide 'orb-core)
;;; orb-core.el ends here
;; Local Variables:
;; fill-column: 79
;; End:
