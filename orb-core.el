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

;; * Customize groups (global)
;; All modules should put their `defgroup' definitions here

(defgroup org-roam-bibtex nil
  "Org-ref and Bibtex-completion integration for Org-roam."
  :group 'org-roam
  :prefix "orb-")

(defgroup orb-note-actions nil
  "Orb Note Actions - run actions useful in note's context."
  :group 'org-roam-bibtex
  :prefix "orb-note-actions-")

;; Various utility functions

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

(defcustom orb-autokey-format "%a%y%T[4][1]"
  "Format string for automatically generated citation keys.

Supported wildcards:

Basic
==========

 %a         |author| - first author's (or editor's) last name
 %t         |title | - first word of title
 %f{field}  |field | - first word of arbitrary field
 %y         |year  | - year YYYY
 %p         |page  | - first page
 %e{(expr)} |elisp | - execute elisp expression

Extended
==========

1. Capitalized versions:

 %A        |author| >
 %T        |title | >  Same as %a,%t,%f{field} but
 %F{field} |field | >  preserve orignial capitalization

2. Starred versions

 %a*, %A* |author| - include author's (editor's) initials
 %t*, %T* |title | - do not ignore words in `orb-autokey-titlewords-ignore'
 %y*      |year  | - year's last two digits __YY
 %p*      |page  | - use \"pagetotal\" field instead of default \"pages\"

3. Optional parameters

 %a[N][M][D]        |author| >
 %t[N][M][D]        |title | > include first N words/names
 %f{field}[N][M][D] |field | > include at most M first characters of word/name
 %p[D]              |page  | > put delimiter D between words

N and M should be a single digit 1-9. Putting more digits or any
other symbols will lead to ignoring the optional parameter and
those following it altogether.  D should be a single alphanumeric
symbol or one of `-_.:|'.

Optional parameters work both with capitalized and starred
versions where applicable.

4. Elisp expression

 - can be anything
 - should return a string or nil
 - will be evaluated before expanding other wildcards and therefore
can insert other wildcards
 - will have `entry' variable bound to the value of BibTeX entry the key
is being generated for, as returned by `bibtex-completion-get-entry'.
The variable may be safely manipulated in a destructive manner.

%e{(or (bibtex-completion-get-value \\\"volume\\\" entry) \\\"N/A\\\")}
%e{(my-function entry)}

Key generation is performed by  `orb-autokey-generate-key'."
  :risky t
  :type 'string
  :group 'org-roam-bibtex)

(defcustom orb-autokey-titlewords-ignore (copy-tree bibtex-autokey-titleword-ignore)
  "Patterns from title that will be ignored during key generation.
Every element is a regular expression to match parts of the title
that should be ignored during automatic key generation.  Case
sensitive.

Default value is set from `bibtex-autokey-titleword-ignore'."
  :type '(repeat :tag "Regular expression" regexp)
  :group 'org-roam-bibtex)

(defun orb-autokey-generate-key (entry &optional control-string)
  "Generate citation key from ENTRY according to `orb-autokey-format'.
Return a string.  If optional CONTROL-STRING is non-nil, use it
instead of `orb-autokey-format'."
  (let* ((case-fold-search nil)
         (str (or control-string orb-autokey-format))
         ;; author wildcard regexp
         (a-rx
          (rx (group-n 1
                       (or "%a" (group-n 2 "%A"))
                       (opt (group-n 3 "*"))
                       (opt (and "[" (opt (group-n 4 digit)) "]"))
                       (opt (and "[" (opt (group-n 5 digit)) "]"))
                       (opt (and "[" (opt (group-n 6 (any alnum "_.:|-"))) "]")))))
         ;; title wildcard regexp
         (t-rx
          (rx (group-n 1
                       (or "%t" (group-n 2 "%T"))
                       (opt (group-n 3 "*"))
                       (opt (and "[" (opt (group-n 4 digit)) "]"))
                       (opt (and "[" (opt (group-n 5 digit)) "]"))
                       (opt (and "[" (opt (group-n 6 (any alnum "_.:|-"))) "]")))))
         ;; any field wildcard regexp
         (f-rx
          (rx (group-n 1
                       (or "%f" (group-n 2 "%F"))
                       (and "{" (group-n 3 (1+ letter)) "}")
                       (opt (and "[" (opt (group-n 4 digit)) "]"))
                       (opt (and "[" (opt (group-n 5 digit)) "]"))
                       (opt (and "[" (opt (group-n 6 (any alnum "_.:|-"))) "]")))))
         ;; year wildcard regexp
         (y-rx (rx (group-n 1 "%y" (opt (group-n 2 "*")))))
         ;; page wildcard regexp
         (p-rx
          (rx (group-n 1 "%p"
                       (opt (group-n 2 "*"))
                       (opt (and "[" (group-n 3 (any alnum "_.:|-")) "]")))))
         ;; elisp expression wildcard regexp
         (e-rx (rx (group-n 1 "%e"
                            "{" (group-n 2 "(" (1+ ascii) ")") "}"))))
    ;; Evaluating elisp expression should go the first because it can produce
    ;; additional wildcards
    (while (string-match e-rx str)
      (setq str (replace-match
                 (orb--autokey-evaluate-expression (match-string 2 str) entry)
                 t nil str 1)))
    ;; Handle author wildcards
    (let ((author (or (bibtex-completion-get-value "author" entry)
                      (bibtex-completion-get-value "editor" entry))))
      (while (string-match a-rx str)
        (setq str (replace-match
                   (orb--autokey-format-field nil
                       :field "=name="
                       :value author
                       :capital (match-string 2 str)
                       :starred (match-string 3 str)
                       :words (match-string 4 str)
                       :characters (match-string 5 str)
                       :delimiter (match-string 6 str))
                   t nil str 1))))
    ;; Handle title wildcards
    (let ((title (or (bibtex-completion-get-value "title" entry) "")))
      (while (string-match t-rx str)
        (setq str (replace-match
                   (orb--autokey-format-field nil
                       :field "title"
                       :value title
                       :capital (match-string 2 str)
                       :starred (match-string 3 str)
                       :words (match-string 4 str)
                       :characters (match-string 5 str)
                       :delimiter (match-string 6 str))
                   t nil str 1))))
    ;; Handle custom field wildcards
    (while (string-match f-rx str)
      (setq str (replace-match
                 (orb--autokey-format-field entry
                     :field (match-string 3 str)
                     :capital (match-string 2 str)
                     :words (match-string 4 str)
                     :characters (match-string 5 str)
                     :delimiter (match-string 6 str))
                 t nil str 1)))
    ;; Handle year wildcards
    ;; year should be well-formed: YYYY
    (let ((year (or (bibtex-completion-get-value "year" entry)
                    (bibtex-completion-get-value "date" entry)
                    "YYYY")))
      (while (string-match y-rx str)
        (setq str (replace-match
                   (format "%s" (if (match-string 2 str)
                                    (substring year 2 4)
                                  (substring year 0 4)))
                   t nil str 1))))
    ;; Handle pages wildcards
    (while (string-match p-rx str)
      (setq str (replace-match
                 (orb--autokey-format-field entry
                     :field (if (match-string 2 str) "pagetotal"
                              "pages")
                     :delimiter (match-string 3 str)
                     :words 1)
                 t nil str 1)))
    str))

(defun orb--autokey-format-field (entry &rest specs)
  "Format field of a BibTeX ENTRY according to plist SPECS.

Recognized keys:
==========
:field       - BibTeX field to use
:value       - Value of BibTeX field; it will be used
               instead the value from ENTRY
:capital     - capitalized version
:starred     - starred version
:words       - first optional parameter (number of words)
:characters  - second optional parameter (number of characters)
:delimiter   - third optional parameter (delimiter)

All values should be strings.

This function is used internally by `orb-autokey-generate-key'."
  (-let* (((&plist :field field
                   :value value
                   :capital capital
                   :starred starred
                   :words words
                   :characters chars
                   :delimiter delim) specs)
          ;; field values will be split into list of words. `separator' is a
          ;; regexp for word separators: either whitespaces, or at least two
          ;; dashes, or en dash, or em dash
          (separator "\\([ \n\t]\\|-[-]+\\|[—–]\\)")
          (delim (or delim ""))
          result)
    ;; field "=name=" used internally in `orb-autokey-generate-key'
    ;; stands for author or editor
    (if (string= field "=name=")
        ;; in name fields, logical words are full names consisting of several
        ;; words and containing spaces and punctuation, separated by a logical
        ;; separator, the word "and"
        (setq separator "and"
              value (or value
                        (bibtex-completion-get-value "author" entry)
                        (bibtex-completion-get-value "editor" entry)))
      (setq value (or value
                      (bibtex-completion-get-value field entry))))
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
        (if words
            (setq words (string-to-number words)
                  result (if (> words (length result))
                             (-take (length result) result)
                           (-take words result) ))
          (setq result (list (car result))))
        ;; only for "=name=" field, i.e. author or editor
        ;; STARRED = include initials
        (when (string= field "=name=")
          (if starred
              ;; NOTE: here we expect name field 'Doe, J. B.'
              ;; should ideally be able to handle 'Doe, John M. Longname, Jr'
              (setq result (--map (s-replace-regexp "[ ,.\t\n]" "" it)
                                  result))
            (setq result (--map (s-replace-regexp "\\(.*?\\),.*" "\\1" it)
                                result))))
        ;; take at most CHARS number of characters from every word
        (when chars
          (setq chars (string-to-number chars)
                result (--map (substring it 0 (if (< chars (length it))
                                                        chars
                                                      (length it)))
                                    result)))
        ;; almost there: concatenate words, include DELIMiter
        (setq result (mapconcat #'identity result delim))
        ;; CAPITAL = preserve case
        (unless capital
          (setq result (downcase result))))
      ;; return result or an empty string
      (or result ""))))

(defun orb--autokey-evaluate-expression (expr &optional entry)
  "Evaluate arbitrary elisp EXPR passed as readable string.
The expression will have value of ENTRY bound to `entry' variable
at its disposal.  ENTRY should be a BibTeX entry as returned by
`bibtex-completion-get-entry'.  The result returned should be a
string or nil."
  (let ((result (eval `(let ((entry (quote ,(copy-tree entry))))
                         ,(read expr)))))
    (unless (or (stringp result)
                (not result))
      (user-error "Result: %s, invalid type.  \
Expression must be string or nil" result))
    (or result "")))

(provide 'orb-core)
;;; orb-core.el ends here
;; Local Variables:
;; fill-column: 79
;; End:
