;;; orb-core.el --- Org Roam BibTeX: core library -*- lexical-binding: t -*-

;; Copyright © 2020-2021 Mykhailo Shevchuk
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
  "Org-ref and Bibtex-completion integration for Org-roam."
  :group 'org-roam
  :prefix "orb-")

(defgroup orb-note-actions nil
  "Orb Note Actions - run actions in note's context."
  :group 'org-roam-bibtex
  :prefix "orb-note-actions-")

(defgroup orb-pdf-scrapper nil
  "Orb PDF Scrapper - retrieve references from PDF."
  :group 'org-roam-bibtex
  :prefix "orb-pdf-scrapper-")

(defgroup orb-anystyle nil
  "Elisp interface to `anystyle-cli`."
  :group 'org-roam-bibtex
  :prefix "orb-anystyle-")

(defgroup orb-autokey nil
  "Automatic generation of BibTeX citation keys."
  :group 'org-roam-bibtex
  :prefix "orb-autokey-")


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

(defcustom orb-file-field-extensions '("pdf")
  "When processing the file field, keep file names only with these extensions.
This is a list of file extensions without a dot as case-insensitive
strings.

Set it to nil to keep all file names regardless of their extensions.

The name of the BibTeX file field is determined by
`bibtex-completion-pdf-field' and defaults to \"file\"."
  :group 'org-roam-bibtex
  :type '(repeat :tag "List of extensions" (string)))

(defcustom orb-abbreviate-file-name t
  "Non-nil to force abbreviation of file names by `orb-process-file-field'.

When this option is set to a non-nil value, the file name
returned by expanding the file keyword or looking up in
`bibtex-completion-library-path' will get the home directory part
abbreviated to '~/'.  Otherwise, the as-is value will be used,
which may or may not be abbreviated."
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

;;;###autoload
(defun orb-process-file-field (citekey)
  "Look up documents associated with the BibTeX entry and choose one.
Process the BibTeX 'file' field (`bibtex-completion-pdf-field')
or search in the `bibtex-completion-library-path' for a file or files with
the CITEKEY as filename sans extension.

If `orb-file-field-extensions' is non-nil, return only file paths
matching the respective extensions.

If `orb-abbreviate-file-name' is non-nil, force an abbreviated
file name.

Mendeley, Zotero, normal paths are all supported.  If there are
multiple files found, the user will be prompted to select one."
  (condition-case err
      (when-let* ((entry (bibtex-completion-get-entry citekey))
                  (paths
                   (--> (bibtex-completion-find-pdf entry)
                     (if (not orb-file-field-extensions)
                         it             ; do not filter by extensions
                       ;; filter by extensions
                       (--filter
                        (when-let ((ext (file-name-extension it)))
                          (member-ignore-case ext orb-file-field-extensions))
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
      (format "error in `orb-process-file-field`: %s %s"
              (car err) (cdr err))))))

;; ============================================================================
;;;; Orb autokey
;; ============================================================================

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
 %F{field} |field | >  preserve original capitalization

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

%e{(or (bibtex-completion-get-value \"volume\" entry) \"N/A\")}
%e{(my-function entry)}

Key generation is performed by  `orb-autokey-generate-key'."
  :risky t
  :type 'string
  :group 'org-roam-bibtex)

(defcustom orb-autokey-titlewords-ignore
  '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
    "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")
  "Patterns from title that will be ignored during key generation.
Every element is a regular expression to match parts of the title
that should be ignored during automatic key generation.  Case
sensitive."
  ;; Default value was take from `bibtex-autokey-titleword-ignore'.
  :type '(repeat :tag "Regular expression" regexp)
  :group 'orb-autokey)

(defcustom orb-autokey-empty-field-token "N/A"
  "String to use when BibTeX field is nil or empty."
  :type 'string
  :group 'orb-autokey)

(defcustom orb-autokey-invalid-symbols
  " \"'()={},~#%\\"
  "Characters not allowed in a BibTeX key.
The key will be stripped of these characters."
  :type 'string
  :group 'orb-autokey)

(defun orb--autokey-format-field (field &rest specs)
  "Return BibTeX FIELD formatted according to plist SPECS.

Recognized keys:
==========
:entry       - BibTeX entry to use
:value       - Value of BibTeX field to use
               instead retrieving it from :entry
:capital     - capitalized version
:starred     - starred version
:words       - first optional parameter (number of words)
:characters  - second optional parameter (number of characters)
:delimiter   - third optional parameter (delimiter)

All values should be strings, including those representing numbers.

This function is used internally by `orb-autokey-generate-key'."
  (declare (indent 1))
  (-let* (((&plist :entry entry
                   :value value
                   :capital capital
                   :starred starred
                   :words words
                   :characters chars
                   :delimiter delim) specs)
          ;; field values will be split into a list of words. `separator' is a
          ;; regexp for word separators: either a whitespace, one or more
          ;; dashes, or en dash, or em dash
          (separator "\\([ \n\t]\\|[-]+\\|[—–]\\)")
          (invalid-chars-rx
           (rx-to-string `(any ,orb-autokey-invalid-symbols) t))
          (delim (or delim ""))
          result)
    ;; 0. virtual field "=name=" is used internally here and in
    ;; `orb-autokey-generate-key'; it stands for author or editor
    (if (string= field "=name=")
        ;; in name fields, logical words are full names consisting of several
        ;; words and containing spaces and punctuation, separated by a logical
        ;; separator, the word "and"
        (setq separator " and "
              value (or value
                        (bibtex-completion-get-value "author" entry)
                        (bibtex-completion-get-value "editor" entry)))
      ;; otherwise proceed with value or get it from entry
      (setq value (or value
                      (bibtex-completion-get-value field entry))))
    (if (or (not value)
            (string-empty-p value))
        (setq result orb-autokey-empty-field-token)
      (when (> (length value) 0)
        (save-match-data
          ;; 1. split field into words
          (setq result (split-string value separator t "[ ,.;:-]+"))
          ;; 1a) only for title;
          ;; STARRED = include words from `orb-autokey-titlewords-ignore
          ;; unstarred version filters the keywords, starred ignores this block
          (when (and (string= field "title")
                     (not starred))
            (let ((ignore-rx (concat "\\`\\(:?"
                                     (mapconcat #'identity
                                                orb-autokey-titlewords-ignore
                                                "\\|") "\\)\\'"))
                  (words ()))
              (setq result (dolist (word result (nreverse words))
                             (unless (string-match-p ignore-rx word)
                               (push word words))))))
          ;; 2. take number of words equal to WORDS if that is set
          ;; or just the first word; also 0 = 1.
          (if words
              (setq words (string-to-number words)
                    result (-take (if (> words (length result))
                                      (length result)
                                    words)
                                  result))
            (setq result (list (car result))))
          ;; 2a) only for "=name=" field, i.e. author or editor
          ;; STARRED = include initials
          (when (string= field "=name=")
            ;; NOTE: here we expect name field 'Doe, J. B.'
            ;; should ideally be able to handle 'Doe, John M. Longname, Jr'
            (let ((r-x (if starred
                           "[ ,.\t\n]"
                         "\\`\\(.*?\\),.*\\'"))
                  (rep (if starred "" "\\1"))
                  (words ()))
              (setq result
                    (dolist (name result (nreverse words))
                      (push (s-replace-regexp r-x rep name) words)))))
          ;; 3. take at most CHARS number of characters from every word
          (when chars
            (let ((words ()))
              (setq chars (string-to-number chars)
                    result (dolist (word result (nreverse words))
                             (push
                              (substring word 0
                                         (if (< chars (length word))
                                             chars
                                           (length word)))
                              words)))))
          ;; 4. almost there: concatenate words, include DELIMiter
          (setq result (mapconcat #'identity result delim))
          ;; 5. CAPITAL = preserve case
          (unless capital
            (setq result (downcase result))))))
    ;; return result stripped of the invalid characters
    (s-replace-regexp invalid-chars-rx "" result t)))

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

;;;###autoload
(defun orb-autokey-generate-key (entry &optional control-string)
  "Generate citation key from ENTRY according to `orb-autokey-format'.
Return a string.  If optional CONTROL-STRING is non-nil, use it
instead of `orb-autokey-format'."
  (let* ((case-fold-search nil)
         (str (or control-string orb-autokey-format))
         ;; star regexp: group 3!
         (star '(opt (group-n 3 "*")))
         ;; optional parameters: regexp groups 4-6!
         (opt1 '(opt (and "[" (opt (group-n 4 digit)) "]")))
         (opt2 '(opt (and "[" (opt (group-n 5 digit)) "]")))
         (opt3 '(opt (and "[" (opt (group-n 6 (any alnum "_.:|-"))) "]")))
         ;; capital letters: regexp group 2!
         ;; author wildcard regexp
         (a-rx (macroexpand
                `(rx (group-n 1 (or "%a" (group-n 2 "%A"))
                              ,star ,opt1 ,opt2 ,opt3))))
         ;; title wildcard regexp
         (t-rx (macroexpand
                `(rx (group-n 1 (or "%t" (group-n 2 "%T"))
                              ,star ,opt1 ,opt2 ,opt3))))
         ;; any field wildcard regexp
         ;; required parameter: group 7!
         (f-rx (macroexpand
                `(rx (group-n 1 (or "%f" (group-n 2 "%F"))
                              (and "{" (group-n 7 (1+ letter)) "}")
                              ,opt1 ,opt2 ,opt3))))
         ;; year wildcard regexp
         (y-rx (rx (group-n 1 "%y" (opt (group-n 3 "*")))))
         ;; page wildcard regexp
         (p-rx (macroexpand `(rx (group-n 1 "%p" ,star ,opt3))))
         ;; elisp expression wildcard regexp
         ;; elisp sexp: group 8!
         (e-rx (rx (group-n 1 "%e"
                            "{" (group-n 8 "(" (1+ ascii) ")") "}"))))
    ;; Evaluating elisp expression should go the first because it can produce
    ;; additional wildcards
    (while (string-match e-rx str)
      (setq str (replace-match
                 (save-match-data
                   (orb--autokey-evaluate-expression
                    (match-string 8 str) entry)) t nil str 1)))
    ;; Expanding all other wildcards are actually
    ;; variations of calls to `orb--autokey-format-field' with many
    ;; commonalities, so we wrap it into a macro
    (cl-macrolet
        ((expand
          (wildcard &key field value entry capital
                    starred words characters delimiter)
          (let ((cap (or capital '(match-string 2 str)))
                (star (or starred '(match-string 3 str)))
                (opt1 (or words '(match-string 4 str)))
                (opt2 (or characters '(match-string 5 str)))
                (opt3 (or delimiter '(match-string 6 str))))
            `(while (string-match ,wildcard str)
               (setq str (replace-match
                          ;; we can safely pass nil values
                          ;; `orb--autokey-format-field' should
                          ;; handle them correctly
                          (orb--autokey-format-field ,field
                            :entry ,entry :value ,value
                            :capital ,cap :starred ,star
                            :words ,opt1 :characters ,opt2 :delimiter ,opt3)
                          t nil str 1))))))
      ;; Handle author wildcards
      (expand a-rx
              :field "=name="
              :value (or (bibtex-completion-get-value "author" entry)
                         (bibtex-completion-get-value "editor" entry)))
      ;; Handle title wildcards
      (expand t-rx
              :field "title"
              :value (or (bibtex-completion-get-value "title" entry) ""))
      ;; Handle custom field wildcards
      (expand f-rx
              :field (match-string 7 str)
              :entry entry)
      ;; Handle pages wildcards %p*[-]
      (expand p-rx
              :field (if (match-string 3 str)
                         "pagetotal" "pages")
              :entry entry
              :words "1"))
    ;; Handle year wildcards
    ;; it's simple, so we do not use `orb--autokey-format-field' here
    ;; year should be well-formed: YYYY
    ;; TODO: put year into cl-macrolet
    (let ((year (or (bibtex-completion-get-value "year" entry)
                    (bibtex-completion-get-value "date" entry))))
      (if (or (not year)
              (string-empty-p year)
              (string= year orb-autokey-empty-field-token))
          (while (string-match y-rx str)
            (setq str (replace-match orb-autokey-empty-field-token
                                     t nil str 1)))
        (while (string-match y-rx str)
          (setq year (format "%04d" (string-to-number year))
                str (replace-match
                     (format "%s" (if (match-string 3 str)
                                      (substring year 2 4)
                                    (substring year 0 4)))
                     t nil str 1)))))
    str))

(provide 'orb-core)
;;; orb-core.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
