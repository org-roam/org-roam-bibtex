;;; orb-pdf-scrapper.el --- Orb Roam BibTeX: PDF reference scrapper -*- lexical-binding: t -*-

;; Copyright © 2020-2021 Mykhailo Shevchuk

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
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

;;; Code:

;; ============================================================================
;;; Dependencies
;; ============================================================================

(require 'orb-core)
(require 'orb-anystyle)

;; it's fine here since `orb-pdf-scrapper' is autoloaded
(require 'bibtex-completion)

(require 'bibtex)
(require 'rx)
(require 'cl-extra)

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-macs)
  (require 'subr-x))

(declare-function bibtex-set-field "org-ref" (field value &optional nodelim))


;; ============================================================================
;;; Customize definitions
;; ============================================================================

(defcustom orb-pdf-scrapper-prompt-to-generate-keys 'when-buffer-modified
  "Prompt the user to generate keys in the BibTeX buffer?
After having finished editing in the BibTeX buffer and before
proceeding to Org buffer, the user will be prompted
to (re-)generate citation keys according to the value of this
option on these occasions:

- symbol `when-buffer-modified' - only when the buffer has
  modified and changes have not been saved
- nil - never
- t or any other value - always."
  :group 'orb-pdf-scrapper
  :type '(choice
          (const when-buffer-modified)
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom orb-pdf-scrapper-grouped-export
  '((in-roam "In Org Roam database" list)
    (in-bib "In BibTeX file" list)
    (valid "Valid citation keys" table)
    (invalid "Invalid citation keys" table))
  "Determines appearence of grouped references in Org view.
A list of five elements of the form (GROUP TITLE TYPE).

GROUP must be one of the symbols `parent', `in-roam', `in-bib',
`valid' or `invalid'.

TITLE is an arbitrary string, which will be the title of the
group's headline.

TYPE must be one of the symbols `list' or `table' determining how
the generated citations will appear under the group's headline.
TYPE is ignored for the `parent' group and defaults to `list' for
other groups when set to nil.

Takes effect when `orb-pdf-scrapper-group-references' is t."
  :type '(list (list :tag "\nIn-roam"
                     (const :format "" in-roam)
                     (string :tag "Title")
                     (radio :tag "Type" :value list
                            (const list) (const table)))
               (list :tag "\nIn-bib"
                     (const :format "" in-bib)
                     (string :tag "Title")
                     (radio :tag "Type" :value list
                            (const list) (const table)))
               (list :tag "\nValid"
                     (const :format "" valid)
                     (string :tag "Title")
                     (radio :tag "Type" :value table
                            (const list) (const table)))
               (list :tag "\nInvalid"
                     (const :format "" invalid)
                     (string :tag "Title")
                     (radio :tag "Type" :value table
                            (const list) (const table))))
  :group 'orb-pdf-scrapper)

(defcustom orb-pdf-scrapper-ungrouped-export 'list
  "Determines appearence of ungrouped references in Org view.
Valid values are the symbols `list' and `table'.

Takes effect when `orb-pdf-scrapper-group-references' is nil."
  :group 'orb-pdf-scrapper
  :type '(radio
          (const list)
          (const table)))

(defcustom orb-pdf-scrapper-table-export-fields
  '("#" "citekey" "author" "editor" "journal"
    "date" "volume" "pages")
  "BibTeX fields for export into Org mode tables in the Org view.
A list in which each element is of form FIELD or (FIELD . TYPES).
The order of items in this list determines the order of table
columns.

FIELD is a field to export.  Field mapping according to the value
of `orb-bibtex-field-aliases' is recognized.  The non-standard
BibTeX field `citation-number' created during the reference
extraction process is treated specially according to the value of
the variable `orb-pdf-scrapper-reference-numbers'.

TYPES is a list of strings corresponding to BibTeX entry types
for which to export the FIELD.  E.g. a value of a list cell

\(\"editor\" . \"collection\")

means to export the value of the field \"editor\" only for
entries whose entry type is \"collection\".  If it is
nil (default), export the FIELD for all entry types.

See also the variables `orb-pdf-scrapper-grouped-export' and
`orb-pdf-scrapper-ungrouped-export', which allow to choose between
list and table export."
  :type '(repeat (string :tag "Field"))
  :group 'orb-pdf-scrapper)

(defcustom orb-pdf-scrapper-group-references t
  "If this is non-nil, group the retrieved references in the Org view.
These groups are `in-roam', `in-bib', `valid' and `invalid'."
  :group 'orb-pdf-scrapper
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom orb-pdf-scrapper-list-style 'unordered-hyphen
  "Format of Org lists produced by ORB PDF Scrapper.
Valid values are symbols:
`ordered-point'       => 1. citation
`ordered-parenthesis' => 1) citation
`unordered-hyphen'    => - citation
`unordered-plus'      => + citation
`unordered-asterisk'  => * citation

This variable can also take a string value, in which case the
string should be a valid format string containing the '%s'
specifier, which will be used to insert citation numbers, for
example:

\"- [%s] \"            => - [1] citation

The format string should take care of padding as no additional
spaces will be inserted automatically.

The variable `orb-pdf-scrapper-reference-numbers' allows to
choose the numbering scheme for ordered and custom format lists."
  :group 'orb-pdf-scrapper
  :type
  '(choice
    (const :tag "Point-terminated ordered list" ordered-point)
    (const :tag "Parenthesis-terminated ordered list" ordered-parenthesis)
    (const :tag "Hyphen-led unordered list" unordered-hyphen)
    (const :tag "Plus-led unordered list" unordered-plus)
    (const :tag "Asterisk-led unordered list" unordered-asterisk)
    (string :tag "Custom ordered format")))

(defcustom orb-pdf-scrapper-reference-numbers 'citation-number
  "This variable specifies the source of ordered list numbers.
Valid values are:

`citation-number' - use the extracted `citation-number',
stripping off any non-numerical characters.  Fall back to
unordered hyphen-led list if this field is non-existent, empty,
or does not contain a number.

`citation-number-alnum' - use the extracted `citation-number',
stripping off any non-alphanumerical characters, fall back to
unordered hyphen-led list if this field is non-existent, empty or
does not contain alphanumerical characters.  This value will only
have effect when `orb-pdf-scrapper-list-style' is a custom format
string and behave as `citation-number' otherwise.

`as-retrieved' - use the natural order of BibTeX entry in the buffer

If the value of `orb-pdf-scrapper-list-style' is one of the
'unordered' choices, this variable will have no effect."
  :group 'orb-pdf-scrapper
  :type '(radio
          (const :tag "Citation number" citation-number)
          (const :tag "Alphanumerical citation number" citation-number-alnum)
          (const :tag "As retrieved" as-retrieved)))

(defcustom orb-pdf-scrapper-citekey-format "cite:%s"
  "Format of the citekey for insertion into Org mode buffer."
  :group 'orb-pdf-scrapper
  :type 'string)

(defcustom orb-pdf-scrapper-export-options
  '((org (heading "References (extracted by ORB PDF Scrapper)"
                   :property-drawer (("PDF_SCRAPPER_TYPE")
                                     ("PDF_SCRAPPER_SOURCE")
                                     ("PDF_SCRAPPER_DATE")))))
  "Options for automatic export of references extracted by ORB PDF Scrapper.
This variable is an association list of the form
\(TYPE . ((TARGET LOCATION PROPERTIES))).

TYPE is the type of the exported data, one of the symbols `txt',
`bib' or `org'.  The data will only be exported if its
corresponding symbol is present on the list.

TARGET must be one of the symbols `heading' or `path'.  The
symbol `heading' means export the data under a heading in the
buffer of origin, the Org-mode buffer where the ORB PDF Scrapper
process was started.  The symbol `path' means export the data to
another file.  It is possible to specify both export targets
simultaneously for a given export TYPE or multiple targets of the
same type.

Example:
\(setq orb-pdf-scrapper-export-options
      '((org (heading HEADLINE PROPERTIES)
             (path LOCATION PROPERTIES))))

LOCATION (HEADLINE) is a string specifying location of the TARGET.

- If TARGET is `heading', the supplied string will be used as
headline text.  The data will be exported slightly differently
depending on TYPE.  Text references will be exported as is.
BibTeX references will be put into an Org-mode source code block.
Org-mode references, if grouped under different headings, will be
exported with the headings demoted by one level.

- If TARGET is `path', the supplied string will be used as a
filesystem target.  The path can be absolute or relative, in the
latter case it will be relative to the directory of the buffer of
origin.  If the path (absolute or relative) is an *existing*
directory, the full path to the target file will be constructed
from the supplied string as a directory name, the #+ROAM_KEY:
property in the buffer of origin as a file name and TYPE as the
file's extension.  If the path is not an existing directory, it
will be treated as a file name, and the data will be exported
there.  The file will be created if it does not exist:

> \"~/org/references.org\"           - absolute path
> \"this-note's-bib-references.bib\" - path relative to the buffer of origin
> \"~/orb-pdf-scrapper-references/\" - absolute directory path.

In the latter case, if the directory exists, the extracted data
will be put into a file \"Doe2020.bib\", assuming the #+ROAM_KEY:
property is \"Doe2020\" and TYPE is `bib'.  If the directory does
not exist, the extracted data will be put into a (newly created)
file \"~/orb-pdf-scrapper-references/\".

Example:
\(setq orb-pdf-scrapper-export-options
      '((org (heading \"Org references\" PROPERTIES))
        (txt (path \"text-references/\" PROPERTIES))))

PROPERTIES is a property list providing additional export
specifications.  Some properties are specific to only certain
export TYPEs or TARGETs.

`:placement' property allows to specify placement of the exported
data.  It can be a symbol `prepend' or `append'.

When TARGET is `path', text data is simply put at the beginning
or end of the target file accordingly to the value of the
`:placement' property.  Org-mode data is placed before the first
or after the last heading, respectively.  Similarly, BibTeX data
is placed or before the first or after the last entry, comments
and @String entries ignored.

When TARGET is `heading', this property specifies whether the
parent heading should be put before or after other headings.

When TARGET is `path' and LOCATION is an Org-mode file, the value
of the `:placement' property can also be a list of the form
\(heading HEADLINE PROPERTIES).  In this case the data will be
put in the target file under a heading with HEADLINE as the
headline text.  PROPERTIES are additional export options as
described here and below.  The `heading' value of the
`:placement' property cannot be used recursively in this case.

Example:
\(setq orb-pdf-scrapper-export-options
      '((org (path \"references.org\" :placement append))
        (bib (path \"references.bib\" :placement prepend))
        (txt (path \"references.txt\"
                   :placement (headline \"References\" :placement append)))))

If placement is not specified, the data is appended by default.

`:property-drawer' property allows to supply a heading with some
properties.  The value of this property is list with
elements (PROPERTY_NAME . PROPERTY_VALUE) or PROPERTY_NAME, the
latter form being treated as (PROPERTY_NAME . nil).

PROPERTY_NAME must be a string, it will be used as a property
name.  PROPERY_VALUE can be a string, in which case it will be
used as the value of the property.  It can also be a function
name as an unquoted symbol, in which case this function will be
called to get the value of the property.  The return value must
be a string.

The following properties are recognized internally and will be
supplied with automatically generated values if PROPERTY_VALUE is
nil:

> PDF_SCRAPPER_TYPE   - TYPE of the export data
> PDF_SCRAPPER_SOURCE - name of the PDF file the data were extracted from
> PDF_SCRAPPER_DATE   - time and date the data were exported

Example:
\(setq orb-pdf-scrapper-export-options
      '((org (heading \"Org references\" PROPERTIES)))
        (txt (heading \"Text references\"
                       :property-drawer
                       '((\"PDF_SCRAPPER_TYPE\" . \"text\")
                         \"PDF_SCRAPPER_DATE\"
                         \"PDF_SCRAPPER_SOURCE\"
                         (\"PROPERTY_1\" . \"VALUE_1\")
                         (\"PROPERTY_2\" . my-function))))))

`:filter-bib-entries' property controls filtering of exported
BibTeX entries.  If the value of this property is non-nil and
TARGET is a BibTeX file, only the entries that are not already
present in this file will be exported.  The value can also be a
string or a list of strings specifying BibTeX file(s), or a
variable as an unquoted symbol holding a string or a list of
strings specifying BibTeX file(s), in which cases the entries
will be filtered also against this/these file(s) in *addition* to
the TARGET file.  In such instances, filtering will also be
applied to entries exported to an Org-mode heading.

Example:
\(setq orb-pdf-scrapper-export-options
      '((bib (path \"references.bib\" :filter-bib-entries t
             (heading \"BibTeX references\"
                       :filter-bib-entries bibtex-completion-bibliography))))."

  :group 'orb-pdf-scrapper
  :risky t
  :type '(repeat list))

(defcustom orb-pdf-scrapper-set-fields
  '(("author" orb-pdf-scrapper--invalidate-nil-value)
    ("editor" orb-pdf-scrapper--invalidate-nil-value
     "book" "collection")
    ("title" orb-pdf-scrapper--invalidate-nil-value)
    ("journal" orb-pdf-scrapper--invalidate-nil-value
     "article")
    ("date" orb-pdf-scrapper--invalidate-nil-value)
    ("volume" orb-pdf-scrapper--invalidate-nil-value
     "article" "incollection")
    ("pages" orb-pdf-scrapper--fix-or-invalidate-range
     "article" "incollection"))
  "BibTeX fields to set during key generation.
A list in which each element is a list of the form (FIELD FUNCTION . TYPES).

FIELD is a BibTeX field name to be set.

FUNCTION is a function that will be called to generate the value,
it takes one argument ENTRY, which is the current entry.

TYPES is a list of strings corresponding to BibTeX entry types
for which the FIELD should be set.  If it is nil, set the FIELD
for all entry types."
  :risky t
  :type '(repeat
          (list :tag "Item"
                (string :tag "Field")
                (function :tag "Function")
                (repeat :tag "Entry types" :inline t
                        (string :tag "Type"))))
  :group 'orb-pdf-scrapper)

(defcustom orb-pdf-scrapper-invalid-key-pattern "\\`.*N/A.*\\'"
  "Regexp to match an invalid key."
  :type 'regexp
  :group 'orb-pdf-scrapper)


;; ============================================================================
;;; Helper functions: BibTeX buffer and citekey-related routines
;; ============================================================================

(defvar orb-pdf-scrapper--refs nil
  "Internal list with cell format (CITEKEY ENTRY . VALIDP).")

(defun orb-pdf-scrapper--invalidate-nil-value (field entry)
  "Return value of FIELD or `orb-autokey-empty-field-token' if it is nil.
ENTRY is a BibTeX entry."
  (bibtex-completion-get-value field entry orb-autokey-empty-field-token))

(defun orb-pdf-scrapper--fix-or-invalidate-range (field entry)
  "Replace missing or non-standard delimiter between two strings with \"--\".
FIELD is the name of a BibTeX field from ENTRY.  Return
`orb-autokey-empty-field-token' if the value is nil.

This function is primarily intended for fixing anystyle parsing
artefacts such as those often encountered in \"pages\" field,
where two numbers have only spaces between them."
  (replace-regexp-in-string "\\`[[:alnum:]]*?\\([- –]+\\)[[:alnum:]]*\\'"
                            "--"
                            (bibtex-completion-get-value
                             field entry orb-autokey-empty-field-token)
                            nil nil 1))

(defun orb-pdf-scrapper--get-entry-info (entry &optional collect-only)
  "Collect some information from and about the BibTeX ENTRY for further use.
Take a bibtex entry as returned by `bibtex-completion-get-entry' \
and return a plist with the following keys set:

:key            |string | citekey generated with `orb-autokey-generate-key'
:validp         |boolean| according to `orb-pdf-scrapper-invalid-key-pattern'
:set-fields     |(cons) | as per `orb-pdf-scrapper-set-fields'

Each element of `:set-fields' list is a a cons cell (FIELD . VALUE).

If optional COLLECT-ONLY is non-nil, do not generate the key,
`:set-fields' is set to nil."
  (let ((type (bibtex-completion-get-value "=type=" entry))
        ;; return values
        key validp fields-to-set
        ;; internal variable
        fields)
    ;; when requested to collect keys, just do that
    (if collect-only
        (setq key (bibtex-completion-get-value "=key=" entry)
              fields entry)
      ;; otherwise
      ;; prepare fields for setting
      (dolist (field-to-set orb-pdf-scrapper-set-fields)
        (let ((field-name (car field-to-set))
              (types-to-set (cddr field-to-set)))
          ;; push the field for setting only when entry type is one of the
          ;; specified types or nil, which means set the field regardless of
          ;; entry type
          (when (or (not types-to-set)
                    (member type types-to-set))
            (push (cons field-name
                        ;; call the function if provided
                        (if-let ((fn (cadr field-to-set)))
                            (funcall fn field-name entry)
                          ;; otherwise get the value from current entry
                          (bibtex-completion-get-value field-name entry "")))
                  fields-to-set))))
      ;; prioritize fields from fields-to-set over entry fields
      ;; for autokey generation
      (let ((-compare-fn (lambda (x y)
                           (string= (car x) (car y)))))
        (setq fields (-union fields-to-set entry)
              key (orb-autokey-generate-key fields))))
    ;; validate the new shiny key (or the old existing one)
    ;; not sure if save-match-data is needed here
    ;; but it seems to be always a good choice
    (save-match-data
      (setq validp (and (not (string-match-p
                              orb-pdf-scrapper-invalid-key-pattern key))
                        t)))
    ;; return the entry
    (list :key key
          :validp validp
          :set-fields fields-to-set)))

(defun orb-pdf-scrapper--update-record-at-point
    (&optional collect-only natural-order)
  "Generate citation key and update the BibTeX record at point.
Calls `orb-pdf-scrapper--get-entry-info' to get information about
BibTeX record at point and updates it accordingly.  If optional
COLLECT-ONLY is non-nil, do not generate the key and do not set
the fields.

If optional argument NATURAL-ORDER is non-nil, set the field
'natural-order' of the returned entry to its value.

This is an auxiliary function for command
`orb-pdf-scrapper-generate-keys'."
  (let* ((entry (parsebib-read-entry (parsebib-find-next-item)))
         (key-plist (orb-pdf-scrapper--get-entry-info entry collect-only))
         (new-key (plist-get key-plist :key))
         (validp (plist-get key-plist :validp))
         (fields-to-set (plist-get key-plist :set-fields)))
    (unless collect-only
      (save-excursion
        ;; update citekey
        ;; adjusted from bibtex-clean-entry
        (bibtex-beginning-of-entry)
        (re-search-forward bibtex-entry-maybe-empty-head)
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))
        (insert new-key)
        ;; set the bibtex fields
        (when fields-to-set
          (dolist (field fields-to-set)
            (bibtex-set-field (car field) (cdr field))))))
    ;; return the result ((NEW-KEY . ENTRY) . VALIDP)
    ;; TODO: for testing until implemented
    (when natural-order
      (cl-pushnew `("natural-order" . ,natural-order) entry))
    (cons new-key (cons entry validp))))

(defun orb-pdf-scrapper--sort-refs (refs)
  "Sort references REFS.
Auxiliary function for `orb-pdf-scrapper-generate-keys'.
REFS should be an alist of form ((CITEKEY . FORMATTED-ENTRY) . VALIDP).

References validated by `orb-pdf-scrapper-keygen-function' function
are further sorted into four groups:

'in-roam - available in the `org-roam' database;
'in-bib  - available in `bibtex-completion-bibliography' file(s);
'valid   - marked valid by the keygen function but are not
           available in user database(s);
'invalid - marked invalid by the keygen function."
  (let* ((bibtex-completion-bibliography (orb-pdf-scrapper--get :global-bib))
         ;; When using a quoted list here, sorted-refs is not erased in
         ;; consecutive runs
         (sorted-refs (list (list 'in-roam) (list 'in-bib)
                            (list 'valid) (list 'invalid))))
    (dolist (ref refs)
      (cond ((org-roam-db-query [:select [ref]
                                 :from refs
                                 :where (= ref $s1)]
                                (format "%s" (car ref)))
             (push ref (cdr (assoc 'in-roam sorted-refs))))
            ((bibtex-completion-get-entry (car ref))
             (push ref (cdr (assoc 'in-bib sorted-refs))))
            ((cddr ref)
             (push ref (cdr (assoc 'valid sorted-refs))))
            (t
             (push ref (cdr (assoc 'invalid sorted-refs))))))
    sorted-refs))


;; ============================================================================
;;; Helper functions: Org buffer-related routines
;; ============================================================================

(defun orb-pdf-scrapper--get-reference-number
    (entry &optional numbering-source)
  "ENTRY NUMBERING-SOURCE."
  (let ((numbering-source
         (or numbering-source orb-pdf-scrapper-reference-numbers)))
    (cl-case numbering-source
      (citation-number
       (--> (bibtex-completion-get-value "citation-number" entry nil)
            (when (and it (string-match ".*?\\([0-9]+\\).*?" it))
              (match-string 1 it))))
      (citation-number-alnum
       (--> (bibtex-completion-get-value "citation-number" entry nil)
         (when (and it (string-match "\
[^[:alnum:]]?\\([[:digit:]]*\\)\\([^[:alnum:]]*\\)\\([[:alpha:]]*\\)" it))
              (concat (match-string 1 it) (match-string 3 it)))))
      (as-retrieved
       (bibtex-completion-get-value "natural-order" entry ""))
      (t (user-error "Unsupported reference numbers source: %s"
                     numbering-source)))))

(defun orb-pdf-scrapper--insert-org-as-list (ref-alist)
  "Insert REF-ALIST as Org-mode list."
  (let* ((numbering-source
          (if (and (eq orb-pdf-scrapper-reference-numbers
                       'citation-number-alnum)
                   (not (stringp orb-pdf-scrapper-list-style)))
              'citation-number
            orb-pdf-scrapper-reference-numbers))
         (leader
          (cl-case orb-pdf-scrapper-list-style
            (ordered-point "%s. ")
            (ordered-parenthesis "%s) ")
            (unordered-hyphen "- ")
            (unordered-plus "+ ")
            (unordered-asterisk "* ")
            (t (if (stringp orb-pdf-scrapper-list-style)
                   orb-pdf-scrapper-list-style
                 (user-error "ORB: Unrecognized list style %s requested"
                             orb-pdf-scrapper-list-style)))))
         (unorderedp
          (memq orb-pdf-scrapper-list-style
                '(unordered-hyphen unordered-plus unordered-asterisk)))
         (fallback (if unorderedp leader "- ")))
    (dolist (ref ref-alist)
      (let* ((citekey (format orb-pdf-scrapper-citekey-format (car ref)))
             (entry (cadr ref))
             (number (unless unorderedp
                       (orb-pdf-scrapper--get-reference-number
                        entry numbering-source))))
        (insert (orb-format leader `(,number . ,fallback)) citekey "\n")))))

(defun orb-pdf-scrapper--get-export-value (field entry)
  "Get FIELD value from ENTRY.
Similar to `bibtex-completion-get-value' but does some additional cleaning."
  ;; list fields for org export
  (let* ((field (or (car (rassoc field orb-bibtex-field-aliases))
                    field))
         (value (bibtex-completion-get-value field entry "")))
    ;; truncate author list to first three names, append et.al instead
    ;; of the remaining names
    ;; This is a hard-coded "reasonable default"
    ;; and it may be replaced with something more
    ;; flexible in the future
    (cond
     ((member field '("author" "editor"))
      (--> value
           (split-string it " and " t "[ ,.;:-]+")
           (if (> (length it) 3)
               (append (-take 3 it) '("et.al."))
             it)
           (concat (mapconcat #'identity it "; "))))
     ((string= field "citation-number")
      (orb-pdf-scrapper--get-reference-number entry))
     ((string= field "=key=")
       (format orb-pdf-scrapper-citekey-format value))
     (t value))))

(defun orb-pdf-scrapper--insert-org-as-table (ref-alist)
  "Insert REF-ALIST as Org-mode table."
  (insert
   (format "|%s\n" (mapconcat #'identity
                              orb-pdf-scrapper-table-export-fields "|")))
  (forward-line -1)
  (org-table-insert-hline)
  (forward-line 2)
  (let ((table ""))
    (dolist (ref ref-alist)
      (setq table
            (format "%s|%s|\n" table
                    (mapconcat
                     (lambda (field)
                       (orb-pdf-scrapper--get-export-value field (cadr ref)))
                     orb-pdf-scrapper-table-export-fields "|"))))
    (insert table))
  (forward-line -1)
  (org-table-align))

(defun orb-pdf-scrapper--insert-refs ()
  "Insert the references list as org structure.
If `orb-pdf-scrapper-group-references' is non-nil, sort the references into
categories `in-roam', `in-bib', `valid', `invalid'.  Make a plain
list otherwise."
  (cond
   (orb-pdf-scrapper-group-references
    (dolist (ref-group
             (orb-pdf-scrapper--sort-refs orb-pdf-scrapper--refs))
      (when-let* ((group (car ref-group))
                  (refs (cdr ref-group))
                  (heading
                   (cdr (assoc group
                               orb-pdf-scrapper-grouped-export)))
                  (title (car heading))
                  (type (cadr heading))
                  (pos (make-marker)))
        ;; NOTE: Investigate
        ;; The behaviour of org-insert-heading has changed at some point:
        ;; If in an empty buffer, e.g. temp-buffer, the function fails messaging "beginning of buffer"
        (org-N-empty-lines-before-current 1)
        (org-insert-heading '(16) nil t)
        ;; insert heading
        (insert (format "%s\n" title))
        (org-N-empty-lines-before-current 1)
        ;; insert references
        (insert (format "#+name: %s\n" group))
        (set-marker pos (point))
        (set-marker-insertion-type pos t)
        (cl-case type
          ('table
           (orb-pdf-scrapper--insert-org-as-table refs))
          (t
           (orb-pdf-scrapper--insert-org-as-list refs)))
        (goto-char pos))))
   (t
    (insert "\n")
    (let ((refs (nreverse orb-pdf-scrapper--refs)))
      (cl-case orb-pdf-scrapper-ungrouped-export
        ('table
         (orb-pdf-scrapper--insert-org-as-table refs))
        (t
         (orb-pdf-scrapper--insert-org-as-list refs))))))
  (goto-char (point-max))
  (org-N-empty-lines-before-current 0))


;; ============================================================================
;;; Helper functions: Dispatcher
;; ============================================================================

(defvar orb-pdf-scrapper--plist nil
  "Communication channel for Orb PDF Scrapper.")

(defvar orb-pdf-scrapper--buffer "*Orb PDF Scrapper*"
  "Orb PDF Scrapper special buffer.")

(defmacro orb--with-scrapper-buffer! (&rest body)
  "Execute BODY with `orb-pdf-scrapper--buffer' as current.
If the buffer does not exist it will be created."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (set-buffer (get-buffer-create orb-pdf-scrapper--buffer))
     ,@body))

(defmacro orb--when-current-context! (context &rest body)
  "Execute BODY if CONTEXT is current context.
Run `orb-pdf-scrapper-keygen-function' with `error' context
otherwise.  If CONTEXT is a list then current context must be a
member of that list."
  (declare (indent 1) (debug t))
  `(if (not (orb-pdf-scrapper--current-context-p ,context))
       (orb-pdf-scrapper-dispatcher 'error)
     ,@body))

(defun orb-pdf-scrapper--current-context-p (context)
  "Return t if CONTEXT is current context.
CONTEXT can also be a list, in which case t is returned when
current context is its memeber."
  (if (listp context)
      (memq (orb-pdf-scrapper--get :context) context)
    (eq (orb-pdf-scrapper--get :context) context)))

(defun orb-pdf-scrapper--refresh-mode (mode)
  "Restart `orb-pdf-scrapper-mode' with new major MODE."
  (cl-case mode
    ('txt
     (text-mode)
     (orb-pdf-scrapper--put :callee 'edit-bib
                            :context 'start
                            :caller 'edit-txt))
    ('bib
     (bibtex-mode)
     ;; anystyle uses biblatex dialect
     (bibtex-set-dialect 'biblatex t)
     (orb-pdf-scrapper--put :callee 'edit-org
                            :context 'start
                            :caller 'edit-bib))
    ('org
     (org-mode)
     (orb-pdf-scrapper--put :callee 'checkout
                            :context 'start
                            :caller 'edit-org))
    ('xml
     (xml-mode)
     (cl-case (orb-pdf-scrapper--get :context)
       ;; since :callee is not used in training session, we set :callee here to
       ;; the original :caller, so that we can return to the editing mode we
       ;; were called from if the training session is to be cancelled
       ('start
        (orb-pdf-scrapper--put :callee (orb-pdf-scrapper--get :caller)
                               :context 'edit
                               :caller 'edit-xml))))
    ('train
     (fundamental-mode)
     (cl-case (orb-pdf-scrapper--get :context)
       ('train
        (orb-pdf-scrapper--put :context 'train
                               :caller 'train))
       ;; Since the session was not cancelled, we return to text, as everything
       ;; else should be regenerated anyway.
       ('finished
        (orb-pdf-scrapper--put :callee 'edit-txt
                               :context 'continue
                               :caller 'train))))
    (t
     (unwind-protect
         (error "Oops...something went wrong.  \
Pressing the RED button, just in case")
       (orb-pdf-scrapper-dispatcher 'error))))
  (set-buffer-modified-p nil)
  (setq mark-active nil)
  (orb-pdf-scrapper-mode -1)
  (orb-pdf-scrapper-mode +1)
  (goto-char (point-min)))

(defun orb-pdf-scrapper--edit-txt ()
  "Edit text references in `orb-pdf-scrapper--buffer'."
  ;; callee will be overridden in case of error
  (cl-case (orb-pdf-scrapper--get :context)
    ;; parse pdf file and switch to text editing mode
    ('start
     (let ((temp-txt (orb-temp-file "orb-pdf-scrapper-" ".txt"))
           (pdf-file (orb-pdf-scrapper--get :pdf-file)))
       (orb-pdf-scrapper--put :temp-txt temp-txt)
       (let ((same-window-buffer-names (list orb-pdf-scrapper--buffer)))
         (pop-to-buffer orb-pdf-scrapper--buffer))
       (setq buffer-file-name nil)
       (orb--with-message! (format "Scrapping %s.pdf" (f-base pdf-file))
         (erase-buffer)
         (orb-anystyle 'find
           :format 'ref
           :layout nil
           :finder-model orb-anystyle-finder-model
           :input pdf-file
           :stdout t
           :buffer orb-pdf-scrapper--buffer))
       (setq buffer-undo-list nil)
       (orb-pdf-scrapper--refresh-mode 'txt)))
    ;; read the previously generated text file
    ('continue
     (if-let ((temp-txt (orb-pdf-scrapper--get :temp-txt))
              (f-exists? temp-txt))
         (progn
           (pop-to-buffer orb-pdf-scrapper--buffer)
           (erase-buffer)
           (insert-file-contents temp-txt)
           (setq buffer-undo-list (orb-pdf-scrapper--get :txt-undo-list))
           (orb-pdf-scrapper--refresh-mode 'txt))
       (orb-pdf-scrapper-dispatcher 'error)))
    (t
     (orb-pdf-scrapper-dispatcher 'error))))

(defun orb-pdf-scrapper--edit-bib ()
  "Generate and edit BibTeX data in `orb-pdf-scrapper--buffer'."
  (pop-to-buffer orb-pdf-scrapper--buffer)
  (cl-case (orb-pdf-scrapper--get :context)
    ('start
     (let* ((temp-bib (or (orb-pdf-scrapper--get :temp-bib)
                          (orb-temp-file "orb-pdf-scrapper-" ".bib"))))
       (orb-pdf-scrapper--put :temp-bib temp-bib)
       ;; save previous progress in txt buffer
       (write-region (orb-buffer-string)
                     nil (orb-pdf-scrapper--get :temp-txt) nil -1)
       (orb-pdf-scrapper--put :txt-undo-list (copy-tree buffer-undo-list))
       (orb--with-message! "Generating BibTeX data"
         ;; Starting from Emacs 27, whether shell-command erases buffer
         ;; is controlled by `shell-command-dont-erase-buffer', so we
         ;; make sure the buffer is clean
         (erase-buffer)
         (orb-anystyle 'parse
           :format 'bib
           :parser-model orb-anystyle-parser-model
           :input (orb-pdf-scrapper--get :temp-txt)
           :stdout t
           :buffer orb-pdf-scrapper--buffer)
         (write-region (orb-buffer-string) nil temp-bib nil -1))
       (setq buffer-undo-list nil))
     (orb-pdf-scrapper--refresh-mode 'bib))
    ('continue
     (if-let ((temp-bib (orb-pdf-scrapper--get :temp-bib))
              (f-exists? temp-bib))
         (progn
           (erase-buffer)
           (insert-file-contents temp-bib)
           (setq buffer-undo-list (orb-pdf-scrapper--get :bib-undo-list))
           (orb-pdf-scrapper--refresh-mode 'bib))
       (orb-pdf-scrapper-dispatcher 'error)))
    (t
     (orb-pdf-scrapper-dispatcher 'error))))

(defun orb-pdf-scrapper--edit-org ()
  "Edit generated Org-mode data."
  (pop-to-buffer orb-pdf-scrapper--buffer)
  (cl-case (orb-pdf-scrapper--get :context)
    ('start
     ;; if the BibTeX buffer was modified, save it and maybe generate keys
     (orb-pdf-scrapper-generate-keys
      nil
      (cl-case orb-pdf-scrapper-prompt-to-generate-keys
        ('when-buffer-modified
         (if (buffer-modified-p)
             ;; TODO: it's clumsy
             ;; not "yes" means generate
             ;; not "no" means collect only
             (not (y-or-n-p "The buffer contents has changed.  \
Generate BibTeX keys? "))
           t))
        ;; do not prompt
        (nil t)
        ;; always prompt
        (t
         (not (y-or-n-p "Generate BibTeX keys? ")))))
     (when (> (cl-random 100) 98)
       (orb--with-message! "Pressing the RED button"))
     (write-region (orb-buffer-string)
                   nil (orb-pdf-scrapper--get :temp-bib) nil 1)
     (orb-pdf-scrapper--put :bib-undo-list (copy-tree buffer-undo-list))
     ;; generate Org-mode buffer
     (let* ((temp-org (or (orb-pdf-scrapper--get :temp-org)
                          (orb-temp-file "orb-pdf-scrapper-" ".org"))))
       (orb-pdf-scrapper--put :temp-org temp-org
                              :caller 'edit-org)
       ;; we must change the mode in the beginning to get all the Org-mode
       ;; facilities
       (orb-pdf-scrapper--refresh-mode 'org)
       (orb--with-message! "Generating Org data"
         (erase-buffer)
         (orb-pdf-scrapper--insert-refs)
         (write-region (orb-buffer-string) nil temp-org nil -1)
         (setq buffer-undo-list nil)
         (set-buffer-modified-p nil)
         (goto-char (point-min)))))
    ('continue
     (if-let ((temp-org (orb-pdf-scrapper--get :temp-org))
              (f-exists? temp-org))
         (progn
           (erase-buffer)
           (insert-file-contents temp-org)
           (setq buffer-undo-list (orb-pdf-scrapper--get :org-undo-list))
           (orb-pdf-scrapper--refresh-mode 'org))
       (orb-pdf-scrapper-dispatcher 'error)))))

(defun orb-pdf-scrapper--edit-xml ()
  "Edit XML data."
  (pop-to-buffer orb-pdf-scrapper--buffer)
  (cl-case (orb-pdf-scrapper--get :context)
    ('start
     (let* ((temp-xml (or (orb-pdf-scrapper--get :temp-xml)
                          (orb-temp-file "orb-pdf-scrapper-" ".xml"))))
       (orb-pdf-scrapper--put :temp-xml temp-xml)
       (orb--with-message! "Generating XML data"
         ;; save progress in text mode when called from there if called from
         ;; anywhere else, text mode progress is already saved, other data will
         ;; be re-generated anyway
         (when (eq (orb-pdf-scrapper--get :caller) 'edit-txt)
           (write-region (orb-buffer-string)
                         nil (orb-pdf-scrapper--get :temp-txt) nil -1)
           (orb-pdf-scrapper--put :txt-undo-list (copy-tree buffer-undo-list)))
         (erase-buffer)
         (orb-anystyle 'parse
           :format 'xml
           :parser-model orb-anystyle-parser-model
           :input (orb-pdf-scrapper--get :temp-txt)
           :stdout t
           :buffer orb-pdf-scrapper--buffer)
         (write-region (orb-buffer-string) nil temp-xml nil -1)
         (setq buffer-undo-list nil)
         (orb-pdf-scrapper--refresh-mode 'xml))))
    ('edit-master
     (progn
       (erase-buffer)
       (insert-file-contents orb-anystyle-parser-training-set)
       ;; we allow the user to see which file they are editing
       (setq buffer-file-name orb-anystyle-parser-training-set)
       (setq buffer-undo-list nil)
       (orb-pdf-scrapper--refresh-mode 'xml)))
    (t
     (orb-pdf-scrapper-dispatcher 'error))))

(defun orb-pdf-scrapper--update-master-file ()
  "Append generated XML data to `orb-anystyle-parser-training-set'."
  (orb--with-scrapper-buffer!
    (orb--with-message! (format "Appending to master training set %s"
                                orb-anystyle-parser-training-set)
      ;; save any progress in XML mode
      (write-region (orb-buffer-string) nil
                    (orb-pdf-scrapper--get :temp-xml) nil -1)
      (let (new-data)
        ;; strip down the header and footer tokens from our data
        (save-excursion
          (save-match-data
            (let* (beg end)
              (goto-char (point-min))
              (re-search-forward "\\(^[ \t]*<dataset>[ \t]*\n\\)" nil t)
              (setq beg (or (match-end 1)
                            (point-min)))
              (re-search-forward "\\(^[ \t]*</dataset>[ \t]*\n\\)" nil t)
              (setq end (or (match-beginning 1)
                            (point-max)))
              (setq new-data (orb-buffer-string beg end)))))
        ;; append our data to the master file
        (with-temp-buffer
          (insert-file-contents orb-anystyle-parser-training-set)
          ;; backup the master file
          (let ((master-backup (concat orb-anystyle-parser-training-set ".back")))
            (orb-pdf-scrapper--put :master-backup master-backup)
            (rename-file orb-anystyle-parser-training-set master-backup t))
          (goto-char (point-max))
          (forward-line -1)
          (insert new-data)
          (f-touch orb-anystyle-parser-training-set)
          (write-region (orb-buffer-string) nil
                        orb-anystyle-parser-training-set nil -1))))))

(defun orb-pdf-scrapper--train (&optional review)
  "Update parser training set and run anystyle train.
If optional REVIEW is non-nil, run `orb-pdf-scrapper--edit-xml'
in `:edit-master' context."
  (pop-to-buffer orb-pdf-scrapper--buffer)
  ;; edit the master file or proceed to training
  (if review
      ;; we've been requested to review the master file
      (progn
        (orb-pdf-scrapper--update-master-file)
        (orb-pdf-scrapper--put :context 'edit-master)
        (orb-pdf-scrapper--edit-xml))
    ;; start the training process otherwise
    (orb-pdf-scrapper--update-master-file)
    (message "Training anystyle parser model...")
    (when buffer-file-name
      (save-buffer))
    (setq buffer-file-name nil)
    (erase-buffer)
    (orb-pdf-scrapper--put :context 'train)
    (orb-pdf-scrapper--refresh-mode 'train)
    (insert (format "\
This can take several minutes depending on the size of your training set.
You can continue your work meanwhile and return here later.\n
Training set => %s
Parser model => %s\n
anystyle output:
=====================\n"
                    orb-anystyle-parser-training-set
                    (or orb-anystyle-parser-model "none")))
    (goto-char (point-min))
    ;; normally, anystyle runs with `shell-command', anystyle train, however,
    ;; can take minutes on large files, so it runs in a shell sub-process
    (let ((training-process
           (orb-anystyle 'train
             :stdout t
             :overwrite t
             :input orb-anystyle-parser-training-set
             :output orb-anystyle-parser-model
             :buffer orb-pdf-scrapper--buffer)))
      (orb-pdf-scrapper--put :training-process training-process)
      ;; finalize
      (set-process-sentinel
       training-process
       (lambda (_p result)
         (orb--with-scrapper-buffer!
           (if (string= result "finished\n")
               (orb--with-scrapper-buffer!
                 (goto-char (point-max))
                 (insert "=====================\n\nDone!\n\n")
                 (if orb-anystyle-parser-model
                     (insert (format "Parser model update: %s"
                                          orb-anystyle-parser-model))
                   (insert
                    (format "Parser model created: %s\n"
                            (concat
                             (or (file-name-directory
                                  orb-anystyle-parser-training-set)
                                 (file-name-as-directory
                                  orb-anystyle-user-directory))
                             "parser.mod"))
                    "To use the model, \
set `orb-anystyle-parser-model' variable to the above path."))
                 (message "Training anystyle parser model...done")
                 (orb-pdf-scrapper--put :context 'finished
                                        :training-process nil)
                 (orb-pdf-scrapper--refresh-mode 'train))
             (orb-pdf-scrapper--put :context 'error
                                    :training-process nil))))))))


;; ============================================================================
;;; Helper functions: Export of extracted references
;; ============================================================================

(defun orb-pdf-scrapper--export-get-point (type placement)
  "In current buffer, go to the point where data should be placed.
TYPE is target type, one of the symbols `txt', `bib' or `org'.
PLACEMENT is placement type, one of the symbols `append' or `prepend'.

Return the point."
  ;; for Org export go to the first or last heading, for BibTeX export
  ;; go to the first or last entry rather than the beginning or end of
  ;; buffer, respectively.
  (cl-case placement
    ('prepend
     (cl-case type
       (bib
        (let ((bibtex-sort-ignore-string-entries t))
          (bibtex-beginning-of-first-entry)))
       (org
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (org-get-next-sibling)))
       (t
        (goto-char (point-min)))))
    (t
     (cl-case type
       (bib
        (let ((bibtex-sort-ignore-string-entries t))
          (bibtex-end-of-entry)))
       (org
        (goto-char (point-max))
        (org-end-of-subtree)
        (forward-line))
       (t
        (goto-char (point-max))))))
  (point))

(defun orb-pdf-scrapper--export-insert-temp-data (type properties)
  "Insert data from temporary file at point.
TYPE is type of data.  PROPERTIES are additional export properties."
  (let* ((temp-file (orb-pdf-scrapper--get
                     (intern (format ":temp-%s" type))))
         (filter (plist-get properties :filter-bib-entries))
         ;; inline subroutine to filter BibTeX entries
         (insert-filtered-bib-entries
          (lambda (temp-file filter)
            (when (symbolp filter)
              (setq filter (symbol-value filter)))
            (let ((sources (cond
                            ((stringp filter) (list filter buffer-file-name))
                            ((listp filter) (append filter buffer-file-name))
                            (t (list buffer-file-name))))
                  keys buf-data)
              (save-excursion
                (dolist (source sources)
                  (when source
                    (let ((buffer-visisted-p (find-buffer-visiting source)))
                      (find-file source)
                      (when (eq major-mode 'bibtex-mode)
                        (maphash (lambda (key _val)
                                   (push key keys))
                                 (car (parsebib-parse-buffer))))
                      (unless buffer-visisted-p
                        (kill-buffer (current-buffer)))))))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (goto-char (point-min))
                (let ((bibtex-sort-ignore-string-entries t))
                  (bibtex-set-dialect 'biblatex t)
                  (bibtex-map-entries
                   (lambda (key _beg _end)
                     (when (member key keys)
                       (bibtex-kill-entry)))))
                (setq buf-data (orb-buffer-string)))
              (insert buf-data)))))
    (cl-case type
      (bib
       (if filter
           (funcall insert-filtered-bib-entries temp-file filter)
         (insert-file-contents temp-file)))
      (t
       (insert-file-contents temp-file)))))

(defun orb-pdf-scrapper--export-to-heading (type name properties)
  "Description TYPE NAME PROPERTIES."
  (let ((drawer-props (plist-get properties :property-drawer))
        (placement (plist-get properties :placement))
        (end (make-marker))
        beg data)
    ;; Make the heading in a temporary buffer
    (with-temp-buffer
      ;; get the desired position
      ;; insert parent heading
      ;; NOTE: Investigate
      ;; The behaviour of org-insert-heading has changed at some point:
      ;; If in an empty buffer, e.g. temp-buffer, the function fails messaging "beginning of buffer"
      (org-N-empty-lines-before-current 1)
      (org-insert-heading nil nil t)
      (insert name)
      ;; insert properties
      (dolist (prop drawer-props)
        (let ((prop-name (or (car-safe prop) prop))
              (value (cdr-safe prop))
              prop-value)
          (cond
           ;; call user function if provided
           ((functionp value)
            (setq prop-value (funcall value))
            (unless (stringp prop-value)
              (user-error "Function %s must return a string.  \
Check `orb-pdf-scrapper-export-options'" value)))
           ;; provide some values for select properties - if the name was
           ;; specified but not a value;
           ;; NOTE: rather a placeholder for future elaboration
           ((null value)
            (cond
             ((string= "PDF_SCRAPPER_TYPE" prop-name)
              (setq prop-value (format "%s" type)))
             ((string= "PDF_SCRAPPER_SOURCE" prop-name)
              (setq prop-value
                    (f-filename (orb-pdf-scrapper--get :pdf-file))))
             ((string= "PDF_SCRAPPER_DATE" prop-name)
              (setq prop-value (org-timestamp-format
                                (org-timestamp-from-time
                                 (current-time) 'with-time)
                                "%Y-%m-%d %a %H:%M")))))
           ;; insert the user value
           (t (setq prop-value value)))
          ;; insert the property
          (org-set-property prop-name prop-value)))
      (org-end-of-meta-data)
      (insert "\n")
      (setq beg (point))
      (set-marker end beg)
      (set-marker-insertion-type end t)
      (orb-pdf-scrapper--export-insert-temp-data type properties)
      ;; do some type-specific stuff
      ;;
      ;; Org: demote group headings which are to become subheadings of the
      ;; newly created heading.
      ;;
      ;; BibTeX: insert into a language source block
      (cl-case type
        (org
         (org-mode)
         (goto-char beg)
         (while (re-search-forward org-heading-regexp nil t)
           (org-demote)))
        (bib
         (goto-char beg)
         (insert "#+begin_src bibtex\n")
         (goto-char end)
         (insert "#+end_src\n")))
      (setq data (orb-buffer-string))
      (set-marker end nil))
    ;; insert the data
    (orb-pdf-scrapper--export-get-point 'org placement)
    (insert data "\n")))

(defun orb-pdf-scrapper--export-to-file (type location properties)
  "Export data generated by ORB PDF Scrapper to a file.
TYPE is a symbol identifying type of data to be exported, one of
`org', `txt', or `bib'.

LOCATION is a string specifying the location of the target file.
It can be a relative or an absolute file path.  If the file does
not exist, it will be created.  It can also be a relative or an
absolute path to an existing directory.  In this case the data
will be exported to a file in that directory with the citation
key associated with the buffer of origin (extracted from its
#+ROAM_KEY: property) as the filename and TYPE as the extension.

PROPERTIES is a property list with additional export properties.
See `orb-pdf-scrapper-export-options' for details."
  (let* ((current-dir (file-name-directory
                       (buffer-file-name
                        (orb-pdf-scrapper--get :original-buffer))))
         (current-key
          (orb-pdf-scrapper--get :current-key))
         ;; this is a sort of cond, but execute all clauses sequentially
         (path (--> location
                    ;; if location is non-nil and it is a relative filename,
                    ;; expand it within the original buffer's directory
                    (when it
                      (if (f-relative? it) (f-join current-dir it) it))
                    ;; if location is nil assume current directory
                    (if (null it) current-dir it)
                    ;; if location is a directory, make a file with citekey as
                    ;; the file name and type as the extension the location
                    ;; otherwise return the location
                    (if (f-dir? it)
                        (f-join it (format "%s.%s" current-key type))
                      it)))
         ;; file extension if any
         (ext (f-ext path))
         (buffer-visited-p (find-buffer-visiting path))
         target-type buf)
      (find-file path)
      (setq buf (current-buffer))
      ;; type of the target file; try to determine it from the major mode;
      ;; assume TYPE otherwise.
      (setq target-type
            (pcase major-mode
              ('org-mode 'org)
              ('bibtex-mode 'bib)
              ((or 'text-mode 'fundamental-mode) 'txt)
              (_ type)))
      (save-mark-and-excursion
        (pcase (plist-get properties :placement)
          ('prepend
           (orb-pdf-scrapper--export-get-point target-type 'prepend)
           (orb-pdf-scrapper--export-insert-temp-data type properties)
           (when (memq type '(org txt))
             (insert "\n")))
          (`(heading ,headline . ,heading-properties)
           (if (string= ext "org")
               ;; NOTE: heading-properties take precendence over path
               ;; properties
               (orb-pdf-scrapper--export-to-heading
                type headline (append heading-properties properties))
             (user-error "Heading placement only possible in ORG files")))
          ;; defaults to append
          (_
           (orb-pdf-scrapper--export-get-point target-type 'append)
           (when (memq type '(bib txt))
             (insert "\n"))
           (orb-pdf-scrapper--export-insert-temp-data type properties))))
      (save-buffer buf)
    (unless buffer-visited-p
      (kill-buffer buf))))

(defun orb-pdf-scrapper--export (type)
  "Export the extracted and/or generated data.
TYPE is a symbol identifying type of data to be exported, one
of `txt', `bib' or `org'.

The user variable `orb-pdf-scrapper-export-options' controls
export options."
  ;; there may be several targets for a given TYPE, export to all of them
  (cl-loop
   for (target location . properties)
   in (cdr (assoc type orb-pdf-scrapper-export-options))
   do (cl-case target
        (heading
         (orb-pdf-scrapper--export-to-heading type location properties))
        (path
         (orb-pdf-scrapper--export-to-file type location properties)))))

(defun orb-pdf-scrapper--checkout ()
  "Finalize Orb PDF Scrapper process.
Insert the extracted and generated data according to the settings
of `orb-pdf-scrapper-export-options'."
  (cl-case (orb-pdf-scrapper--get :context)
    ('start
     (pop-to-buffer (orb-pdf-scrapper--get :original-buffer))
     ;; export the extracted/generated data
     (dolist (type (mapcar #'car orb-pdf-scrapper-export-options))
       (orb-pdf-scrapper--export type))
     ;; NOTE: "break point" for ease of debugging
     ;; (user-error "Halt")
     (orb-pdf-scrapper-dispatcher 'kill))
    (t
     (orb-pdf-scrapper-dispatcher 'error))))

(defun orb-pdf-scrapper--cleanup ()
  "Clean up before and after Orb Pdf Scrapper process."
  (setq orb-pdf-scrapper--refs ())
  (dolist (prop (list :running :callee :context :caller
                      :current-key :prevent-concurring
                      :temp-txt :temp-bib :temp-org :temp-xml
                      :pdf-file :global-bib :master-backup
                      :txt-undo-list :bib-undo-list :org-undo-list
                      :training-process :window-conf :original-buffer))
    (orb-pdf-scrapper--put prop nil)))


;; ============================================================================
;;; Minor mode
;; ============================================================================

;;; Code in this section was adopted from org-capture.el
(defvar orb-pdf-scrapper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" #'orb-pdf-scrapper-kill)
    (define-key map [remap save-buffer] #'orb-pdf-scrapper-save)
    (define-key map [remap write-file] #'orb-pdf-scrapper-save-as)
    map)
  "Keymap for `orb-pdf-scrapper-mode' minor mode.
The keymap is updated automatically according to the Orb PDF
Scrapper process context.  It is not supposed to be modified
directly by user." )

(defcustom orb-pdf-scrapper-mode-hook nil
  "Hook for the `orb-pdf-scrapper-mode' minor mode."
  :type 'hook
  :group 'orb-pdf-scrapper)

(define-minor-mode orb-pdf-scrapper-mode
  "Minor mode for special key bindings in a orb-pdf-scrapper buffer.
Turning on this mode runs the normal hook `orb-pdf-scrapper-mode-hook'."
  nil " OPS" orb-pdf-scrapper-mode-map
  (when orb-pdf-scrapper-mode
    (orb-pdf-scrapper--update-keymap)
    (setq-local
     header-line-format
     (orb-pdf-scrapper--format-header-line))))

(defun orb-pdf-scrapper--put (&rest props)
  "Add properties PROPS to `orb-pdf-scrapper--plist'.
Returns the new plist."
  (while props
    (setq orb-pdf-scrapper--plist
          (plist-put orb-pdf-scrapper--plist
                     (pop props)
                     (pop props)))))

(defun orb-pdf-scrapper--get (prop)
  "Get PROP from `orb-pdf-scrapper--plist'."
  (plist-get orb-pdf-scrapper--plist prop))
;;;
;;; End of code adopted from org-capture.el


;; TODO combine `orb-pdf-scrapper--format-header-line'
;; and `orb-pdf-scrapper--update-keymap' into one
;; function and use a macro to generate each entry
(defun orb-pdf-scrapper--format-header-line ()
  "Return formatted buffer header line depending on context."
  (substitute-command-keys
   (format "\\<orb-pdf-scrapper-mode-map>Orb PDF Scrapper: %s.  %s"
           (orb-pdf-scrapper--get :current-key)
           (cl-case (orb-pdf-scrapper--get :caller)
             ('edit-txt
              "\
Generate BibTeX `\\[orb-pdf-scrapper-dispatcher]', \
sanitize text `\\[orb-pdf-scrapper-sanitize-text]', \
train parser `\\[orb-pdf-scrapper-training-session]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             ('edit-bib
              "\
Generate Org `\\[orb-pdf-scrapper-dispatcher]', \
generate keys `\\[orb-pdf-scrapper-generate-keys]', \
return to text `\\[orb-pdf-scrapper-cancel]', \
train parser `\\[orb-pdf-scrapper-training-session], \
abort `\\[orb-pdf-scrapper-kill]'.")
             ('edit-org
              "\
Finish `\\[orb-pdf-scrapper-dispatcher]', \
return to BibTeX `\\[orb-pdf-scrapper-cancel]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             ('edit-xml
              (cl-case (orb-pdf-scrapper--get :context)
                ('edit
                 (format "\
Train `\\[orb-pdf-scrapper-training-session]', \
review %s `\\[orb-pdf-scrapper-review-master-file]', \
cancel `\\[orb-pdf-scrapper-cancel], \
abort `\\[orb-pdf-scrapper-kill]'."
                      (file-name-nondirectory
                       orb-anystyle-parser-training-set)))
                ('edit-master
                 "\
Train `\\[orb-pdf-scrapper-training-session]', \
cancel `\\[orb-pdf-scrapper-cancel], \
abort `\\[orb-pdf-scrapper-kill]'.")))
             ('train
              (cl-case (orb-pdf-scrapper--get :context)
                ('train
                 "\
Abort `\\[orb-pdf-scrapper-kill]'.")
                ('continue
                 "\
Finish `\\[orb-pdf-scrapper-dispatcher]', \
abort `\\[orb-pdf-scrapper-kill]'.")))
             (t
              "\
Press the RED button `\\[orb-pdf-scrapper-kill]'.")))))

(defun orb-pdf-scrapper--update-keymap ()
  "Update `orb-pdf-scrapper-mode-map' according to current editing mode.
Context is read from `orb-pdf-scrapper--plist' property `:context'."
  (let ((map orb-pdf-scrapper-mode-map))
    (cl-case (orb-pdf-scrapper--get :caller)
      ;;
      ('edit-txt
       (define-key map "\C-c\C-c" #'orb-pdf-scrapper-dispatcher)
       (define-key map "\C-c\C-u" #'orb-pdf-scrapper-sanitize-text)
       (define-key map "\C-C\C-t" #'orb-pdf-scrapper-training-session)
       (define-key map "\C-c\C-r" nil))
      ;;
      ('edit-bib
       (define-key map "\C-c\C-c" #'orb-pdf-scrapper-dispatcher)
       (define-key map "\C-c\C-u" #'orb-pdf-scrapper-generate-keys)
       (define-key map "\C-C\C-t" #'orb-pdf-scrapper-training-session)
       (define-key map "\C-c\C-r" #'orb-pdf-scrapper-cancel))
      ;;
      ('edit-org
       (define-key map "\C-c\C-c" #'orb-pdf-scrapper-dispatcher)
       (define-key map "\C-c\C-u" nil)
       (define-key map "\C-C\C-t" nil)
       (define-key map "\C-c\C-r" #'orb-pdf-scrapper-cancel))
      ('edit-xml
       (cl-case (orb-pdf-scrapper--get :context)
         ('edit
          (define-key map "\C-c\C-c" #'orb-pdf-scrapper-training-session)
          (define-key map "\C-c\C-u" nil)
          (define-key map "\C-C\C-t" #'orb-pdf-scrapper-review-master-file)
          (define-key map "\C-c\C-r" #'orb-pdf-scrapper-cancel))
         ('edit-master
          (define-key map "\C-c\C-c" #'orb-pdf-scrapper-training-session)
          (define-key map "\C-c\C-u" nil)
          (define-key map "\C-C\C-t" nil)
          (define-key map "\C-c\C-r" #'orb-pdf-scrapper-cancel))))
      ('train
       (cl-case (orb-pdf-scrapper--get :context)
         ('train
          (define-key map "\C-c\C-c" nil)
          (define-key map "\C-c\C-r" nil)
          (define-key map "\C-c\C-u" nil)
          (define-key map "\C-c\C-t" nil))
         ('continue
          (define-key map "\C-c\C-c" #'orb-pdf-scrapper-dispatcher))))
      (t
       (define-key map "\C-c\C-u" nil)
       (define-key map "\C-c\C-t" nil)
       (define-key map "\C-c\C-r" nil)))))


;; ============================================================================
;;; Interactive functions
;; ============================================================================

(defun orb-pdf-scrapper-generate-keys (&optional at-point collect-only)
  "Generate BibTeX citation keys in the current buffer.
\\<orb-pdf-scrapper-mode-map>
While the Orb PDF Scrapper interactive process, when editing
BibTeX data, press \\[orb-pdf-scrapper-generate-keys] to generate
citation keys using the function specified in
`orb-pdf-scrapper-keygen-function'.  When called interactively
with a \\[universal-argument] prefix argument AT-POINT, generate
key only for the record at point.

When called from Lisp, if optional COLLECT-ONLY is non-nil, do
not generate the key and update the records, just collect records
for future use."
  (interactive "P")
  (orb--with-message! "Generating citation keys"
    (let ((bibtex-help-message nil)
          (bibtex-contline-indentation 2)
          (bibtex-text-indentation 2))
      (save-excursion
        (if (equal at-point '(4))
            ;; generate key at point
            (progn
              (bibtex-beginning-of-entry)
              (let* ((old-key (save-excursion
                                (re-search-forward
                                 bibtex-entry-maybe-empty-head)
                                (bibtex-key-in-head)))
                     (old-ref (assoc old-key orb-pdf-scrapper--refs))
                     (new-ref (orb-pdf-scrapper--update-record-at-point
                               collect-only)))
                (if old-ref
                    (setf (car old-ref) (car new-ref)
                          (cdr old-ref) (cdr new-ref))
                  (cl-pushnew new-ref orb-pdf-scrapper--refs :test 'equal))))
          ;; generate keys in the buffer otherwise
          (let ((refs ())
                (natural-order 1))
            (goto-char (point-min))
            (bibtex-skip-to-valid-entry)
            (while (not (eobp))
              (cl-pushnew (orb-pdf-scrapper--update-record-at-point
                           collect-only (format "%s" natural-order))
                          refs)
              (bibtex-skip-to-valid-entry)
              (setq natural-order (1+ natural-order)))
            (setq orb-pdf-scrapper--refs refs)))))
    (write-region (orb-buffer-string) nil
                  (orb-pdf-scrapper--get :temp-bib) nil -1)
    (set-buffer-modified-p nil)))

(defun orb-pdf-scrapper-sanitize-text (&optional contents)
  "Run string processing in current buffer.
Try to get every reference onto newline.  Return this buffer's
contents (`orb-buffer-string').

If optional string CONTENTS was specified, run processing on this
string instead.  Return modified CONTENTS."
  (interactive)
  (let* ((rx1 '(and "(" (** 1 2 (any "0-9")) ")"))
         (rx2 '(and "[" (** 1 2 (any "0-9")) "]"))
         (rx3 '(and "(" (any "a-z") (opt (any space)) ")"))
         (rx4 '(and " " (any "a-z") ")"))
         (regexp (rx-to-string
                  `(group-n 1 (or (or (and ,rx1 " " ,rx3)
                                      (and ,rx2 " " ,rx3))
                                  (or (and ,rx1 " " ,rx4)
                                      (and ,rx2 " " ,rx4))
                                  (or ,rx1 ,rx2)
                                  (or ,rx3 ,rx4))) t)))
    (if contents
        (--> contents
             (s-replace "\n" " " it)
             (s-replace-regexp regexp "\n\\1" it))
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match "\n\\1" nil nil))
      (goto-char (point-min))
      (orb-buffer-string))))

(defun orb-pdf-scrapper-training-session (&optional context)
  "Run training session subroutines depending on CONTEXT.
If context is not provided, it will be read from
`orb-pdf-scrapper--plist''s `:context'."
  (interactive)
  (pop-to-buffer orb-pdf-scrapper--buffer)
  (let ((context (or context (orb-pdf-scrapper--get :context))))
    (orb-pdf-scrapper--put :context context)
    (cl-case context
      ('start
       ;; generate xml
       (orb-pdf-scrapper--edit-xml))
      ((edit edit-master)
       (orb-pdf-scrapper--train nil))
      ('finished
       (orb-pdf-scrapper-dispatcher 'edit-txt 'continue))
      (t (orb-pdf-scrapper-dispatcher 'error)))))

(defun orb-pdf-scrapper-review-master-file ()
  "Review parser training set (master file)."
  (interactive)
  (orb-pdf-scrapper--train t))

(defun orb-pdf-scrapper-dispatcher (&optional callee context)
  "Call Orb PDF Scrapper subroutine CALLEE in context CONTEXT.
CALLEE and CONTEXT can be passed directly as optional variables,
or they will be read from `orb-pdf-scrapper--plist''s
respectively `:collee' and `:context' properties.

Recognized CALLEEs are:
==========
'edit-txt - `orb-pdf-scrapper--edit-txt'
'edit-bib - `orb-pdf-scrapper--edit-bib'
'edit-org - `orb-pdf-scrapper--edit-org'
'train    - `orb-pdf-scrapper-training-session'
'checkout - `orb-pdf-scrapper--checkout'

Passing or setting any other CALLEE will kill the process.

This function also checks `:prevent-concurring' property in
`orb-pdf-scrapper--plist' and will suggest to restart the process
if its value is non-nil."
  ;; TODO: check for whether the user killed any of the buffers
  (interactive)
  (let ((callee (or callee (orb-pdf-scrapper--get :callee)))
        (context (or context (orb-pdf-scrapper--get :context))))
    ;; in case context was passed as an argument
    (orb-pdf-scrapper--put :callee callee
                           :context context)
    (if
        ;; Prevent another Orb PDF Scrapper process from running
        ;; Ask user whether to kill the currently running process
        (orb-pdf-scrapper--get :prevent-concurring)
        (if (y-or-n-p
             (format "Another Orb PDF Scrapper process is running: %s.  \
Kill it and start a new one %s? "
                     (orb-pdf-scrapper--get :current-key)
                     (orb-pdf-scrapper--get :new-key)))
            ;; Kill the process and start a new one
            (progn
              (orb--with-message! "Killing current process"
                (orb-pdf-scrapper--cleanup))
              (orb-pdf-scrapper-run (orb-pdf-scrapper--get :new-key)))
          ;; go to the Scrapper buffer
          (pop-to-buffer orb-pdf-scrapper--buffer)
          ;; reset the concurring flag set by `orb-pdf-scrapper-run'
          (orb-pdf-scrapper--put :prevent-concurring nil))
      ;; Finilize the requested context otherwise
      (cl-case callee
        ('edit-txt
         (orb-pdf-scrapper--edit-txt))
        ('edit-bib
         (orb-pdf-scrapper--edit-bib))
        ;; edit org
        ('edit-org
         (orb-pdf-scrapper--edit-org))
        ('checkout
         ;; currently, this is unnecessary but may be useful
         ;; if some recovery options are implemented
         (orb--with-scrapper-buffer!
           (write-region (orb-buffer-string)
                         nil (orb-pdf-scrapper--get :temp-org) nil 1))
         (orb-pdf-scrapper--checkout))
        (t
         ;; 1 in 100 should not be too annoying
         (when (> (cl-random 100) 98)
           (message "Oops...")
           (sleep-for 1)
           (message "Oops...Did you just ACCIDENTALLY press the RED button?")
           (sleep-for 1)
           (message "Activating self-destruction subroutine...")
           (sleep-for 1)
           (message "Activating self-destruction subroutine...Bye-bye")
           (sleep-for 1))
         (let ((kill-buffer-query-functions nil))
           (and (get-buffer orb-pdf-scrapper--buffer)
                (kill-buffer orb-pdf-scrapper--buffer)))
         (set-window-configuration (orb-pdf-scrapper--get :window-conf))
         (orb-pdf-scrapper--cleanup))))))

(defun orb-pdf-scrapper-cancel ()
  "Discard edits and return to previous editing mode."
  (interactive)
  (cl-case (orb-pdf-scrapper--get :caller)
    ('edit-bib
     (orb--with-scrapper-buffer!
       (orb-pdf-scrapper--put :bib-undo-list nil))
     (orb-pdf-scrapper-dispatcher 'edit-txt 'continue))
    ('edit-org
     (orb-pdf-scrapper-dispatcher 'edit-bib 'continue))
    ('edit-xml
     (when-let ((master-backup (orb-pdf-scrapper--get :master-backup)))
       (rename-file master-backup orb-anystyle-parser-training-set t)
       (setq buffer-file-name nil))
     (orb-pdf-scrapper-dispatcher (orb-pdf-scrapper--get :callee) 'continue))
    (t
     (orb-pdf-scrapper-dispatcher 'error))))

(defun orb-pdf-scrapper-kill ()
  "Kill the interactive Orb PDF Scrapper process."
  (interactive)
  (when-let (process (orb-pdf-scrapper--get :training-process))
    (kill-process process))
  (orb-pdf-scrapper-dispatcher 'kill))

(defun orb-pdf-scrapper-save ()
  "Save current ORB PDF Scrapper buffer in the respective temp file.
This command shadows `save-buffer' when `orb-pdf-scrapper-mode' is active."
  (interactive)
  (let ((temp-file
         (cl-case (orb-pdf-scrapper--get :caller)
           ('edit-txt (orb-pdf-scrapper--get :temp-txt))
           ('edit-bib (orb-pdf-scrapper--get :temp-bib))
           ('edit-org (orb-pdf-scrapper--get :temp-org))
           ('edit-xml (orb-pdf-scrapper--get :temp-xml))
           (t nil))))                   ; fallback flag
    (cond
     ;; ORB PDF Scrapper buffers do not have file names
     ((and (not buffer-file-name) temp-file)
      (write-region (orb-buffer-string) nil temp-file nil -1)
      (set-buffer-modified-p nil))
     ((save-buffer)))))

(defun orb-pdf-scrapper-save-as ()
  "Export current ORB PDF Scrapper buffer to a file.
This command shadows `write-file' when `orb-pdf-scrapper-mode' is active."
  (interactive)
  ;; ORB PDF Scrapper buffers do not have file names
  (cond
   ((not buffer-file-name)
    (call-interactively #'write-file)
    (set-visited-file-name nil)
    (rename-buffer orb-pdf-scrapper--buffer))
   ((call-interactively #'write-file))))


;; ============================================================================
;;; Entry point
;; ============================================================================

;;;###autoload
(defun orb-pdf-scrapper-run (key)
  "Run Orb PDF Scrapper interactive process.
KEY is note's citation key."
  (interactive)
  (if (orb-pdf-scrapper--get :running)
      (progn
        (orb-pdf-scrapper--put :prevent-concurring t
                               :new-key key)
        (orb-pdf-scrapper-dispatcher))
    ;; in case previous process was not killed properly
    (orb-pdf-scrapper--cleanup)
    (orb-pdf-scrapper--put :callee 'edit-txt
                           :context 'start
                           :caller 'run
                           :current-key key
                           :new-key nil
                           :pdf-file (file-truename
                                      (orb-process-file-field key))
                           :running t
                           :prevent-concurring nil
                           :global-bib bibtex-completion-bibliography
                           :original-buffer (current-buffer)
                           :window-conf (current-window-configuration))
    (orb-pdf-scrapper-dispatcher)))

(provide 'orb-pdf-scrapper)
;;; orb-pdf-scrapper.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
