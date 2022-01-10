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

(require 'cl-lib)
(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x)
  (require 'rx))

;; own functions and variables
;;
(declare-function orb-helm-insert "orb-helm")
(declare-function orb-note-actions-helm "orb-helm")
(declare-function orb-ivy-insert "orb-ivy")
(declare-function orb-note-actions-ivy "orb-ivy")

;; external functions and variables

;; Bibtex-completion
(declare-function
 bibtex-completion-get-entry "bibtex-completion" (entry-key))
(declare-function
 bibtex-completion-get-value "bibtex-completion" (field entry &optional default))
(declare-function
 bibtex-completion-find-pdf (key-or-entry &optional find-additional))


;; Projectile, Perspective-mode
(declare-function projectile-relevant-open-projects "ext:projectile")
(declare-function persp-switch "ext:persp-mode")
(declare-function persp-names "ext:persp-mode")

;; Org-ref
(defvar org-ref-notes-function)
(declare-function org-ref-find-bibliography "ext:org-ref-core")

;; Citar
(defvar citar-open-note-function)

;; Hydra
(declare-function defhydra "ext:hydra")

;; Refuse
(declare-function refuse-run "refuse" (key))


;; ============================================================================
;;; Customize groups
;; ============================================================================

(defgroup org-roam-bibtex nil
  "Org-roam integration with BibTeX software."
  :group 'org-roam
  :prefix "orb-")

(defgroup orb-note-actions nil
  "Orb Note Actions - run actions in note's context."
  :group 'org-roam-bibtex
  :prefix "orb-note-actions-")


;; ============================================================================
;;; Customize definitions
;; ============================================================================

;; Templates

(defcustom orb-preformat-templates t
  "Non-nil to enable template pre-expanding.
See `orb-edit-note' for details."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'org-roam-bibtex)

(defcustom orb-preformat-keywords
  '("citekey" "entry-type" "date" "pdf?" "note?" "file"
    "author" "editor" "author-abbrev" "editor-abbrev"
    "author-or-editor-abbrev")
  "A list of template placeholders for pre-expanding.
Any BibTeX field can be set for pre-expanding including
Bibtex-completion virtual fields such as '=key=' and '=type='.
BibTeX fields can be referred to by means of their aliases
defined in `orb-bibtex-field-aliases'.

Usage example:

\(setq orb-preformat-keywords '(\"citekey\" \"author\" \"date\"))
\(setq orb-templates
      '((\"r\" \"reference\" plain
         \"#+ROAM_KEY: %^{citekey}%?
%^{author} published %^{entry-type} in %^{date}: fullcite:%\\1.\"
         :target
         (file+head \"references/${citekey.org}\" \"#+title: ${title}\n\")
         :unnarrowed t)))

Special cases:

The \"file\" keyword will be treated specially if the value of
`orb-process-file-keyword' is non-nil.  See its docstring for an
explanation.

This variable takes effect when `orb-preformat-templates' is set
to t (default). See also `orb-edit-note' for further details.

Consult Bibtex-completion documentation for additional
information on BibTeX field names."
  :type '(repeat :tag "BibTeX field names" string)
  :group 'org-roam-bibtex)

(defcustom orb-roam-ref-format 'org-ref-v2
  "Defines the format of citation key in the `ROAM_REFS' property.
Should be one of the following symbols:
- `org-ref-v2': Old Org-ref `cite:links'
- `org-ref-v3': New Org-ref `cite:&links'
- `org-cite'  : Org-cite `@elements'

This can also be a custom `format' string with a single `%s' specifier."
  :type '(radio
          (const :tag "Org-ref v2" org-ref-v2)
          (const :tag "Org-ref v3" org-ref-v3)
          (const :tag "Org-cite" org-cite)
          (string :tag "Custom format string"))
  :group 'org-roam-bibtex)

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

(defcustom orb-bibtex-entry-get-value-function #'bibtex-completion-apa-get-value
  "Function to be used by ORB for values from a BibTeX entry.

The default value of this variable is `bibtex-completion-apa-get-value',
which offers some post-formatting for author fields.

Another possible choice available out of the box is
`bibtex-completion-get-value', which returns a verbatim value.

Set this to a custom function if you need more flexibility.
This function should take two arguments FIELD-NAME and ENTRY.
FIELD-NAME is the name of the field whose value should be retrieved.
ENTRY is a BibTeX entry as returned by `bibtex-completion-get-entry'."
  :risky t
  :group 'org-roam-bibtex
  :type '(radio (function-item bibtex-completion-apa-get-value)
                (function-item bibtex-completion-get-value)
                (function :tag "Custom function")))

;; Handling BibTeX file field and attachments

(defcustom orb-process-file-keyword t
  "Whether to treat the file keyword specially during template pre-expanding.
When this variable is non-nil, the \"%^{file}\" and \"${file}\"
wildcards will be processed by `org-process-file-field' rather
than simply replaced with the field value.  This may be useful in
situations when the file field contains several file names and
only one file name is desirable for retrieval.  The \"file\"
keyword must be set for pre-expanding in `orb-preformat-keywords'
as usual.

If this variable is `string', for example \"my-file\", use its
value as the wildcard keyword instead of the default \"file\"
keyword.  Thus, it will be possible to get both the raw file
field value by expanding the %^{file} and ${file} wildcards and a
single file name by expanding the %^{my-file} and ${my-file}
wildcards.  The keyword, e.g. \"my-file\", must be set for
pre-expanding in `orb-preformat-keywords' as usual.

The variable `orb-attached-file-extensions' controls filtering of
file names based on file extensions."
  ;; TODO: check if a custom string is really working as described
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)
          (string :tag "Custom wildcard keyword")))

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

;; ORB Insert

(defcustom orb-insert-interface 'generic
  "Interface frontend to use with `orb-insert-link'.
Possible values are the symbols `helm-bibtex', `ivy-bibtex', or
`generic' (default).  In the first two cases the respective
commands will be used, while in the latter case the command
`orb-insert-generic' will be used.

When using `helm-bibtex' or `ivy-bibtex' as `orb-insert-interface',
choosing the action \"Edit note & insert a link\" will insert the
desired link.  For convenience, this action is made default for
the duration of an `orb-insert-link' session.  It will not
persist when `helm-bibtex' or `ivy-bibtex' proper are run.
Otherwise, the command is just the usual `helm-bibtex'/`ivy-bibtex'.
For example, it is possible to run other `helm-bibtex' or
`ivy-bibtex' actions.  When action other than \"Edit note &
insert a link\" is run, no link will be inserted, although the
session can be resumed later with `helm-resume' or `ivy-resume',
respectively, where it will be possible to select the \"Edit note
& insert a link\" action.

When using the `generic' interface, a simple list of available
citation keys is presented using `completion-read' and after
choosing a candidate the appropriate link will be inserted.

Please note that this variable should be set using the Customize
interface, `use-package''s `:custom' keyword, or Doom's `setq!'
macro.  Simple `setq' will not work."
  :group 'org-roam-bibtex
  :type '(radio
          (const helm-bibtex)
          (const ivy-bibtex)
          (const generic))
  :set (lambda (var value)
         (cond
          ((eq value 'ivy-bibtex)
           (require 'orb-ivy))
          ((eq value 'helm-bibtex)
           (require 'orb-helm)))
         (set-default var value)))

(defcustom orb-insert-link-description 'title
  "Link description format for links created with `orb-insert-link'.
The command `orb-insert-link' can be used to create Org-mode
links to bibliographic notes of type [[id:note_id][Description]].
This variable determines the 'Description' part from the example
above.  It is an `s-format' string, where special placeholders of
form '${field}' will be expanded with data from the respective
BibTeX field of the associated BibTeX entry.  If the field's
value cannot be retrieved, the user will be prompted to input a
value interactively.  When retrieving BibTeX data, the user
options `orb-bibtex-field-aliases' and
`orb-bibtex-entry-get-value-function' are respected.

This variable can also be one of the following symbols:

`title'              - equivalent to \"${title}\"
`citekey'            - equivalent to \"${citekey}\"
`citation-org-ref-2' - create Org-ref v2 'cite:citekey' citation instead
`citation-org-ref-3' - create Org-ref v3 'cite:&citekey' citation instead
`citation-org-cite'  - create Org-cite '[cite:@citekey]' citation instead

The default value set by this variable can be overriden by
calling `orb-insert-link' with an appropriated numerical prefix
argument.  See its docstring for more information."
  :group 'org-roam-bibtex
  :type '(choice
          (string :tag "Format string")
          (const :tag "Title" title)
          (const :tag "Citation key" citekey)
          (const :tag "Citation link" citation-org-ref-2)
          (const :tag "Citation link" citation-org-ref-3)
          (const :tag "Citation link" citation-org-cite)))

(defcustom orb-insert-follow-link nil
  "Whether to follow a newly inserted link."
  :group 'orb-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom orb-insert-generic-candidates-format 'key
  "Format of selection candidates for `orb-insert-generic' interface.
Possible values are `key' and `entry'."
  :group 'org-roam-bibtex
  :type '(choice
          (const key)
          (const entry)))

;; ORB Note Actions

(defcustom orb-note-actions-interface 'default
  "Interface frontend for `orb-note-actions'.
Supported values (interfaces) are symbols `default', `ido',
`hydra', `ivy' and `helm'.

Alternatively, it can be set to a function, in which case the
function should expect one argument CITEKEY, which is a list
whose car is the citation key associated with the org-roam note
the current buffer is visiting.  Also, it should ideally make use
of `orb-note-actions-default', `orb-note-actions-extra' and
`orb-note-actions-user' for providing an interactive interface,
through which the combined set of note actions is presented as a
list of candidates and the function associated with the candidate
is executed upon selecting it.

This variable should be set using the Customize interface,
`use-package''s `:custom' keyword, or Doom's `setq!' macro.
Simple `setq' will not work."
  :risky t
  :type '(radio
          (const :tag "Default" default)
          (const :tag "Ido" ido)
          (const :tag "Hydra" hydra)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (function :tag "Custom function"))
  :set (lambda (var value)
         (cond
          ((eq value 'ivy)
           (require 'orb-ivy))
          ((eq value 'helm)
           (require 'orb-helm))
          ((eq value 'hydra)
           (require 'hydra)))
         (set-default var value))
  :group 'orb-note-actions)

(defcustom orb-note-actions-default
  '(("Open PDF file(s)" . orb-open-attached-file)
    ("Add PDF to library" . bibtex-completion-add-pdf-to-library)
    ("Open URL or DOI in browser" . bibtex-completion-open-url-or-doi)
    ("Show record in the bibtex file" . bibtex-completion-show-entry))
  "Default actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :risky t
  :type '(alist
          :tag "Default actions for `orb-note-actions'"
          :key-type (string :tag "Description")
          :value-type (function :tag "Function"))
  :group 'orb-note-actions)

(defcustom orb-note-actions-extra
  '(("Save citekey to kill-ring and clipboard" . orb-note-actions-copy-citekey)
    ("Run Orb PDF Scrapper" . orb-note-actions-scrape-pdf))
  "Extra actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :risky t
  :type '(alist
          :tag "Extra actions for `orb-note-actions'"
          :key-type (string :tag "Description")
          :value-type (function :tag "Function"))
  :group 'orb-note-actions)

(defcustom orb-note-actions-user nil
  "User actions for `orb-note-actions'.
Each action is a cons cell DESCRIPTION . FUNCTION."
  :risky t
  :type '(alist
          :tag "User actions for `orb-note-actions'"
          :key-type (string :tag "Description")
          :value-type (function :tag "Function"))
  :group 'orb-note-actions)

;; Miscellaneous

(defcustom orb-ignore-bibtex-store-link-functions
  '(org-bibtex-store-link)
  "Functions to override with `ignore' during note creation process.

Org Ref defines function `org-ref-bibtex-store-link' to store
links to a BibTeX buffer, e.g. with `org-store-link'.  At the
same time, Org ref requires `ol-bibtex' library, which defines
`org-bibtex-store-link' to do the same.  When creating a note
with `orb-edit-note' from a BibTeX buffer, for example by calling
`org-ref-open-bibtex-notes', the initiated `org-capture' process
implicitly calls `org-store-link'.  The latter loops through all
the functions for storing links, and if more than one function
can store links to the location, the BibTeX buffer in this
particular case, the user will be prompted to choose one.  This
is definitely annoying, hence ORB will advise all functions in
this list to return nil to trick `org-capture' and get rid of the
prompt.

The default value is `(org-bibtex-store-link)', which means this
function will be ignored and `org-ref-bibtex-store-link' will be
used to store a link to the BibTeX buffer.  See
`org-capture-templates' on how to use the link in your templates."
  :type '(repeat (function))
  :risky t
  :group 'org-roam-bibtex)

(defcustom orb-persp-project `("notes" . ,org-roam-directory)
  "Perspective name and path to the project with bibliography notes.
A cons cell (PERSP-NAME . PROJECT-PATH).  Only relevant when
`orb-switch-persp' is set to t.

PERSP-NAME should be a valid Perspective name, PROJECT-PATH should be
an open Projectile project.

See `orb-edit-note' for details"
  :type '(cons (string :tag "Perspective name")
               (directory :tag "Projectile directory"))
  :group 'org-roam-bibtex)

(defcustom orb-switch-persp nil
  "Non-nil to enable switching to the notes perspective.
Set the name of the perspective and the path to the notes project
in `orb-persp-project' for this to take effect.

Perspective switching works with Pers-mode and Projectile."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'org-roam-bibtex)


;; ============================================================================
;;; BibTeX fields and their special handling
;; ============================================================================

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


;; ============================================================================
;;; Orb edit notes
;; ============================================================================

(defun orb--switch-perspective ()
  "Helper function for `orb-edit-note'."
  (when (and (require 'projectile nil t)
             (require 'persp-mode nil t))
    (let ((notes-project (cdr orb-persp-project))
          (projects (projectile-relevant-open-projects))
          openp)
      (dolist (project projects openp)
        (setq openp (or (f-equal? project notes-project) openp)))
      (when openp
        (let ((p-names (cdr (persp-names))))
          (dolist (p-name p-names)
            (when (s-equals? p-name (car orb-persp-project))
              (persp-switch p-name))))))))

(defun orb--store-link-functions-advice (action)
  "Add or remove advice for each of `orb-ignore-bibtex-store-link-functions'.
ACTION should be a symbol `add' or `remove'.  A piece of advice
is the function `ignore', it is added as `:override'."
  (when orb-ignore-bibtex-store-link-functions
    (let ((advice-func (intern (format "advice-%s" action)))
          (advice (cl-case action
                    (add (list :override #'ignore))
                    (remove (list #'ignore))
                    (t (user-error "Action type not recognised: %s" action)))))
      (dolist (advisee orb-ignore-bibtex-store-link-functions)
        (apply advice-func (push advisee advice))))))

(defun orb--pre-expand-template (template entry)
  "Helper function for `orb--new-note'.
TEMPLATE is an element of `org-roam-capture-templates' and ENTRY
is a BibTeX entry as returned by `bibtex-completion-get-entry'."
  ;; Handle org-roam-capture part
  (letrec (;; Org-capture templates: handle different types of
           ;; org-capture-templates: string, file and function; this is
           ;; a stripped down version of `org-capture-get-template'
           (org-template
            (pcase (nth 3 template)       ; org-capture template is here
              (`nil 'nil)
              ((and (pred stringp) tmpl) tmpl)
              (`(file ,file)
               (let ((flnm (expand-file-name file org-directory)))
                 (if (file-exists-p flnm) (f-read-text flnm)
                   (format "Template file %S not found" file))))
              (`(function ,fun)
               (if (functionp fun) (funcall fun)
                 (format "Template function %S not found" fun)))
              (_ (user-error "ORB: Invalid capture template"))))
           ;;  org-roam capture properties are here
           (plst (cddddr template))
           ;; regexp for org-capture prompt wildcard
           (rx "\\(%\\^{[[:alnum:]-_]*}\\)")
           (file-keyword (when orb-process-file-keyword
                           (or (and (stringp orb-process-file-keyword)
                                    orb-process-file-keyword)
                               "file")))
           ;; inline function to handle :target list expansion
           (expand-roam-template
            (lambda (roam-template-list old new)
              (let (elements)
                (dolist (el roam-template-list)
                  (if (listp el)
                      (setq elements
                            (nreverse
                             (append elements
                                     (list (funcall expand-roam-template
                                                    el old new)))))
                    (push (s-replace old new el) elements)))
                (nreverse elements))))
           (lst nil))
    ;; First run:
    ;; 1) Make a list of (org-wildcard field-value match-position) for the
    ;; second run
    ;; 2) replace org-roam-capture wildcards
    (dolist (keyword orb-preformat-keywords)
      (let* (;; prompt wildcard keyword
             (keyword (cond
                       ;; for some backward compatibility with old
                       ;; `orb-preformat-keywords'
                       ((consp keyword) (car keyword))
                       ((stringp keyword) keyword)
                       (t (user-error "Error in `orb-preformat-keywords': \
Keyword \"%s\" has invalid type (string was expected)" keyword))))
             ;; bibtex field name
             (field-name (orb-resolve-field-alias keyword))
             ;; get the bibtex field value
             (field-value
              ;; maybe process file keyword
              (or (if (and file-keyword (string= field-name file-keyword))
                      (prog1
                          (orb-get-attached-file
                           (funcall orb-bibtex-entry-get-value-function "=key=" entry))
                        ;; we're done so don't even compare file-name with
                        ;; file-keyword in the successive cycles
                        (setq file-keyword nil))
                    ;; do the usual processing otherwise
                    ;; condition-case to temporary workaround an upstream bug
                    (condition-case nil
                        (funcall orb-bibtex-entry-get-value-function field-name entry)
                      (error "")))
                  ""))
             ;; org-capture prompt wildcard
             (org-wildcard (concat "%^{" (or keyword "citekey") "}"))
             ;; org-roam-capture prompt wildcard
             (roam-wildcard (concat "${" (or keyword "citekey") "}"))
             ;; org-roam-capture :target property
             (roam-template (or (plist-get plst :if-new) (plist-get plst :target)))
             (i 1)                      ; match counter
             pos)
        ;; Search for org-wildcard, set flag m if found
        (when org-template
          (while (string-match rx org-template pos)
            (if (string= (match-string 1 org-template) org-wildcard)
                (progn
                  (setq pos (length org-template))
                  (cl-pushnew (list org-wildcard field-value i) lst ))
              (setq pos (match-end 1)
                    i (1+ i)))))
        ;; Replace placeholders in org-roam-capture-templates :target property
        (when roam-template
          (setcdr roam-template
                  (funcall expand-roam-template
                           (cdr roam-template) roam-wildcard field-value)))))
    ;; Second run: replace prompts and prompt matches in org-capture
    ;; template string
    (dolist (l lst)
      (when (and org-template (nth 1 l))
        (let ((pos (concat "%\\" (number-to-string (nth 2 l)))))
          ;; replace prompt match wildcards with prompt wildcards
          ;; replace prompt wildcards with BibTeX field value
          (setq org-template (s-replace pos (car l) org-template)
                org-template (s-replace (car l) (nth 1 l) org-template))))
      (setf (nth 3 template) org-template))
    template))

(defun orb--new-note (citekey &optional props)
  "Process templates and run `org-roam-capture-'.
CITEKEY is the citation key of an entry for which the note is
created.  PROPS are additional properties for `org-roam-capture-'."
  ;; Check if the requested BibTeX entry actually exists and fail
  ;; gracefully otherwise
  (if-let* ((entry (or (bibtex-completion-get-entry citekey)
                       (orb-warning
                        "Could not find the BibTeX entry" citekey)))
            ;; Depending on the templates used: run
            ;; `org-roam-capture--capture' or call `org-roam-node-find'
            (org-capture-templates org-roam-capture-templates)
            ;; hijack org-capture-templates
            ;; entry is our bibtex entry, it just happens that
            ;; `org-capture' calls a single template entry "entry";
            (template (--> (if (null (cdr org-capture-templates))
                               ;; if only one template is defined, use it
                               (car org-capture-templates)
                             (org-capture-select-template))
                        (when (listp it)
                          (copy-tree it))
                        ;; optionally pre-expand templates
                        (if (and it orb-preformat-templates)
                            (orb--pre-expand-template it entry)
                          it)))
            ;; pretend we had only one template
            ;; `org-roam-capture--capture' behaves specially in this case
            ;; NOTE: this circumvents using functions other than
            ;; `org-capture', see `org-roam-capture-function'.
            ;; If the users start complaining, we may revert previous
            ;; implementation
            (org-roam-capture-templates (list template))
            ;; Org-roam coverts the templates to its own syntax;
            ;; since we are telling `org-capture' to use the template entry
            ;; (by setting `org-capture-entry'), and Org-roam converts the
            ;; whole template list, we must do the conversion of the entry
            ;; ourselves
            (props (--> (or props (list :finalize 'find-file))
                        (plist-put it :call-location (point-marker))))
            (org-capture-entry
             (org-roam-capture--convert-template template props))
            (citekey-ref (format
                          (pcase orb-roam-ref-format
                            ('org-ref-v2 "cite:%s")
                            ('org-ref-v3 "cite:&%s")
                            ('org-cite "@%s")
                            ((pred stringp) orb-roam-ref-format)
                            (_ (user-error "Invalid format `orb-roam-ref-format'")))
                          citekey))
            (title
             (or (funcall orb-bibtex-entry-get-value-function "title" entry)
                 (and
                  (orb-warning "Title not found for this entry")
                  ;; this is not critical, the user may input their own
                  ;; title
                  "No title")))
            (node (org-roam-node-create :title title)))
      (org-roam-capture-
       :node node
       :info (list :ref citekey-ref))
    (user-error "Abort")))

;;;###autoload
(defun orb-edit-note (citekey)
  "Open an Org-roam note associated with the CITEKEY or create a new one.

This function allows to use Org-roam as a backend for managing
bibliography notes.  It relies on `bibtex-completion' to get
retrieve bibliographic information from a BibTeX file.

Implementation details and features:

1. This function first calls `org-roam-find-ref' trying to find
the note file associated with the CITEKEY.  The Org-roam key can
be set with '#+ROAM_KEY:' in-buffer keyword.

2. If the Org-roam reference has not been found, the function
calls `org-roam-node-find' passing to it the title associated
with the CITEKEY as retrieved by `bibtex-completion-get-entry'.
The prompt presented by `org-roam-node-find' will thus be
pre-populated with the record title.

3. Optionally, when `orb-preformat-templates' is non-nil, any
prompt wildcards in `orb-templates' or
`org-roam-capture-templates', associated with the bibtex record
fields as specified in `orb-preformat-templates', will be
preformatted.  Both `org-capture-templates' (%^{}) and
`org-roam-capture-templates' (`s-format', ${}) prompt syntaxes
are supported.

See `orb-preformat-keywords' for more details on how
to properly specify prompts for replacement.

Please pay attention when using this feature that by setting
title for preformatting, it will be impossible to change it in
the `org-roam-node-find' interactive prompt since all the
template expansions will have taken place by then.  All the title
wildcards will be replace with the BibTeX field value.

4. Optionally, if you are using Projectile and Persp-mode and
have a dedicated workspace to work with your Org-roam collection,
you may want to set the perspective name and project path in
`orb-persp-project' and `orb-switch-persp' to t.  In this case,
the perspective will be switched to the Org-roam notes project
before calling any Org-roam functions.

If optional argument ENTRY is non-nil, use it to fetch the
bibliographic information."
  ;; Optionally switch to the notes perspective
  (when orb-switch-persp
    (orb--switch-perspective))
  (orb-make-notes-cache)
  (if-let ((node (orb-note-exists-p citekey)))
      (ignore-errors (org-roam-node-visit node))
    ;; fix some Org-ref related stuff
    (orb--store-link-functions-advice 'add)
    ;; TODO: consider using unwind-protect and let the errors through
    (condition-case error-msg
        (orb--new-note citekey)
      ((debug error)
       (orb--store-link-functions-advice 'remove)
       (message "%s"
                (concat (and (eq (car error-msg) 'error)
                             "orb-edit-note caught an error during capture: ")
                        (error-message-string error-msg)))))))

;; FIXME: this does not work anymore
;; (defun orb--get-non-ref-path-completions ()
;;   "Return a list of cons for titles of non-ref notes to absolute path.
;; CANDIDATES is a an alist of candidates to consider.  Defaults to
;; `org-roam--get-title-path-completions' otherwise."
;;   (let* ((rows (org-roam-db-query
;;                 [:select [titles:file titles:title tags:tags]
;;                  :from titles
;;                  :left :join tags
;;                  :on (= titles:file tags:file)
;;                  :left :join refs :on (= titles:file refs:file)
;;                  :where refs:file :is :null]))
;;          completions)
;;     (dolist (row rows completions)
;;       (pcase-let ((`(,file-path ,title ,tags) row))
;;         (let ((title (or title
;;                          (list (org-roam--path-to-slug file-path)))))
;;           (let ((k (concat
;;                     (when tags
;;                       (format "(%s) " (s-join org-roam-tag-separator tags)))
;;                     title))
;;                 (v (list :path file-path :title title)))
;;             (push (cons k v) completions)))))))


;; ============================================================================
;;; Orb insert
;; ============================================================================

(defvar orb-insert-lowercase nil
  "Internal.  Dynamic variable for `orb-insert-link' and `orb-insert--link'.")

(defun orb-insert--link (node info)
  "Insert a link to NODE.
INFO contains additional information."
  ;; citekey &optional description lowercase region-text beg end
  (-let (((&plist :region :orb-link-description :orb-citekey :orb-entry) info))
    (when region
      (org-roam-unshield-region (car region) (cdr region))
      (delete-region (car region) (cdr region))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil))
    (pcase orb-link-description
      ((pred stringp)
       (let ((description
              (-->
               (s-format orb-link-description
                         (lambda (template entry)
                           (funcall orb-bibtex-entry-get-value-function
                                    (orb-resolve-field-alias template) entry))
                         orb-entry)
               (if (and it orb-insert-lowercase) (downcase it) it))))
         (insert (org-link-make-string
                  (concat "id:" (org-roam-node-id node)) description))))
      (`citation-org-cite
       (insert (format "[cite:@%s]" orb-citekey)))
      (ref-format
       (let ((cite-link (if (boundp 'org-ref-default-citation-link)
                            (concat org-ref-default-citation-link ":")
                          "cite:")))
         (insert (concat cite-link
                         (when (eq ref-format 'citation-org-ref-3) "&")
                         orb-citekey)))))))

(defun org-roam-capture--finalize-orb-insert-link ()
  "Insert a link to a just captured note.
This function is used by ORB calls to `org-roam-capture-' instead
of `org-roam-capture--finalize-insert-link'."
  (let* ((mkr (org-roam-capture--get :call-location))
         (buf (marker-buffer mkr))
         (region (org-roam-capture--get :region))
         (node (org-roam-populate (org-roam-node-create :id (org-roam-capture--get :id))))
         (citekey (org-capture-get :orb-citekey))
         (entry (bibtex-completion-get-entry citekey)))
    (with-current-buffer buf
      (org-with-point-at mkr
        (orb-insert--link node (list
                                :region region
                                :orb-citekey citekey
                                :orb-entry entry
                                :orb-link-description (org-capture-get :orb-link-description)))))))

(defun orb--insert-captured-ref-h ()
  "Insert value of `:ref' key from `org-roam-capture--info'.
Internal function.  To be installed in `org-roam-capture-new-node-hook'."
  (when-let ((ref (plist-get org-roam-capture--info :ref)))
    (org-roam-ref-add ref)))

(defun orb-insert-edit-note (citekey)
  "Insert a link to a note with citation key CITEKEY.
Capture a new note if it does not exist yet.

CITEKEY can be a list of citation keys (for compatibility with
Bibtex-completion), in which case only the first element of that
list is used."
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((citekey (cl-typecase citekey
                          (string citekey)
                          (list (car citekey))
                          (t (user-error "Invalid citation key data type: %s.  \
String or list of strings expected" citekey))))
               (entry (bibtex-completion-get-entry citekey))
               (title
                (funcall orb-bibtex-entry-get-value-function "title" entry ""))
               (node (or (orb-note-exists-p citekey)
                         (org-roam-node-create :title title)))
               region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text
                          (buffer-substring-no-properties beg end))))
               (description (--> orb-insert-link-description
                                 (cl-case it
                                   (title "${title}")
                                   (citekey "${citekey}")
                                   (t it))
                                 (or region-text it)))
               (info (--> (list :orb-link-description description
                                :orb-citekey citekey
                                :orb-entry entry
                                :finalize 'orb-insert-link)
                          (if (and beg end)
                              (append it (list :region (cons beg end)))
                            it))))
          (if (org-roam-node-id node)
              (orb-insert--link node info)
            (orb--new-note citekey info)))
        (deactivate-mark)))
  (when (and orb-insert-follow-link
             (looking-at org-link-any-re))
    (org-open-at-point)))

(defun orb-insert-generic (&optional arg)
  "Present a list of BibTeX entries for completion.
This is a generic completion function for `orb-insert-link', which
runs `orb-insert-edit-note' on the selected entry.  The list is
made by `bibtex-completion-candidates'.

The appearance of selection candidates is determined by
`orb-insert-generic-candidates-format'.

This function is not interactive, set `orb-insert-interface' to
`generic' and call `orb-insert-link' interactively instead.

If ARG is non-nil, rebuild `bibtex-completion-cache'."
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (candidates2
          (if (eq orb-insert-generic-candidates-format 'key)
              (mapcar (lambda (item)
                        (alist-get "=key=" (cdr item) nil nil #'equal))
                      candidates)
            (mapcar #'car candidates)))
         (selection (completing-read "BibTeX entry:" candidates2 nil t))
         (citekey (if (eq orb-insert-generic-candidates-format 'key)
                      selection
                    (--> (alist-get selection candidates nil nil #'equal)
                         (cdr it)
                         (alist-get "=key=" it  nil nil #'equal)))))
    (orb-insert-edit-note citekey)))

;;;###autoload
(defun orb-insert-link (&optional arg)
  "Insert a link to an Org-roam bibliography note.
If the note does not exist yet, it will be created using
`orb-edit-note' function.

\\<universal-argument-map>\\<org-roam-bibtex-mode-map> The
customization option `orb-insert-link-description' determines
what will be used as the link's description.  It is possible to
override the default value of the variable with a numerical
prefix ARG:

`C-1' \\[orb-insert-link] will force `title'
`C-2' \\[orb-insert-link] will force `citekey'

`C-0' \\[orb-insert-link] will force `citation-org-ref-2'
`C-9' \\[orb-insert-link] will force `citation-org-ref-3'
`C-8' \\[orb-insert-link] will force `citation-org-cite'

If a region of text is active (selected) when calling `orb-insert-link',
the text in the region will be replaced with the link and the
text string will be used as the link's description — similar to
`org-roam-node-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied.

The customization option `orb-insert-interface' allows to set the
completion interface backend for the candidates list."
  (interactive "P")
  ;; parse arg
  ;; C-u or C-u C-u C-u => force lowercase
  ;; C-u C-u or C-u C-u C-u => force `bibtex-completion-clear-cache'
  ;; C-1 force title in description
  ;; C-2 force citekey in description
  ;; C-0,C-9,C-8 force inserting the link as Org-ref org Org-cite citation
  (let* ((lowercase (or (equal arg '(4))
                        (equal arg '(64))))
         (clear-cache (or (equal arg '(16))
                          (equal arg '(64))))
         (link-type (cl-case arg
                      (1 'title)
                      (2 'citekey)
                      (0 'citation-org-ref-2)
                      (9 'citation-org-ref-3)
                      (8 'citation-org-cite)
                      (t nil)))
         (orb-insert-link-description
          (or link-type orb-insert-link-description))
         (orb-insert-lowercase (or lowercase orb-insert-lowercase)))
    (orb-make-notes-cache)
    (cl-case orb-insert-interface
      (helm-bibtex
       (cond
        ((fboundp 'orb-helm-insert)
         (orb-helm-insert clear-cache))
        (t
         (orb-warning "helm-bibtex not available; using generic completion")
         (orb-insert-generic clear-cache))))
      (ivy-bibtex
       (cond
        ((fboundp 'orb-ivy-insert)
         (orb-ivy-insert clear-cache))
        (t
         (orb-warning "ivy-bibtex not available; using generic completion")
         (orb-insert-generic clear-cache))))
      (t
       (orb-insert-generic clear-cache)))))


;; ============================================================================
;;; Non-ref functions
;; ============================================================================

;; ;;;###autoload
;; (defun orb-find-non-ref-file (&optional initial-prompt)
;;   "Find and open an Org-roam, non-ref file.
;; INITIAL-PROMPT is the initial title prompt.
;; See `org-roam-node-finds' and
;; `orb--get-non-ref-path-completions' for details."
;;   (interactive)
;;   (org-roam-node-find initial-prompt
;;                       (orb--get-non-ref-path-completions)))

;; ;;;###autoload
;; (defun orb-insert-non-ref ()
;;   "Find a non-ref Org-roam file, and insert a relative org link to it at point.
;; If PREFIX, downcase the title before insertion.  See
;; `org-roam-insert' and `orb--get-non-ref-path-completions' for
;; details."
;;   (interactive)
;;   ;; FIXME: this is not correct
;;   (org-roam-node-insert (orb--get-non-ref-path-completions)))


;; ============================================================================
;;; Orb note actions
;; ============================================================================

(orb-note-actions-defun default
  (let ((f (cdr (assoc (completing-read name candidates) candidates))))
    (funcall f (list citekey))))

(orb-note-actions-defun ido
  (let* ((c (cl-map 'list 'car candidates))
         (f (cdr (assoc (ido-completing-read name c) candidates))))
    (funcall f (list citekey))))

(declare-function orb-note-actions-hydra/body "org-roam-bibtex" nil t)
(orb-note-actions-defun hydra
  ;; we don't use candidates here because for a nice hydra we need each
  ;; group of completions separately (default, extra, user), so just
  ;; silence the compiler
  (ignore candidates)
  (let ((k ?a)
        actions)
    (dolist (type (list "Default" "Extra" "User"))
      (let ((actions-var
             (intern (concat "orb-note-actions-" (downcase type)))))
        (dolist (action (symbol-value actions-var))
          ;; this makes defhydra HEADS list of the form:
          ;; ("a" (some-action citekey-value) "Some-action description"
          ;;     :column "Type")
          (cl-pushnew
           `(,(format "%c" k) (,(cdr action) (list ,citekey))
             ,(car action) :column ,(concat type " actions"))
           actions)
          ;; increment key a->b->c...
          (setq k (1+ k)))))        ; TODO: figure out a way to supply
                                        ; mnemonic keys
    (setq actions (nreverse actions))
    ;; yes, we redefine hydra on every call
    (eval
     `(defhydra orb-note-actions-hydra (:color blue :hint nil)
        ;; defhydra docstring
        ,(format  "^\n  %s \n\n^"
                  (s-word-wrap (- (window-body-width) 2) name))
        ;; defhydra HEADS
        ,@actions)))
  (orb-note-actions-hydra/body))

(defun orb-note-actions--run (interface citekey )
  "Run note actions on CITEKEY with INTERFACE."
  (when (and (memq interface '(ivy helm hydra))
             (not (featurep interface)))
    (orb-warning
     (format "Feature `%s' not available, using default interface" interface))
    (setq interface 'default))
  (funcall (intern (concat "orb-note-actions-" (symbol-name interface)))
           citekey))

;;;###autoload
(defun orb-note-actions ()
  "Run an interactive prompt to offer note-related actions.
The prompt interface can be set in `orb-note-actions-interface'.
In addition to default actions, which are not supposed to be
modified, there is a number of prefined extra actions
`orb-note-actions-extra' that can be customized.  Additionally,
user actions can be set in `orb-note-actions-user'."
  (interactive)
  (if-let ((non-default-interfaces (list 'hydra 'ido 'ivy 'helm))
           (citekey (orb-get-node-citekey nil 'assert)))
      (cond ((memq orb-note-actions-interface non-default-interfaces)
             (orb-note-actions--run orb-note-actions-interface citekey))
            ((functionp orb-note-actions-interface)
             (funcall orb-note-actions-interface citekey))
            (t
             (unless (eq orb-note-actions-interface 'default)
               (orb-warning
                (format "Feature `%s' not available, using default interface"
                        orb-note-actions-interface)))
             (orb-note-actions--run 'default citekey)))
    (user-error "Could not retrieve the citekey.  Check ROAM_REFS property \
of current node")))

(defun orb-note-actions-copy-citekey (citekey)
  "Save note's citation key to `kill-ring' and copy it to clipboard.
CITEKEY is a list whose car is a citation key."
  (with-temp-buffer
    (insert (car citekey))
    (copy-region-as-kill (point-min) (point-max))))

(defun orb-note-actions-scrape-pdf (citekey)
  "Wrapper around `refuse-run'.
CITEKEY is a list whose car is a citation key."
  (require 'refuse)
  (refuse-run (car citekey)))


;; ============================================================================
;;; Plug-in functions for other packages
;; ============================================================================

(defvar orb--external-vars-original-values nil
  "Variable to hold original values of variables from external packages.
Internal use.")

;;;###autoload
(defun orb-org-ref-edit-note (citekey)
  "Open an Org-roam note associated with the CITEKEY or create a new one.
Set `org-ref-notes-function' to this function if your
bibliography notes are managed by Org-roam and you want some
extra integration between the two packages.

This is a wrapper function around `orb-edit-note' intended for
use with Org-ref.

NOTE: This function is no longer needed for Org-ref v3."
  (when (require 'org-ref nil t)
    (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
      (orb-edit-note citekey))))

;;;###autoload
(defun orb-citar-edit-note (citekey _entry)
  "Open an Org-roam note associated with the CITEKEY or create a new one.
This is a wrapper function around `orb-edit-note' meant to be used with
`citar-file-open-note-function'.
Argument ENTRY is ignored."
  (orb-edit-note citekey))

;;;###autoload
(defun orb-bibtex-completion-edit-note (keys)
  "Open or create an Org-roam note.

This is a wrapper function around `orb-edit-note' meant to be
used with `bibtex-completion-edit-notes-function'.

Only the first KEY of the list KEYS will actually be used.  KEY
must be a string."
  (orb-edit-note (car keys)))

(provide 'orb-core)
;;; orb-core.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
