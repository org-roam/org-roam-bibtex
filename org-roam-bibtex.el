;;; org-roam-bibtex.el --- Org Roam meets BibTeX -*- lexical-binding: t -*-

;; Copyright © 2020-2021 Mykhailo Shevchuk
;; Copyright © 2020 Leo Vivier

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;      Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: bib, hypermedia, outlines, wp
;; Version: 0.6.0
;; Package-Requires: ((emacs "27.2") (org-roam "2.0.0") (bibtex-completion "2.0.0") (org-ref "1.1.1"))

;; Soft dependencies: projectile, persp-mode, helm, ivy, hydra

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
;; This library offers an integration between Bibtex-completion and
;; Org-roam by delegating the tasks of note's creation, editing and
;; retrieval to Org-roam.  From the Org-roam's perspective, the
;; library provides a means to populate Org-roam templates with
;; bibliographic information secured through Bibtex-completion,.
;;
;; To use it:
;;
;; call interactively `org-roam-bibtex-mode' or call (org-roam-bibtex-mode +1)
;; from Lisp.
;;
;; After enabling `org-roam-bibtex-mode', the function `orb-edit-notes' will be
;; used as `bibtex-completion-edit-notes-function' in Helm-bibtex, Ivy-bibtex.
;;
;; Additionally, `orb-org-ref-edit-note', which is a simple wrapper around
;; `orb-edit-note', is installed as Org-ref's `org-ref-notes-function'.  See
;; Org-ref's documentation for how to setup many-files notes.  Take a notice
;; that because of its size, Org-ref is not a dependency of Org Roam BibTeX, so
;; it will not be pulled automatically by your package manager and must be
;; installed manually.
;;
;; As a user option, `org-roam-capture-templates' can be dynamically
;; pre-expanded with BibTeX field values.  See `orb-preformat-keywords' for
;; more details.
;;
;; Optionally, automatic switching to the perspective (Persp-mode) with the
;; notes project (Projectile) is possible.  See `orb-edit-note' for more
;; details.
;;

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'orb-core)

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib)
  (require 'cl-macs))

;; declare own functions and variables
(declare-function orb-helm-insert "orb-helm")
(declare-function orb-note-actions-helm "orb-helm")
(declare-function orb-ivy-insert "orb-ivy")
(declare-function orb-note-actions-ivy "orb-ivy")

;; declare external functions and variables

(declare-function projectile-relevant-open-projects "projectile")
(declare-function persp-switch "persp-mode")
(declare-function persp-names "persp-mode")

(defvar org-ref-notes-function)
(declare-function org-ref-find-bibliography "org-ref-core")

(declare-function defhydra "ext:hydra")
(declare-function org-ref-format-entry "ext:org-ref-bibtex" (key))

(declare-function orb-pdf-scrapper-run "orb-pdf-scrapper" (key))

;; ============================================================================
;;;; Customize definitions
;; ============================================================================

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
         :if-new
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

The variable `orb-file-field-extensions' controls filtering of
file names based on file extensions."
  ;; TODO: check if a custom string is really working as described
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)
          (string :tag "Custom wildcard keyword")))

(defcustom orb-citekey-format "cite:%s"
  "Format string for the citation key.
This string will be prepended to the citation key to obtained an
Org-ref citation, which will be then set in :ROAM_REFS: property."
  :type 'string
  :group 'org-roam-bibtex)

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

(defcustom orb-insert-interface 'generic
  "Interface frontend to use with `orb-insert-link'.
Possible values are the symbols `helm-bibtex', `ivy-bibtex' and
`generic'.  In the first two cases the respective commands will
be used, while in the latter case the command
`orb-insert-generic' will be used."
  :group 'org-roam-bibtex
  :type '(choice
          (const helm-bibtex)
          (const ivy-bibtex)
          (const generic)))

(defcustom orb-insert-link-description 'title
  "Link description source for links created with `orb-insert-link'.
Possible values are the symbols `title', `citekey' and
`citation'.  When the value of this variable is `title' or
`citekey', then the title of the note the link points to or
respectively the citekey associated with it will be used as the
link's description:

[[file:path/to/note.org][title]] or [[file:path/to/note.org][citekey]]

When the value of this variable is `citation', instead of an
Org-mode link create an Org-ref link by appending the citation
key to `org-ref-default-citation-link' \(with a colon inbetween)
or \"cite:\", if the latter variable is not defined, for example
when Org-ref is not loaded.

The default value set by this variable can be overriden by
calling `orb-insert-link' with an appropriated numerical prefix
argument.  See the docstring of the function for more
information."
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Title" title)
          (const :tag "Citation key" citekey)
          (const :tag "Citation link" citation)))

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

(defcustom orb-note-actions-interface 'default
  "Interface frontend for `orb-note-actions'.
Supported values (interfaces) are 'default, 'ido, 'hydra, 'ivy and 'helm.

Alternatively, it can be set to a function, in which case the
function should expect one argument CITEKEY, which is a list
whose car is the citation key associated with the org-roam note
the current buffer is visiting.  Also, it should ideally make use
of `orb-note-actions-default', `orb-note-actions-extra' and
`orb-note-actions-user' for providing an interactive interface,
through which the combined set of note actions is presented as a
list of candidates and the function associated with the candidate
is executed upon selecting it."
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
  '(("Open PDF file(s)" . bibtex-completion-open-pdf)
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
    ("Run Orb PDF Scrapper" . orb-note-actions-scrap-pdf))
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

;; ============================================================================
;;;; Orb edit notes
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
           ;; inline function to handle :if-new list expansion
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
             (field-name (or (car (rassoc keyword orb-bibtex-field-aliases))
                             keyword))
             ;; get the bibtex field value
             (field-value
              ;; maybe process file keyword
              (or (if (and file-keyword (string= field-name file-keyword))
                      (prog1
                          (orb-process-file-field
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
             ;; org-roam-capture :if-new property
             (roam-template (plist-get plst :if-new))
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
        ;; Replace placeholders in org-roam-capture-templates :if-new property
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
            (citekey-formatted (format (or orb-citekey-format "%s") citekey))
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
       :info (list :ref citekey-formatted))
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
before calling any Org-roam functions."
  ;; Optionally switch to the notes perspective
  (when orb-switch-persp
    (orb--switch-perspective))
  (orb-make-notes-cache)
  (if-let ((node (orb-note-exists-p citekey)))
      ;; Find org-roam reference with the CITEKEY and collect data into
      ;; `orb-plist'
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
;;;; Orb insert
;; ============================================================================
(defvar orb-plist nil
  "Communication channel for `orb-edit-note' and related functions.")

(defun orb-plist-put (&rest props)
  "Add properties PROPS to `orb-plist'.
Returns the new plist."
  (while props
    (setq orb-plist (plist-put orb-plist (pop props) (pop props)))))

(defun orb-plist-get (prop)
  "Get PROP from `orb-plist'."
  (plist-get orb-plist prop))

(defun orb-insert--link (node info)
  "Insert a link to NODE.
INFO contains additional information."
  ;; citekey &optional description lowercase region-text beg end
  (-let (((&plist :region :orb-link-description :orb-citekey) info))
    (when region
      (org-roam-unshield-region (car region) (cdr region))
      (delete-region (car region) (cdr region))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil))
    (if orb-link-description
        (insert (org-link-make-string
                 (concat "id:" (org-roam-node-id node)) orb-link-description))
      (let ((cite-link (if (boundp 'org-ref-default-citation-link)
                           (concat org-ref-default-citation-link ":")
                         "cite:")))
        (insert (concat cite-link orb-citekey))))))

(defun org-roam-capture--finalize-orb-insert-link ()
  "Insert a link to a just captured note.
This function is used by ORB calls to `org-roam-capture-' instead
of `org-roam-capture--finalize-insert-link'."
  (let* ((mkr (org-roam-capture--get :call-location))
         (buf (marker-buffer mkr))
         (region (org-roam-capture--get :region))
         (node (org-roam-populate (org-roam-node-create :id (org-roam-capture--get :id)))))
    (with-current-buffer buf
      (org-with-point-at mkr
        (orb-insert--link node (list
                                :region region
                                :orb-citekey (org-capture-get :orb-citekey)
                                :orb-link-description (org-capture-get :orb-link-description)))))))

(defvar orb-insert-lowercase nil)

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
               (title
                (funcall orb-bibtex-entry-get-value-function
                 "title" (bibtex-completion-get-entry citekey) ""))
               (node (or (orb-note-exists-p citekey)
                         (org-roam-node-create :title title)))
               region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text
                          (buffer-substring-no-properties beg end))))
               (lowercase (or (orb-plist-get :link-lowercase)
                              orb-insert-lowercase))
               (description (--> (or (orb-plist-get :link-type)
                                     orb-insert-link-description)
                                 (cl-case it
                                   (title (org-roam-node-title node))
                                   (citekey citekey)
                                   (citation nil))
                                 (or region-text it)
                                 (if (and it lowercase) (downcase it) it)))
               (info (--> (list :orb-link-description description
                                :orb-citekey citekey
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
override the default value with numerical prefix ARG:

`C-1' \\[orb-insert-link] will force `title'
`C-2' \\[orb-insert-link] will force `citekey'
`C-0' \\[orb-insert-link] will force `citation'

If a region of text is active (selected) when calling `orb-insert-link',
the text in the region will be replaced with the link and the
text string will be used as the link's description — similar to
`org-roam-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied.

The customization option `orb-insert-interface' allows to set the
completion interface backend for the candidates list.  Available
interfaces are `helm-bibtex', `ivy-bibtex' and `orb-insert-generic'.

With `helm-bibtex' or `ivy-bibtex', choosing the action \"Edit
note & insert a link\" will insert the desired link.  For
convenience, this action is made default for the duration of an
`orb-insert-link' session.  It will not persist when `helm-bibtex' or
`ivy-bibtex' proper are run.  It is possible to run other
`helm-bibtex' or `ivy-bibtex' actions.  When action other than
\"Edit note & insert a link\" is run, no link will be inserted,
although the session can be resumed later with `helm-resume' or
`ivy-resume', respectively, where it will be possible to select
the \"Edit note & insert a link\" action.

When using `orb-insert-generic', a simple list of available
citation keys is presented using `completion-read' and after
choosing a candidate the appropriate link will be inserted."
  (interactive "P")
  ;; parse arg
  ;; C-u or C-u C-u C-u => force lowercase
  ;; C-u C-u or C-u C-u C-u => force `bibtex-completion-clear-cache'
  ;; C-1 force title in description
  ;; C-2 force citekey in description
  ;; C-3 force inserting the link as Org-ref citation
  (let* ((lowercase (or (equal arg '(4))
                        (equal arg '(64))))
         (link-type (cl-case arg
                        (1 'title)
                        (2 'citekey)
                        (0 'citation)))
         (clear-cache (or (equal arg '(16))
                          (equal arg '(64)))))
    (orb-plist-put :link-type
                   (or link-type orb-insert-link-description)
                   :link-lowercase
                   (or lowercase orb-insert-lowercase))
    (orb-make-notes-cache)
    (cl-case orb-insert-interface
      (helm-bibtex
       (cond
        ((featurep 'helm-bibtex)
         (orb-helm-insert clear-cache))
        (t
         (orb-warning "helm-bibtex not available; using generic completion")
         (orb-insert-generic clear-cache))))
      (ivy-bibtex
       (cond
        ((featurep 'ivy-bibtex)
         (require 'orb-ivy)
         (orb-ivy-insert clear-cache))
        (t
         (orb-warning "ivy-bibtex not available; using generic completion")
         (orb-insert-generic clear-cache))))
      (t
       (orb-insert-generic clear-cache)))))

;; ============================================================================
;;;; Non-ref functions
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
;;;; Orb note actions
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
           ;; FIXME: this does not work anymore
           ;; (citekey (cdr (org-roam--extract-ref)))
           (citekey (orb-get-node-citekey)))
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

;;;;;; Note actions

(defun orb-note-actions-copy-citekey (citekey)
  "Save note's citation key to `kill-ring' and copy it to clipboard.
CITEKEY is a list whose car is a citation key."
  (with-temp-buffer
    (insert (car citekey))
    (copy-region-as-kill (point-min) (point-max))))

(defun orb-note-actions-scrap-pdf (citekey)
  "Wrapper around `orb-pdf-scrapper-insert'.
CITEKEY is a list whose car is a citation key."
  (require 'orb-pdf-scrapper)
  (orb-pdf-scrapper-run (car citekey)))

;; ============================================================================
;;;; Org-roam-bibtex minor mode
;; ============================================================================
;;

;;;###autoload
(defun orb-org-ref-edit-note (citekey)
  "Open an Org-roam note associated with the CITEKEY or create a new one.
Set `org-ref-notes-function' to this function if your
bibliography notes are managed by Org-roam and you want some
extra integration between the two packages.

This is a wrapper function around `orb-edit-note'
intended for use with Org-ref."
  (when (require 'org-ref nil t)
    (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
      (orb-edit-note citekey))))

;;;###autoload
(defun orb-edit-notes (keys)
  "Open or create an Org-roam note associated with the first key from KEYS.
This function replaces `bibtex-completion-edit-notes'.  Only the
first key from KEYS will actually be used."
  (orb-edit-note (car keys)))

(defvar org-roam-bibtex-mode-map
  (make-sparse-keymap)
  "Keymap for `org-roam-bibtex-mode'.")

;;;###autoload
(define-minor-mode org-roam-bibtex-mode
  "Sets `orb-edit-note' as a function for editing bibliography notes.
Affects Org-ref and Helm-bibtex/Ivy-bibtex.

When called interactively, toggle `org-roam-bibtex-mode'. with
prefix ARG, enable `org-roam-bibtex-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `org-roam-bibtex-mode' if ARG is
omitted, nil, or positive.  If ARG is `toggle', toggle
`org-roam-bibtex-mode'.  Otherwise, behave as if called
interactively."
  :lighter " orb"
  :keymap  org-roam-bibtex-mode-map
  :group 'org-roam-bibtex
  :require 'orb
  :global t
  (cond (org-roam-bibtex-mode
         (setq org-ref-notes-function 'orb-org-ref-edit-note)
         (add-to-list 'bibtex-completion-find-note-functions
                      #'orb-find-note-file)
         (setq bibtex-completion-edit-notes-function #'orb-edit-notes)
         (add-hook 'org-capture-after-finalize-hook #'orb-make-notes-cache)
         (orb-make-notes-cache))
        (t
         (setq org-ref-notes-function 'org-ref-notes-function-one-file)
         (setq bibtex-completion-find-note-functions
               (delq #'orb-find-note-file
                     bibtex-completion-find-note-functions))
         (setq bibtex-completion-edit-notes-function
               #'bibtex-completion-edit-notes-default)
         (remove-hook 'org-capture-after-finalize-hook
                        #'orb-make-notes-cache))))

(provide 'org-roam-bibtex)

;;; org-roam-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
