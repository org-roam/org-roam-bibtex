;;; org-roam-bibtex.el --- Org Roam meets BibTeX -*- lexical-binding: t -*-

;; Copyright © 2020-2021 Mykhailo Shevchuk
;; Copyright © 2020 Leo Vivier

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;      Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: bib, hypermedia, outlines, wp
;;
;; Version: 0.5.2
;; Package-Requires: ((emacs "27.2") (org-roam "1.2.3") (bibtex-completion "2.0.0") (org-ref "1.1.1"))

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

(when (featurep 'ivy-bibtex)
  (require 'orb-ivy))

(when (featurep 'helm-bibtex)
  (require 'orb-helm))

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

;; declare own functions and variables
(declare-function orb-helm-insert "orb-helm")
(declare-function orb-ivy-insert "orb-ivy")

;; declare external functions and variables

(defvar bibtex-completion-bibliography)
(defvar bibtex-completion-find-note-functions)
(defvar bibtex-completion-edit-notes-function)

(declare-function bibtex-completion-apa-get-value
                  "bibtex-completion" (field entry &optional default))
(declare-function bibtex-completion-get-entry
                  "bibtex-completion" (entry-key))
(declare-function bibtex-completion-clear-cache
                  "bibtex-completion" (&optional files))
(declare-function bibtex-completion-init "bibtex-completion")
(declare-function bibtex-completion-candidates "bibtex-completion")

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

(defcustom orb-include-citekey-in-titles nil
  "Non-nil to include the citekey in titles.
See `orb-edit-note' for details."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'org-roam-bibtex)

(defcustom orb-preformat-keywords
  '("citekey" "entry-type" "date" "pdf?" "note?" "file"
    "author" "editor" "author-abbrev" "editor-abbrev"
    "author-or-editor-abbrev")
  "A list of template prompt wildcards for pre-expanding.
Any BibTeX field can be set for pre-expanding including
`bibtex-completion` \"virtual\" fields such as '=key=' and
'=type='.  BibTeX fields can be refered to by means of their
aliases defined in `orb-bibtex-field-aliases'.

Usage example:

\(setq orb-preformat-keywords '(\"citekey\" \"author\" \"date\"))
\(setq orb-templates
      '((\"r\" \"reference\" plain (function org-roam-capture--get-point)
         \"#+ROAM_KEY: %^{citekey}%?
%^{author} published %^{entry-type} in %^{date}: fullcite:%\\1.\"
         :file-name \"references/${citekey}\"
         :head \"#+TITLE: ${title}\"
         :unnarrowed t)))

Special cases:

The \"file\" keyword will be treated specially if the value of
`orb-process-file-keyword' is non-nil.  See its docstring for an
explanation.

The \"title\" keyword needs not to be set for pre-expanding if it
is used only within the `:head` section of the templates.

This variable takes effect when `orb-preformat-templates' is set
to t (default). See also `orb-edit-note' for further details.

Consult bibtex-completion package for additional information
about BibTeX field names."
  :type '(repeat :tag "BibTeX field names" string)
  :group 'org-roam-bibtex)

(defcustom orb-process-file-keyword t
  "Whether to treat the file keyword specially during template pre-expanding.
When this variable is non-nil, the \"%^{file}\" and \"${file}\"
wildcards will be expanded by `org-process-file-field' rather
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

The variable `orb-file-field-extensions' controls which filtering
of the file names based on file extensions.

See also `orb-file-field-extensions' for filtering file names
based on their extension."
  :group 'org-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)
          (string :tag "Custom wildcard keyword")))

(defcustom orb-citekey-format "cite:%s"
  "Format string for the citekey.
The citekey obtained from Helm-bibtex/Ivy-bibtex/Org-ref
will be formatted as specified here."
  :type 'string
  :group 'org-roam-bibtex)

(defcustom orb-slug-source 'citekey
  "What should be used as a source for creating the note's slug.
Supported values are symbols `citekey' and `title'.

A special variable `${slug}` in `orb-templates' (and
`org-roam-capture-templates') is used as a placeholder for an
automatically generated string which is meant to be used in
filenames.  Org Roam uses the note's title to create a slug.  ORB
also allows for the citekey.  The function specified in
`org-roam-title-to-slug-function' is used to create the slug.
This operation typilcally involves removing whitespace and
converting words to lowercase, among possibly other things."
  :type '(choice
          (const citekey)
          (const title))
  :group 'org-roam-bibtex)

(defcustom orb-persp-project `("notes" . ,org-roam-directory)
  "Perspective name and path to the project with bibliography notes.
A cons cell (PERSP-NAME . PROJECT-PATH).  Only relevant when
`orb-switch-persp' is set to t.

Requires command `persp-mode' and command `projectile-mode'.

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

Requires command `persp-mode' and command `projectile-mode'.

See `orb-edit-note' for details."
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
with `orb-edit-note' from a BibTeX buffer, for example by
calling `org-ref-open-bibtex-notes', the initiated `org-capture'
process implicitly calls `org-store-link'.  The latter loops
through all the functions for storing links, and if more than one
function can store links to the location, the BibTeX buffer in
this particular case, the user will be prompted to choose one.
This is definitely annoying, hence ORB will advise all functions
in this list to return nil to trick `org-capture' and get rid of
the prompt.

The default value is `(org-bibtex-store-link)', which means this
function will be ignored and `org-ref-bibtex-store-link' will be
used to store a link to the BibTeX buffer.  See
`org-capture-templates' on how to use the link in your templates."
  :type '(repeat (function))
  :risky t
  :group 'org-roam-bibtex)

(defcustom orb-insert-interface 'generic
  "Interface frontend to use with `orb-insert'.
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
  "What should be used as link description for links created with `orb-insert'.
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
calling `orb-insert' with an appropriated numerical prefix
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
  "Format of selection candidates for `orb-insert' with `generic' interface.
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
;;;; Orb plist
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

(defun orb-cleanup ()
  "Clean up `orb-plist'."
  (let ((keywords (-filter #'keywordp orb-plist)))
    (dolist (keyword keywords)
      (orb-plist-put keyword nil))))

(defmacro with-orb-cleanup (&rest body)
  "Execute BODY calling `orb-cleanup' as its last form.
Return the result of executing BODY."
  (declare (indent 0) (debug t))
  `(prog1
       ,@body
     (orb-cleanup)))

;; ============================================================================
;;;; Managing Org-capture hooks (experimental)
;; ============================================================================

;; workflow:
;;
;; 1. (orb-register-hook-function func before nil (my-forms))
;;
;; 2. In `orb-edit-note', if a note does not exist, an `org-capture' process is
;; started.  Before that, the function `func' is added
;; `org-capture-before-finalize-hook' with 0 depth by calling
;; `orb-do-hook-functions'.  Forms (my-forms) will be run by `org-capture'
;; within the hook.  The closure removes itself, so that it does not interfere
;; with any subsequent `org-capture' calls.  `orb-edit-note' also takes care
;; to remove the function from the hook in case the `org-capture' process was
;; aborted.
;;
;; 3. If the note exists, the function will not be added to the hook, but it is
;; still possible to execute `my-forms' by calling:
;;
;; (orb-call-hook-function 'func)
;;
;; 4. The function is registered until `orb-clean' is run.

(defmacro orb-register-hook-function (name target depth &rest body)
  "Register a closure to be run in one of the `org-capture' hooks.
NAME (unquoted) is the name of the function.  TARGET should be an
unquoted SYMBOL, one of `prepare', `before' or `after', meaning
the function will be registered to run with the corresponding
`org-capture-SYMBOL-finlaize-hook'.  DEPTH is the hook depth, nil
is internally converted to 0.

BODY are forms which will be wrapped in an anonymous function
within a `letrec' form.  Additionally, a `remove-hook' called is
appended to BODY, making the closure self-removable:

\(letrec ((NAME
           (lambda ()
            BODY
           (remove-hook 'org-capture-TARGET-finalize-hook NAME)))))

These hook functions are therefore meant to run only in next
`org-capture' session.

The function is not actually added to a hook but is instead
registered on `orb-plist'.  The function `orb-edit-note'
installs the hooks just before starting an `org-capture' process
by calling `orb-do-hook-functions'.  It also takes care of
removing the hooks in case the `org-capture' process was aborted.

After a function has been registered, it is possible to call it
by passing its NAME as a quoted symbol to
`orb-call-hook-function'.  This may be useful if the function
should be run regardless of whether an `org-capture' process was
initiated or not."
  (declare (indent 3) (debug t))
  (let ((hookvar (intern (format "org-capture-%s-finalize-hook" target)))
        (keyword (intern (format ":%s-functions" target)))
        (depth (or depth 0)))
    `(letrec ((,name (lambda () ,@body (remove-hook (quote ,hookvar) ,name))))
       (orb-plist-put ,keyword
                      (cons (list (quote ,name) ,name ,depth)
                            (orb-plist-get ,keyword))))))

(defun orb-call-hook-function (name)
  "Call function NAME registered by `orb-register-hook-function'."
  (let* ((functions (append (orb-plist-get :prepare-functions)
                            (orb-plist-get :before-functions)
                            (orb-plist-get :after-functions)))
         (func (alist-get name functions)))
    (when func (funcall (car func)))))

(defun orb-do-hook-functions (action &optional targets)
  "Add or remove functions to `org-capture-...-finilize-hook's.
ACTION should be a symbol `add' or `remove'.  If optional TARGETS
list is provided, do only the hooks in TARGETS.  TARGETS should
be any of symbols `prepare', `before' and `after'.  TARGETS can
also be a single symbols.  If TARGETS is nil, a list of all three
symbols is implied."
  (let* ((targets (--> (if (and targets (not (listp targets)))
                           (listp targets)
                         targets)
                       ;; filter irrelevant symbols,
                       ;; or if targets were nil, make all targets
                       (or (-intersection it '(prepare before after))
                           '(prepare before after)))))
    (dolist (target targets)
      (let ((functions (sort (orb-plist-get
                              (intern (format ":%s-functions" target)))
                             (lambda (a b)
                               (> (nth 2 a) (nth 2 b))))))
        (dolist (func functions)
          (let ((f (intern (format "%s-hook" action)))
                (hook (intern (format "org-capture-%s-finalize-hook" target))))
            (funcall f hook (when (eq action 'add)
                              (nth 1 func)))))))))

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
  (let* (;; Org-capture templates: handle different types of
         ;; org-capture-templates: string, file and function; this is
         ;; a stripped down version of `org-capture-get-template'
         (tp
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
         lst)
    ;; First run:
    ;; 1) Make a list of (rplc-s field-value match-position) for the
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
                           (bibtex-completion-apa-get-value "=key=" entry))
                        ;; we're done so don't even compare file-name with
                        ;; file-keyword in the successive cycles
                        (setq file-keyword nil))
                    ;; do the usual processing otherwise
                    ;; condition-case to temporary workaround an upstream bug
                    (condition-case nil
                        (bibtex-completion-apa-get-value field-name entry)
                      (error "")))
                  ""))
             ;; org-capture prompt wildcard
             (rplc-s (concat "%^{" (or keyword "citekey") "}"))
             ;; org-roam-capture prompt wildcard
             (rplc-s2 (concat "${" (or keyword "citekey") "}"))
             ;; org-roam-capture :if-new property
             (if-new (plist-get plst :if-new))
             (i 1)                        ; match counter
             pos)
        ;; Search for rplc-s, set flag m if found
        (when tp
          (while (string-match rx tp pos)
            (if (string= (match-string 1 tp) rplc-s)
                (progn
                  (setq pos (length tp))
                  (cl-pushnew (list rplc-s field-value i) lst ))
              (setq pos (match-end 1)
                    i (1+ i)))))
        ;; Replace placeholders in org-roam-capture-templates :if-new property
        (when if-new
          (let (strings)
            (dolist (str (cdr if-new))
              (push (s-replace rplc-s2 field-value str) strings))
            (plist-put plst :if-new (cons (car if-new) (nreverse strings)))))))
    ;; Second run: replace prompts and prompt matches in org-capture
    ;; template string
    (dolist (l lst)
      (when (and tp (nth 1 l))
        (let ((pos (concat "%\\" (number-to-string (nth 2 l)))))
          ;; replace prompt match wildcards with prompt wildcards
          ;; replace prompt wildcards with BibTeX field value
          (setq tp (s-replace pos (car l) tp)
                tp (s-replace (car l) (nth 1 l) tp))))
      (setf (nth 4 template) tp))
    template))

(defun orb--new-note (citekey)
  "Process templates and run `org-roam-capture--capture'.
CITEKEY is a citation key.
Helper function for `orb-edit-note', which abstracts initiating
a capture session."
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
                        (copy-tree it)
                        ;; optionally pre-expand templates
                        (if orb-preformat-templates
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
            (org-capture-entry
             (org-roam-capture--convert-template template))
            (citekey-formatted (format (or orb-citekey-format "%s") citekey))
            (title
             (or (bibtex-completion-apa-get-value "title" entry)
                 (orb-warning "Title not found for this entry")
                 ;; this is not critical, the user may input their own
                 ;; title
                 "No title"))
            (node (org-roam-node-create :title title)))
      ;; data collection hooks functions: remove themselves once run
      ;; Depending on the templates used: run
      ;; `org-roam-capture--capture' with ORB-predefined
      ;; settings or call vanilla `org-roam-node-find'
      (org-roam-capture-
       :node node
       :info (list :ref citekey-formatted)
       :props '(:finalize find-file))
    (message "ORB: Something went wrong. Check the *Warnings* buffer")))

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

3. The template used to create the note is stored in
`orb-templates'.  If the variable is not defined, revert to using
`org-roam-capture-templates'.  In the former case, a new file
will be created and filled according to the template, possibly
preformatted (see below) without additional user interaction.  In
the latter case, an interactive `org-capture' process will be
run.

4. Optionally, when `orb-preformat-templates' is non-nil, any
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

5. Optionally, if you are using Projectile and Persp-mode and
have a dedicated workspace to work with your Org-roam collection,
you may want to set the perspective name and project path in
`orb-persp-project' and `orb-switch-persp' to t.  In this case,
the perspective will be switched to the Org-roam notes project
before calling any Org-roam functions."
  ;; Optionally switch to the notes perspective
  (when orb-switch-persp
    (orb--switch-perspective))
  (let ((node (orb-note-exists-p citekey)))
      ;; Find org-roam reference with the CITEKEY and collect data into
      ;; `orb-plist'
    (orb-plist-put :note-existed (and node t))
    (cond
     (node
      (orb-plist-put :title (org-roam-node-title node)
                     :file (org-roam-node-file node))
      (ignore-errors (org-roam-node-visit node)))
     ;; we need to clean up if the capture process was aborted signaling
     ;; user-error
    (t
      ;; fix some Org-ref related stuff
      (orb--store-link-functions-advice 'add)
      ;; install capture hook functions
      (orb-do-hook-functions 'add)
      (condition-case error-msg
          (orb--new-note citekey)
        ((debug error)
         (with-orb-cleanup
           (orb--store-link-functions-advice 'remove)
           (orb-do-hook-functions 'remove))
         (message "orb-edit-note caught an error during capture: %s"
                  (error-message-string error-msg))))))))

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

(defun orb-insert--link (file citekey &optional description lowercase)
  "Insert a link to FILE.
If a region is active, replace the region with the link and used
the selected text as the link's label.  If DESCRIPTION is
provided, use it as the link's label instead.  If none of the
above is true, insert the CITEKEY as a formatted Org-ref citation
using `org-ref-default-citation-link' or 'cite:' if this variable
is not bound.

If LOWERCASE is non-nil, downcase the link description.
Return the filename if it exists."
  ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text
                          (buffer-substring-no-properties beg end))))
               (description (or region-text description)))
          (when (and file (file-exists-p file))
            (when region-text
              (delete-region beg end)
              (set-marker beg nil)
              (set-marker end nil))
            (if description
                (let ((description (if lowercase
                                       (downcase description)
                                     description)))
                  ;; FIXME: this function does not exist anymore
                  ;; (insert (org-roam-format-link file description))
                  (ignore description))
              (let ((cite-link (if (boundp 'org-ref-default-citation-link)
                                   (concat org-ref-default-citation-link ":")
                                 "cite:")))
                (insert (concat cite-link citekey))))
            ;; return value
            file)))
    (deactivate-mark)))

(defvar orb-insert-lowercase nil)
(defun orb-insert--link-h ()
  "Prepare the environement and call `orb-insert--link'."
  ;; insert link only when file is non-nil
  (with-orb-cleanup
    (when-let ((file (orb-plist-get :file)))
      (let* ((citekey (orb-plist-get :citekey))
             (insert-description (or (orb-plist-get :link-description)
                                     orb-insert-link-description))
             (lowercase (or (orb-plist-get :link-lowercase)
                            orb-insert-lowercase))
             (description (cl-case insert-description
                            (title (orb-plist-get :title))
                            (citekey citekey)
                            (citation nil))))
        (with-current-buffer (orb-plist-get :buffer)
          (save-excursion
            (orb-insert--link file citekey description lowercase)))))
    (set-window-configuration (orb-plist-get :window-conf))
    (when (and orb-insert-follow-link
               (looking-at org-link-any-re))
      (org-open-at-point))
    ;; if any left
    (orb-do-hook-functions 'remove)))

(defun orb-insert-generic (&optional arg)
  "Present a list of BibTeX entries for completion.
This is a generic completion function for `orb-insert', which
runs `orb-insert-edit-notes' on the selected entry.  The list is
made by `bibtex-completion-candidates'.

The appearance of selection candidates is determined by
`orb-insert-generic-candidates-format'.

This function is not interactive, set `orb-insert-interface' to
`generic' and call `orb-insert' interactively instead.

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
    (orb-insert-edit-notes (list citekey))))

(defun orb-insert-edit-notes (citekey)
  "Call `orb-edit-note' and insert a link to a note.
CITEKEY is a citation key and #+ROAM_KEY of the retrieved or
newly created note."
  (orb-plist-put :buffer (current-buffer)
                 :window-conf (current-window-configuration)
                 :citekey (car citekey))
  ;; Here, we play on the specifics of a capture process.
  ;; `org-capture-finalize' runs `prepare-hook', `before-hook' and `after-hook'
  ;; in that order.  But, if `org-capture-finalize' was run via
  ;; `org-capture-kill', the `before-hook' is forced to nil via a let form.
  ;;
  ;; 1. In an interactive session, we get the title while the capture buffer
  ;; still exists.  But if the capture process was killed, our before hook
  ;; function did not run and therefore title is nil on `orb-plist'.
  (orb-register-hook-function get-title before nil
    (orb-plist-put :title (orb-get-buffer-keyword "title")
                   :immediate-finish
                   (plist-get org-capture-plist :immediate-finish)))

  (orb-register-hook-function get-file after -90
    (let ((file (buffer-file-name)))
      ;; 2. We check whether the title on `orb-plist' is nil.  When it is, we
      ;; set file to nil to signal `org-insert--link-h' not to insert a link.
      ;; We do this only in interactive process
      (unless (or (orb-plist-get :immediate-finish)
                  (orb-plist-get :title))
        (setq file nil)
        ;; before hook functions did not run, so they are still in
        ;; `org-capture-before-finalize-hook'; remove them.
        (orb-do-hook-functions 'remove 'before))
      (orb-plist-put :file file)))

  (orb-register-hook-function insert-link after 90
    (orb-insert--link-h))

  (save-excursion
    (orb-edit-note (car citekey)))
  ;; when note existed, a capture process did not run.  We have all the info on
  ;; `orb-plist', so just insert a link
  (when (orb-plist-get :note-existed)
    ;; we call the hook function so that the hook is removed
    (orb-call-hook-function 'insert-link)))

;;;###autoload
(defun orb-insert (&optional arg)
  "Insert a link to an Org-roam bibliography note.
If the note does not exist, create it.
Use candidate selection interface specified in
`orb-insert-interface'.  Available interfaces are `helm-bibtex',
`ivy-bibtex' and `orb-insert-generic'.

When using `helm-bibtex' or `ivy-bibtex', the action \"Edit note
& insert a link\" should be chosen to insert the desired link.
For convenience, this action is made default for the duration of
an `orb-insert' session.  It will not persist when `helm-bibtex'
or `ivy-bibtex' proper are run.  It is absolutely possible to run
other `helm-bibtex' or `ivy-bibtex' actions.  When action another
than \"Edit note & insert a link\" is run, no link will be
inserted, although the session can be resumed later with
`helm-resulme' or `ivy-resume', respectively, to select the
\"Edit note & insert a link\" action.

When using `orb-insert-generic' as interface, a simple list of
available citation keys is presented using `completion-read' and
after choosing a candidate the appropriate link will be inserted.

If the note does not exist yet, it will be created using
`orb-edit-note' function.

\\<universal-argument-map>\\<org-roam-bibtex-mode-map> The
customization option `orb-insert-link-description' determines
what will be used as the link's description.  It is possible to
override the default value with numerical prefix ARG:

`C-1' \\[orb-insert] will force `title'
`C-2' \\[orb-insert] will force `citekey'
`C-0' \\[orb-insert] will force `citation'

If a region of text is active (selected) when calling `orb-insert',
the text in the region will be replaced with the link and the
text string will be used as the link's description — similar to
`org-roam-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied."
  (interactive "P")
  ;; parse arg
  ;; C-u or C-u C-u C-u => force lowercase
  ;; C-u C-u or C-u C-u C-u => force `bibtex-completion-clear-cache'
  ;; C-1 force title in description
  ;; C-2 force citekey in description
  ;; C-3 force inserting the link as Org-ref citation
  (let* ((lowercase (or (equal arg '(4))
                        (equal arg '(64))))
         (description (cl-case arg
                        (1 'title)
                        (2 'citekey)
                        (0 'citation)))
         (clear-cache (or (equal arg '(16))
                          (equal arg '(64)))))
    (orb-plist-put :link-description
                   (or description orb-insert-link-description)
                   :link-lowercase
                   (or lowercase orb-insert-lowercase))
    ;; execution chain:
    ;; 1. interface function
    ;; 2. orb-insert-edit-notes
    ;; 3. orb-edit-note
    ;; 4. orb-insert--link-h
    ;; if the note exists or a new note was created and capture not cancelled
    ;; 5. orb-insert--link
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
(when (featurep 'hydra)
  (require 'hydra)
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
            (setq k (1+ k)))))    ; TODO: figure out a way to supply
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
    (orb-note-actions-hydra/body)))

(defun orb-note-actions--run (interface citekey )
  "Run note actions on CITEKEY with INTERFACE."
  (let ((fun (intern (concat "orb-note-actions-" (symbol-name interface)))))
    (if (fboundp fun)
        (funcall fun citekey)
      (orb-warning "Note actions interface %s not available" interface))))

;;;###autoload
(defun orb-note-actions ()
  "Run an interactive prompt to offer note-related actions.
The prompt interface can be set in `orb-note-actions-interface'.
In addition to default actions, which are not supposed to be
modified, there is a number of prefined extra actions
`orb-note-actions-extra' that can be customized.  Additionally,
user actions can be set in `orb-note-actions-user'."
  (interactive)
  (let ((non-default-interfaces (list 'hydra 'ido 'ivy 'helm))
        ;; FIXME: this does not work anymore
        ;; (citekey (cdr (org-roam--extract-ref)))
        (citekey (orb-get-node-citekey)))
    (if citekey
        (cond ((member orb-note-actions-interface non-default-interfaces)
               (orb-note-actions--run orb-note-actions-interface citekey))
              ((functionp orb-note-actions-interface)
               (funcall orb-note-actions-interface citekey))
              (t
               (orb-note-actions--run 'default citekey)))
      (user-error "Could not retrieve the citekey.  Check ROAM_REFS property \
of current node"))))

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

(defun orb-edit-notes (keys)
  "Open or create an Org-roam note associated with the first key from KEYS.
This function replaces `bibtex-completion-edit-notes'.  Only the
first key from KEYS will actually be used."
  (orb-edit-note (car keys)))

(defun orb-bibtex-completion-parse-bibliography-ad (&optional _ht-strings)
  "Update `orb-notes-cache' before `bibtex-completion-parse-bibliography'."
  (orb-make-notes-cache))

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
  (require 'bibtex-completion)
  (cond (org-roam-bibtex-mode
         (setq org-ref-notes-function 'orb-org-ref-edit-note)
         (add-to-list 'bibtex-completion-find-note-functions
                      #'orb-find-note-file)
         (setq bibtex-completion-edit-notes-function #'orb-edit-notes)
         (advice-add 'bibtex-completion-parse-bibliography
                     :before #'orb-bibtex-completion-parse-bibliography-ad))
        (t
         (setq org-ref-notes-function 'org-ref-notes-function-one-file)
         (setq bibtex-completion-find-note-functions
               (delq #'orb-find-note-file
                     bibtex-completion-find-note-functions))
         (setq bibtex-completion-edit-notes-function
               #'bibtex-completion-edit-notes-default)
         (advice-remove 'bibtex-completion-parse-bibliography
                        #'orb-bibtex-completion-parse-bibliography-ad))))

(provide 'org-roam-bibtex)

;;; org-roam-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
