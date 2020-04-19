;;; org-roam-bibtex.el --- Connector between org-roam, bibtex-completion, and org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+org@gmail.com>

;; Author: Leo Vivier <leo.vivier+org@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; 	Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/zaeph/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (s "1.12.0") (org "9.3") (org-roam "1.0.0") (bibtex-completion "2.0.0"))

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
;; This library offers an integration between bibtex-completion and
;; org-roam by delegating the tasks of note's creation, editing and
;; retrieval to org-roam.  From the org-roam's perspective, the library
;; provides a means to populate org-roam templates with bibliographic
;; information secured through bibtex-completion,.
;;
;; To use it:
;;
;; call interactively `org-roam-bibtex-mode' or
;; call (org-roam-bibtex-mode +1) from Lisp.
;;
;; After enabling `org-roam-bibtex-mode', the function
;; `org-roam-bibtex-edit-notes' will shadow
;; `bibtex-completion-edit-notes' in `helm-bibtex', `ivy-bibtex' and
;; its surrogate will be used as a `org-ref-notes-function' in
;; `org-ref' (see `org-ref' documentation for how to setup many-files
;; notes).
;;
;; As a user option, `org-roam-capture-templates' can be dynamically
;; preformatted with bibtex field values.  See
;; `org-roam-bibtex-preformat-keywords' for more details.
;;
;; Optionally, automatic switching to the perspective (persp-mode) with
;; the notes project (projectile) is possible.  See
;; `org-roam-bibtex-edit-notes' for more details.
;;

;;; Code:
;; * Library Requires

(require 'org-roam)
(require 'bibtex-completion)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function projectile-relevant-open-projects "projectile")
(declare-function persp-switch "persp-mode" (name &optional frame (window (selected-window)) (called-interactively-p (called-interactively-p 'any))))
(declare-function persp-names "persp-mode" (&optional (phash *persp-hash*) (reverse t)))


;; * Customize definitions

(defgroup org-roam-bibtex nil
  "Org-ref and bibtex-completion integration for org-roam."
  :group 'org-roam
  :group 'org-ref
  :prefix "org-roam-bibtex-")

(defcustom org-roam-bibtex-preformat-templates t
  "Non-nil to enable template preformatting.
See `org-roam-bibtex-edit-notes' for details."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-templates
  '(("r" "ref" plain
     (function org-roam-capture--get-point)
     ""
     :file-name "${slug}"
     :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
     :unnarrowed t))
  "Template to use when creating a new note.
See `org-roam-bibtex-edit-notes' for details."
  :type '(list)
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-include-citekey-in-titles nil
  "Non-nil to include the citekey in titles.
See `org-roam-bibtex-edit-notes' for details."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-preformat-keywords "=key="
  "The template prompt wildcards for preformatting.
Only relevant when `org-roam-bibtex-preformat-templates' is set
to t (default). This can be a string, a list of strings or a cons-cell
alist, where each element is (STRING . STRING).

Use only alphanumerical characters, dash and underscore. See
`org-roam-bibtex-edit-notes' for implementation details.

1. If the value is a string, a single keyword, it is treated as
a BibTeX field-name, such as =key=. In the following example all
the prompts with the '=key=' keyword will be preformatted, as well as
the corresponding match group %\\1.

\(setq org-roam-bibtex-preformat-keywords \"=key=\")
\(setq org-roam-capture-templates
      '((\"r\" \"reference\" plain (function org-roam-capture--get-point)
         \"#+ROAM_KEY: %^{=key=}%? fullcite: %\\1\"
         :file-name \"references/${=key=}\"
         :head \"#+TITLE: ${title}\"
         :unnarrowed t)))

2. If the value is a list of strings they are also treated as
BibTeX field-names. The respective prompts will be preformatted.

\(setq org-roam-bibtex-preformat-keywords '(\"=key=\" \"title\"))
\(setq org-roam-capture-templates
      '((\"r\" \"reference\" plain (function org-roam-capture--get-point)
         \"#+ROAM_KEY: %^{=key=}%? fullcite: %\\1\"
         :file-name \"references/${=key=}\"
         :head \"#+TITLE: ${title}\"
         :unnarrowed t)))

3. If the value is a list of cons cells, then the car of the cons
cell is treated as a prompt keyword and the cdr as a BibTeX field
name, and the latter will be used to retrieve the relevant value
from the BibTeX entry. If cdr is omitted, then the car is treated
as the field name.

\(setq org-roam-bibtex-preformat-keywords
      '((\"citekey\" . \"=key=\")
       (\"type\" . \"=type=\")
       \"title\"))
\(setq org-roam-capture-templates
      '((\"r\" \"reference\" plain (function org-roam-capture--get-point)
         \"#+ROAM_KEY: %^{citekey}%? fullcite: %\\1
          #+TAGS: %^{type}
          This %\\2 deals with ...\"
         :file-name \"references/%<%Y-%m-%d-%H%M%S>_${title}\"
         :head \"#+TITLE: ${title}\"
         :unnarrowed t)))

Consult bibtex-completion package for additional information
about BibTeX field names."
  :type '(choice
          (string :tag "BibTeX field name")
          (group :tag "BibTeX field names" (repeat :tag "BibTeX field names" string))
          (alist
           :tag "Template wildcard keyword/BibTeX field name pairs"
           :key-type (string :tag "Wildcard")
           :value-type (string :tag "Field")))
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-citekey-format "cite:%s"
  "Format string for the citekey.
The citekey obtained from `helm-bibtex'/`ivy-bibtex'/`org-ref'
will be formatted as specified here."
  :type 'string
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-persp-project `("notes" . ,org-roam-directory)
  "Perspective name and path to the project with bibliography notes.
A cons cell (PERSP-NAME . PROJECT-PATH).  Only relevant when
`org-roam-bibtex-switch-persp' is set to t.

Requires 'persp-mode' and 'projectile'.

PERSP-NAME should be a valid perspective name, PROJECT-PATH should be
an open projectile project.

See `org-roam-bibtex-edit-notes' for details"
  :type '(cons (string :tag "Perspective name") (directory :tag "Projectile directory"))
  :group 'org-roam-bibtex)

(defcustom org-roam-bibtex-switch-persp nil
  "Non-nil to enable switching to the notes perspective.
Set the name of the perspective and the path to the notes project
in `org-roam-bibtex-persp-project' for this to take effect.

Requires 'persp-mode' and 'projectile'.

See `org-roam-bibtex-edit-notes' for details."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'org-roam-bibtex)


;; * Interface functions

;;;###autoload
(defun org-roam-bibtex-notes-fn (citekey)
  "Open an org-roam note associated with the CITEKEY or create a new one.
Set `org-ref-notes-function' to this function if your
bibliorgaphy notes are managed by org-roam and you want some extra
integration between the two packages.

This is a wrapper function around `org-roam-bibtex-edit-notes'
intended for use with org-ref."
  (when (require 'org-ref nil t)
    (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
      (org-roam-bibtex-edit-notes citekey))))

;;;###autoload
(defun org-roam-bibtex-edit-notes-ad (keys)
  "Open an org-roam note associated with the first key from KEYS.
This function replaces `bibtex-completion-edit-notes'. Only the first key
from KEYS will actually be used."
  (org-roam-bibtex-edit-notes (car keys)))

;;;###autoload
(defun org-roam-bibtex-process-file-field (citekey)
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


;; * Helper functions

(defun org-roam-bibtex--switch-perspective ()
  "Helper function for `org-roam-bibtex-edit-notes'."
  (when (and (require 'projectile nil t)
             (require 'persp-mode nil t))
    (let ((notes-project (cdr org-roam-bibtex-persp-project))
          (projects (projectile-relevant-open-projects))
          openp)
      (dolist (project projects openp)
        (setq openp (or (f-equal? project notes-project) openp)))
      (when openp
        (let ((p-names (cdr (persp-names))))
          (dolist (p-name p-names)
            (when (s-equals? p-name (car org-roam-bibtex-persp-project))
              (persp-switch p-name))))))))

(defun org-roam-bibtex--preformat-template (template entry)
  "Helper function for `org-roam-bibtex--preformat-templates'.
TEMPLATE is an element of `org-roam-capture-templates' and ENTRY
is a BibTeX entry as returned by `bibtex-completion-get-entry'."
  ;; Handle org-roam-capture part
  (let* ((kwds (if (listp org-roam-bibtex-preformat-keywords) ; normalize org-roam-bibtex-preformat-keywords
                   org-roam-bibtex-preformat-keywords
                 (list org-roam-bibtex-preformat-keywords)))
         ;; Org-capture templates:
         ;; handle different types of org-capture-templates: string, file and function;
         ;; this is a stripped down version of `org-capture-get-template'
         f
         (tp
          (pcase (nth 4 template)       ; org-capture template is here
            (`nil 'nil)
            ((and (pred stringp) tmpl) tmpl)
            (`(file ,file)
             (let ((flnm (expand-file-name file org-directory)))
               (if (file-exists-p flnm) (f-read-text flnm)
                 (format "Template file %S not found" file))))
            (`(function ,fun)
             (if (functionp fun) (funcall fun)
               (format "Template function %S not found" f)))
            (_ "Invalid capture template")))
         (plst (cdr template))          ; org-roam capture properties are here
         (rx "\\(%\\^{[[:alnum:]-_]*}\\)") ; regexp for org-capture prompt wildcard
         lst)
    ;; First run:
    ;; 1) Make a list of (rplc-s field-value match-position) for the second run
    ;; 2) replace org-roam-capture wildcards
    (dolist (kwd kwds)
      (let* ((keyword (or (car-safe kwd) kwd))        ; prompt wildcard keyword
             (field-name (or (cdr-safe kwd) kwd)) ; bibtex field name
             (field-value                ; get the bibtex field value
              (or (s-format (concat "${" field-name "}") 'bibtex-completion-apa-get-value entry)
                  nil))                                         ; nil will be used to set back the proper wildcard
             (rplc-s (concat "%^{" (or keyword "citekey") "}")) ; org-capture prompt wildcard
             (rplc-s2 (concat "${" (or keyword "citekey") "}")) ; org-roam-capture prompt wildcard
             (head (plist-get plst :head))  ; org-roam-capture :head template
             (fl-nm (plist-get plst :file-name)) ; org-roam-capture :file-name template
             (i 1)                               ; match counter
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
        ;; Replace org-roam-capture prompt wildcards
        (when (and field-value head)
          (plist-put plst :head (s-replace rplc-s2 field-value head)))
        (when (and field-value fl-nm)
          (plist-put plst :file-name (s-replace rplc-s2 field-value fl-nm)))))
    ;; Second run: replace prompts and propmt matches in org-capture template string
    (dolist (l lst)
      (when (and tp (nth 1 l))
        (let ((pos (concat "%\\" (number-to-string (nth 2 l)))))
          ;; replace prompt match wildcards with prompt wildcards
          ;; replace prompt wildcards with bitex field value
          (setq tp (s-replace pos (car l) tp)
                tp (s-replace (car l) (nth 1 l) tp))))
      (setf (nth 4 template) tp))
    template))

(defun org-roam-bibtex--get-non-ref-path-completions (&optional candidates)
  "Return a list of cons for titles of non-ref notes to absolute path.
CANDIDATES is a an alist of candidates to consider.  Defaults to
`org-roam--get-title-path-completions' otherwise."
  (let* ((candidates (or candidates
                         (org-roam--get-title-path-completions)))
         (refs-path (->> (org-roam--get-ref-path-completions)
                         (mapcar #'cdr)))
         completions)
    (dolist (candidate candidates completions)
      (let ((path (cdr candidate)))
        (unless (member path refs-path)
          (setq completions (nconc completions
                                   (list candidate))))))))


;; * Main functions

;;;###autoload
(define-minor-mode org-roam-bibtex-mode
  "Sets `org-roam-bibtex-edit-notes' as a function for editing bibliography notes.
Affects `org-ref' and `helm-bibtex'/`ivy-bibtex'.

When called interactively, toggle `org-roam-bibtex-mode'. with prefix
ARG, enable `org-roam-bibtex-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively."
  :lighter " orb"
  :group 'org-roam-bibtex
  :require 'org-roam-bibtex
  :global t
  (cond (org-roam-bibtex-mode
         (setq org-ref-notes-function 'org-roam-bibtex-notes-fn)
         (advice-add 'bibtex-completion-edit-notes
                     :override #'org-roam-bibtex-edit-notes-ad))
        (t
         (setq org-ref-notes-function 'org-ref-notes-function-one-file)
         (advice-remove 'bibtex-completion-edit-notes
                        #'org-roam-bibtex-edit-notes-ad))))

;;;###autoload
(defun org-roam-bibtex-edit-notes (citekey)
  "Open an org-roam note associated with the CITEKEY or create a new one.

This function allows to use org-roam as a backend for managing
bibliography notes. It relies on `bibtex-completion' to get
retrieve bibliograpic information from a BibTeX file.

Implementation details and features:

1. This function first calls `org-roam-find-ref' trying to find the
note file associated with the CITEKEY. The org-roam key can be
set with '#+ROAM_KEY:' in-buffer keyword.

2. If the org-roam reference has not been found, the function
calls `org-roam-find-file' passing to it the title associated
with the CITEKEY as retrieved by `bibtex-completion-get-entry'.
The prompt presented by `org-roam-find-file' will thus be
pre-populated with the record title.

3. The template used to create the note is stored in
`org-roam-bibtex-templates'. If the variable is not defined,
revert to using `org-roam-capture-templates'. In the former case,
a new file will be created and filled according to the template,
possibly preformatted (see below) without additional user
interaction. In the latter case, an interactive `org-capture'
process will be run.

4. Optionally, when `org-roam-bibtex-preformat-templates' is
non-nil, any prompt wildcards in `org-roam-bibtex-templates' or
`org-roam-capture-templates' associated with the bibtex record
fields as specified in `org-roam-bibtex-preformat-templates'
will be preformatted. Both `org-capture-templates' (%^{}) and
`org-roam-capture-templates' (`s-format', ${}) prompt syntaxes
are supported.

See `org-roam-bibtex-preformat-keywords' for more details on how
to properly specify prompts for replacement.

Please pay attention when using this feature that by setting
title for preformatting it will be impossible to change it in the
`org-roam-find-file' interactive prompt since all the template
expansions will have taken place by then. All the title wildcards
will be replace with the BibTeX field value.

5. Optionally, if you are using projectile and persp-mode and
have a dedicated workspace to work with your org-roam collection,
you may want to set the perspecive name and project path in
`org-roam-bibtex-persp-project' and `org-roam-bibtex-switch-persp' to
t. In this case, the perspective will be switched to the org-roam
notes project before calling any org-roam functions."
  (unless org-roam-mode
    (org-roam-mode +1))
  (let* ((citekey-formatted (format (or org-roam-bibtex-citekey-format "%s") citekey))
         (note-info (list (cons 'ref citekey-formatted))))
    ;; Optionally switch to the notes perspective
    (when org-roam-bibtex-switch-persp
      (org-roam-bibtex--switch-perspective))
    ;; Find org-roam reference with the CITEKEY
    (unless (ignore-errors (org-roam-find-ref note-info))
      ;; Check if the requested entry actually exists and fail gracefully
      (if-let* ((entry (bibtex-completion-get-entry citekey))
                ;; Depending on the templates used, run org-roam--capture or call org-roam-find-file
                (templates (or org-roam-bibtex-templates
                               org-roam-capture-templates
                               (and (display-warning :warning "Could not find the requested templates.")
                                    nil)))
                (org-roam-capture-templates
                 ;; Optionally preformat keywords
                 (or
                  (when org-roam-bibtex-preformat-templates
                    (let* ((tmpls (copy-tree templates))
                           result)
                      ;; HACK: Currently, there is no easy way to inject ourselves into
                      ;; the org-capture process once it's started. We traverse and preformat
                      ;; all the templates beforehand, although only one will be used eventually.
                      ;; This is a waste of resources and may be slow with many templates.
                      (dolist (tmpl tmpls result)
                        (cl-pushnew (org-roam-bibtex--preformat-template tmpl entry) result))))
                  templates))
                (title
                 (or (s-format "${title}" 'bibtex-completion-apa-get-value entry)
                     "Title not found for this entry (Check your BibTeX file)")))
          ;; Check if a custom template has been set
          (if org-roam-bibtex-templates
              (let ((org-roam-capture--context 'ref)
                    (org-roam-capture--info (list (cons 'title title)
                                                  (cons 'ref citekey-formatted)
                                                  (cons 'slug (org-roam--title-to-slug citekey)))))
                (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h)
                (org-roam--capture))
            (org-roam-find-file title))
        (message "Something went wrong. Check the *Warnings* buffer.")))))

;;;###autoload
(defun org-roam-bibtex-find-non-ref-file (&optional initial-prompt)
  "Find and open an Org-roam, non-ref file.
INITIAL-PROMPT is the initial title prompt.
See `org-roam-find-files' and
`org-roam-bibtex--get-non-ref-path-completions' for details."
  (interactive)
  (org-roam-find-file initial-prompt
                      #'org-roam-bibtex--get-non-ref-path-completions))

;;;###autoload
(defun org-roam-bibtex-insert-non-ref (prefix)
  "Find a non-ref Org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion.
See `org-roam-insert' and
`org-roam-bibtex--get-non-ref-path-completions' for details."
  (interactive "P")
  (org-roam-insert prefix #'org-roam-bibtex--get-non-ref-path-completions))

(provide 'org-roam-bibtex)
;;; org-roam-bibtex.el ends here
