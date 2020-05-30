;;; orb-pdf-scrapper.el --- Orb Roam BibTeX: PDF reference scrapper -*- coding: utf-8; lexical-binding: t -*-

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

;; N.B. This file contains code snippets adopted from other
;; open-source projects. These snippets are explicitly marked as such
;; in place. They are not subject to the above copyright and
;; authorship claims.

;;; Commentary:
;;

;;; Code:
;; * Library requires

(require 'orb-core)
(require 'orb-anystyle)

(require 'bibtex)
(require 'rx)
(require 'cl-extra)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; * Customize definitions

;; TODO: make these defcustom
(defvar orb-pdf-scrapper-keygen-function #'orb-pdf-scrapper--bibkey-default-fn
  "Function to generate citation keys in Orb PDF Scrapper.
It should take a bibtex entry as returned by `bibtex-completion-get-entry'\
and return a plist with the following keys set:

 :key             |string  | generated key
 :validp          |boolean | non-nil if the key is deemed to be valid
 :set-fields      |(cons)  | list of fields to set in the entry
 :export-fields   |(cons)  | list of fields to export to org

Each element of `:set-fields' and `:export-fields' lists must be
a cons cell (FIELD . VALUE).")

(defvar orb-pdf-scrapper-bibkey-set-fields
  '(("pages" . orb-pdf-scrapper--fix-or-invalidate-range)
    ("author" . orb-pdf-scrapper--invalidate-nil-value)
    ("title" . orb-pdf-scrapper--invalidate-nil-value)
    ("journal" . orb-pdf-scrapper--invalidate-nil-value)
    ("date" . orb-pdf-scrapper--invalidate-nil-value)
    ("volume" . orb-pdf-scrapper--invalidate-nil-value)))

(defvar orb-pdf-scrapper-bibkey-export-fields
  '("author" "journal" "date" "volume" "pages"))

(defvar orb-pdf-scrapper-bibkey-invalid-pattern "\\`.*N/A.*\\'")

;; * Helper functions: utilities

(defvar orb-pdf-scrapper--sorted-refs nil)

;; * Helper functions: citekey generation

(defun orb-pdf-scrapper--invalidate-nil-value (field entry)
  "Return value of FIELD or \"N/A\" if it is nil.
ENTRY is a BibTeX entry."
  (bibtex-completion-get-value field entry "N/A"))

(defun orb-pdf-scrapper--fix-or-invalidate-range (field entry)
  "Replace missing or non-standard delimiter between two strings with \"--\".
FIELD is the name of a BibTeX field from ENTRY.  Return N/A if
value of the field is nil.

This function is primarily intended for fixing anystyle parsing
artefacts such as those often encountered in \"pages\" field,
where two numbers have only spaces between them."
  (replace-regexp-in-string "\\`[[:alnum:]]\\([ –-]+\\)[[:alnum:]]\\'" "--"
                            (or (bibtex-completion-get-value field entry)
                                "N/A")
                            nil nil 1))

(defun orb-pdf-scrapper--bibkey-default-fn (entry)
  "Generate citation key from ENTRY according to `orb-autokey-format'.
Returns a plist as described in `orb-pdf-scrapper-bibkey-function'.
Keys are validated according to `orb-pdf-scrapper-bibkey-valid-pattern'.
Returns fields for setting as per `orb-pdf-scrapper-bibkey-set-fields'.
Returns fields for exporting as per `orb-pdf-scrapper-bibkey-export-fields'."
  (let (key validp set-fields export-fields ;; return values
            ;; internal variable
            fields)
    ;; prepare fields for setting
    (dolist (field orb-pdf-scrapper-bibkey-set-fields)
      (let ((name (car field))
            (fn (cdr field)))
        (cl-pushnew (cons name
                          (if fn
                              (funcall fn name entry)
                            (bibtex-completion-get-value name entry)))
                    set-fields)))
    ;; prioritize fields from set-fields over entry fields
    ;; for autokey generation
    (let ((-compare-fn (lambda (x y)
                         (string= (car x) (car y)))))
      (setq fields (-union set-fields entry)
            key (orb-autokey-generate-key fields)))
    ;; validate the new shiny key
    ;; not sure if save-match-data is needed here
    ;; but it seems to be always a good choice
    (save-match-data
      (setq validp (and (not (string-match-p
                              orb-pdf-scrapper-bibkey-invalid-pattern key))
                        'valid)))
    ;; list for org export
    (dolist (field orb-pdf-scrapper-bibkey-export-fields)
      ;; truncate author list to first three names, append et.al instead
      ;; of the remaining names
      (let ((value (cdr (assoc field fields))))
        (when (or (string= field "author")
                  (string= field "editor"))
          (setq value (split-string value "and" t "[ ,.;:-]+")
                value (if (> (length value) 3)
                          (append (-take 3 value) '("et.al."))
                        value)
                value (concat (mapconcat #'identity value "; "))))
        (cl-pushnew (cons field value) export-fields)))
    (list :key key
          :validp validp
          :set-fields set-fields
          :export-fields (nreverse export-fields))))

(defun orb-pdf-scrapper--sort-refs (refs)
  "Sort references REFS.
Auxiliary function for `orb-pdf-scrapper-generate-keys'.
REFS should be an alist of form ((CITEKEY . FORMATTED-ENTRY) . VALIDP).

References marked valid by `orb-pdf-scrapper-keygen-function' function
are further sorted into four groups:

'in-roam - available in the `org-roam' database;
'in-bib  - available in `bibtex-completion-bibliography' file(s);
'valid   - marked valid by the keygen function but are not
available in the user databases;
'invalid - marked invalid by the keygen function."
  (let* ((bibtex-completion-bibliography (orb-pdf-scrapper--get :global-bib))
         ;; When using a quoted list here, sorted-refs is not erased
         ;; upon consecutive runs
         (sorted-refs (list (list 'in-roam) (list 'in-bib)
                            (list 'valid) (list 'invalid)))
         (spaces (-repeat (length orb-pdf-scrapper-bibkey-export-fields) (cons nil " "))))
    (dolist (citation refs)
      (cond ((org-roam-db-query [:select [ref]
                                 :from refs
                                 :where (= ref $s1)]
                                (format "%s" (caar citation)))
             (cl-pushnew
              (cons (format "cite:%s" (caar citation)) spaces)
              (cdr (assoc 'in-roam sorted-refs))))
            ((bibtex-completion-get-entry (caar citation))
             (cl-pushnew
              (cons (format "cite:%s" (caar citation)) spaces)
              (cdr (assoc 'in-bib sorted-refs))))
            ((cdr citation)
             (cl-pushnew
              (cons (format "cite:%s" (caar citation)) (cdar citation))
              (cdr (assoc 'valid sorted-refs))))
            (t
             (cl-pushnew
              (cons " " (cdar citation))
              (cdr (assoc 'invalid sorted-refs))))))
    (setq orb-pdf-scrapper--sorted-refs sorted-refs)))

;; * Helper functions: dispatcher

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
  "Execute BODY if current context is CONTEXT.
Run `orb-pdf-scrapper-keygen-function' with `error' context otherwise."
  (declare (indent 1) (debug t))
  `(if (not (orb-pdf-scrapper--current-context-p ,context))
       (orb-pdf-scrapper-dispatcher 'error)
     ,@body))

(defun orb-pdf-scrapper--current-context-p (context)
  "Return t if CONTEXT is current context."
  (eq context (orb-pdf-scrapper--get :context)))

(defun orb-pdf-scrapper--refresh-mode (context)
  "Restart `orb-pdf-scrapper-mode' in new CONTEXT."
  (cl-case context
    ('txt
     (text-mode))
    ('bib
     (bibtex-mode)
     (bibtex-set-dialect 'BibTeX t))
    ('org
     (org-mode))
    (t
     (unwind-protect
         (error "Oops...something went wrong.  \
Pressing the RED button, just in case")
       (orb-pdf-scrapper-dispatcher 'error))))
  (orb-pdf-scrapper-mode -1)
  (orb-pdf-scrapper--put :context context)
  (orb-pdf-scrapper-mode +1)
  (goto-char (point-min)))

(defun orb-pdf-scrapper--edit-txt ()
  "Edit text references in `orb-pdf-scrapper--buffer'."
  (orb--when-current-context! 'start
    (unwind-protect
        (case (orb-pdf-scrapper--get :caller)
          ;; parse pdf file and switch to text editing mode
          ('run
           (let ((temp-txt (orb--temp-file "orb-pdf-scrapper-" ".txt"))
                 (pdf-file (orb-pdf-scrapper--get :pdf-file)))
             (orb-pdf-scrapper--put :temp-txt temp-txt)
             (let ((same-window-buffer-names (list orb-pdf-scrapper--buffer)))
               (pop-to-buffer orb-pdf-scrapper--buffer))
             (erase-buffer)
             (setq buffer-file-name nil)
             (setq mark-active nil)
             (orb--with-message! (format "Scrapping %s.pdf" (f-base pdf-file))
               (orb-anystyle 'find
                 :format 'ref
                 :layout nil
                 :finder-model orb-anystyle-finder-model
                 :input pdf-file
                 :stdout t
                 :buffer orb-pdf-scrapper--buffer))
             (setq buffer-undo-list nil)
             (set-buffer-modified-p nil)
             (orb-pdf-scrapper--refresh-mode 'txt)))
          ;; read the previously generated text file
          ('edit-bib
           (if-let ((temp-txt (orb-pdf-scrapper--get :temp-txt))
                    (f-exists? temp-txt))
               (progn
                 (pop-to-buffer orb-pdf-scrapper--buffer)
                 (erase-buffer)
                 (message "%s" (insert-file-contents temp-txt))
                 (insert-file-contents temp-txt)
                 (setq buffer-undo-list (orb-pdf-scrapper--get :txt-undo-list))
                 (set-buffer-modified-p nil)
                 (setq mark-active nil)
                 (orb-pdf-scrapper--refresh-mode 'txt))
             (orb-pdf-scrapper-dispatcher 'error)))
          (t
           (orb-pdf-scrapper-dispatcher 'error)))
      (orb-pdf-scrapper--put :caller 'edit-txt))))

(defun orb-pdf-scrapper--edit-bib ()
  "Generate and edit BibTeX data in `orb-pdf-scrapper--buffer'."
  (orb--when-current-context! 'txt
    (unwind-protect
        (cl-case (orb-pdf-scrapper--get :caller)
          ('edit-txt
           (let* ((temp-bib (or (orb-pdf-scrapper--get :temp-bib)
                                (orb--temp-file "orb-pdf-scrapper-" ".bib"))))
             (orb-pdf-scrapper--put :temp-bib temp-bib)
             (pop-to-buffer orb-pdf-scrapper--buffer)
             ;; save previous progress in txt buffer
             (write-region (orb--buffer-string)
                           nil (orb-pdf-scrapper--get :temp-txt) nil -1)
             (orb-pdf-scrapper--put :txt-undo-list (copy-tree buffer-undo-list))
             ;; no need to erase the buffer: shell-command invoked by
             ;; `orb-anystyle' will do that
             (orb--with-message! "Generating BibTeX data"
               (orb-anystyle 'parse
                 :format 'bib
                 :parser-model orb-anystyle-parser-model
                 :input (orb-pdf-scrapper--get :temp-txt)
                 :stdout t
                 :buffer orb-pdf-scrapper--buffer)
               (write-region (orb--buffer-string) nil temp-bib nil -1))
             (setq buffer-undo-list nil)
             (set-buffer-modified-p nil))
           (orb-pdf-scrapper--refresh-mode 'bib))
          ('edit-org
           (if-let ((temp-bib (orb-pdf-scrapper--get :temp-bib))
                    (f-exists? temp-bib))
               (progn
                 (pop-to-buffer orb-pdf-scrapper--buffer)
                 (erase-buffer)
                 (setq mark-active nil)
                 (insert-file-contents temp-bib)
                 (setq buffer-undo-list (orb-pdf-scrapper--get :bib-undo-list))
                 (set-buffer-modified-p nil)
                 (orb-pdf-scrapper--refresh-mode 'bib))
             (orb-pdf-scrapper-dispatcher 'error)))
          (t
           (orb-pdf-scrapper-dispatcher 'error)))
      (orb-pdf-scrapper--put :caller 'edit-bib))))

(defun orb-pdf-scrapper--edit-org ()
  "Insert sorted references as Org-mode tables sorted into subheadings."
  (orb--when-current-context! 'bib
    (let* ((temp-org (or (orb-pdf-scrapper--get :temp-org)
                         (orb--temp-file "orb-pdf-scrapper-" ".org"))))
      (orb-pdf-scrapper--put :temp-org temp-org
                             :caller 'edit-org)
      (pop-to-buffer orb-pdf-scrapper--buffer)
      (orb-pdf-scrapper--refresh-mode 'org)
      (orb--with-message! "Generating Org data"
        (erase-buffer)
        (org-insert-heading nil nil t)
        (insert "References (retrieved by Orb PDF Scrapper from "
                (f-filename (orb-pdf-scrapper--get :pdf-file)) ")")
        (dolist (ref-group orb-pdf-scrapper--sorted-refs)
          (org-insert-heading '(16) nil t)
          (org-demote)
          (insert (format "%s\n" (car ref-group)))
          (insert (concat "\n|citekey|"
                          (mapconcat #'identity orb-pdf-scrapper-bibkey-export-fields "|")
                          "|\n"))
          (forward-line -1)
          (org-table-insert-hline)
          (forward-line 2)
          (dolist (ref (cdr ref-group))
            (insert (concat "|" (car ref) "|"
                            (mapconcat (lambda (value)
                                         (format "%s" (cdr value)))
                                       (cdr ref) "|")
                            "|\n")))
          (forward-line -1)
          (org-table-align)
          (org-back-to-heading nil))
        (write-region (orb--buffer-string) nil temp-org nil -1)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)
        (goto-char (point-min))))))

(defun orb-pdf-scrapper--checkout ()
  "Finalize Orb PDF Scrapper process.
Insert generated Org data into the note buffer that started the
process."
  (orb--when-current-context! 'org
    (pop-to-buffer (orb-pdf-scrapper--get :original-buffer))
    (save-restriction
      (save-excursion
        (widen)
        (goto-char (point-max))
        (insert-file-contents (orb-pdf-scrapper--get :temp-org))))
    (orb-pdf-scrapper--cleanup)))

;; (defun orb-pdf-scrapper--checkout2 ()
;;   "Parse bib buffer and populate `orb-pdf-scrapper--sorted-refs'."
;;   (let ((context (orb-pdf-scrapper--get :context)))
;;     (cond ((equal context 'checkout-non-interactive)
;;            (orb--with-message! "Generating citation keys"
;;              (orb--with-scrapper-buffer!
;;                (orb-anystyle 'parse
;;                  :format 'bib
;;                  :parser-model orb-anystyle-parser-model
;;                  :input (orb-pdf-scrapper--get :temp-txt)
;;                  :stdout t
;;                  :buffer orb-pdf-scrapper--buffer)
;;                (bibtex-mode)
;;                (bibtex-set-dialect 'BibTeX t)
;;                (condition-case nil
;;                    (orb-pdf-scrapper-generate-keys)
;;                  (t
;;                   (orb-pdf-scrapper--put :context 'checkout-interactive)
;;                   (orb-pdf-scrapper--checkout)))))
;;            (orb-pdf-scrapper--insert-org))
;;           ((equal context 'checkout-interactive)
;;            (when (> (cl-random 100) 98)
;;              (orb--with-message! "Pressing the RED button"))
;;            (when (buffer-modified-p (get-buffer orb-pdf-scrapper--buffer))
;;              (orb-pdf-scrapper-generate-keys))
;;            (orb-pdf-scrapper--insert-org))
;;           (t
;;            (orb-pdf-scrapper-dispatcher 'error)))))

(defun orb-pdf-scrapper--cleanup ()
  "Clean up after Orb Pdf Scrapper process."
  (and (get-buffer orb-pdf-scrapper--buffer)
       (kill-buffer orb-pdf-scrapper--buffer))
  (set-window-configuration (orb-pdf-scrapper--get :window-conf))
  (dolist (prop (list :running :context :current-key :prevent-concurring
                      :temp-txt :temp-bib :temp-org :pdf-file :global-bib
                      :window-conf :original-buffer :caller))
    (orb-pdf-scrapper--put prop nil)))


;; * Minor mode

;;; Code in this section was adopted from org-capture.el
;;
;; Copyright (C) 2010-2020 Free Software Foundation, Inc.
;; Author: Carsten Dominik <carsten at orgmode dot org>
(defvar orb-pdf-scrapper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'orb-pdf-scrapper-dispatcher)
    (define-key map "\C-c\C-k" #'orb-pdf-scrapper-kill)
    map)
  "Keymap for `orb-pdf-scrapper-mode' minor mode.")

(defvar orb-pdf-scrapper-mode-hook nil
  "Hook for the `orb-pdf-scrapper-mode' minor mode.")

(define-minor-mode orb-pdf-scrapper-mode
  "Minor mode for special key bindings in a orb-pdf-scrapper buffer.
Turning on this mode runs the normal hook `orb-pdf-scrapper-mode-hook'."
  nil " OPS" orb-pdf-scrapper-mode-map
  (when orb-pdf-scrapper-mode
    (orb-pdf-reference-scrapper--update-keymap)
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

(defun orb-pdf-scrapper--format-header-line ()
  "Return formatted buffer header line depending on context."
  (substitute-command-keys
   (format "\\<orb-pdf-scrapper-mode-map>Orb PDF Scrapper: %s.  %s"
           (orb-pdf-scrapper--get :current-key)
           (cl-case (orb-pdf-scrapper--get :context)
             ('txt
              "\
Generate BibTeX `\\[orb-pdf-scrapper-dispatcher]', \
sanitize text `\\[orb-pdf-scrapper-sanitize-text]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             ('bib
              "\
Generate Org `\\[orb-pdf-scrapper-dispatcher]', \
generate keys `\\[orb-pdf-scrapper-generate-keys]', \
return to text `\\[orb-pdf-scrapper-return-to-txt]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             ('org
              "\
Finish `\\[orb-pdf-scrapper-dispatcher]', \
return to BibTeX `\\[orb-pdf-scrapper-return-to-bib]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             (t
              "\
Press the RED button `\\[orb-pdf-scrapper-kill]'.")))))

(defun orb-pdf-reference-scrapper--update-keymap ()
  "Update `orb-pdf-scrapper-mode-map' according to context.
Context is read from `orb-pdf-scrapper--plist' property `:context'."
  ;; context is a "future action"
  (cl-case (orb-pdf-scrapper--get :context)
    ;; we are editing txt
    ('txt
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-u" #'orb-pdf-scrapper-sanitize-text))
    ;; we are editing bib
    ('bib
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-u" #'orb-pdf-scrapper-generate-keys)
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-r" #'orb-pdf-scrapper-return-to-txt))
    ;; we are editing org
    ('org
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-r" #'orb-pdf-scrapper-return-to-bib))
    (t
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-c" nil)
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-u" nil)
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-r" nil))))

;; * Interactive functions

(defun orb-pdf-scrapper-generate-keys ()
  "Generate citation keys in the current buffer.
Use the function specified in `orb-pdf-scrapper-keygen-function'.
Sort and push the retreived references to
`orb-pdf-scrapper--sorted-refs'."
  (interactive)
  ;; if the buffer contents has changed,
  ;; we need to write the changes to the temp file
  ;; because bibtex-completion-bibliography
  ;; will be bound to this file during key generation
  (write-region (orb--buffer-string) nil
                (orb-pdf-scrapper--get :temp-bib) nil -1)
  (orb--with-message! "Generating citation keys"
    (let ((bibtex-completion-bibliography
           (or (orb-pdf-scrapper--get :temp-bib)
               (buffer-file-name)))
          (bibtex-help-message nil)
          (bibtex-contline-indentation 2)
          (bibtex-text-indentation 2)
          (refs nil))
      (save-excursion
        (goto-char (point-min))
        (bibtex-skip-to-valid-entry)
        (while (not (eobp))
          (let* ((key (bibtex-key-in-head))
                 ;; TODO: consider using more more low-level parsebib
                 (entry (bibtex-completion-get-entry key))
                 (key-plist (funcall orb-pdf-scrapper-keygen-function entry))
                 (new-key (plist-get key-plist :key))
                 (validp (plist-get key-plist :validp))
                 (fields-to-set (plist-get key-plist :set-fields))
                 (formatted-entry (plist-get key-plist :export-fields)))
            ;; update citekey
            ;; adjusted from bibtex-clean-entry
            (save-excursion
              (bibtex-beginning-of-entry)
              (re-search-forward bibtex-entry-maybe-empty-head)
              (if (match-beginning bibtex-key-in-head)
                  (delete-region (match-beginning bibtex-key-in-head)
                                 (match-end bibtex-key-in-head)))
              (insert new-key))
            ;; set the bibtex fields
            (when fields-to-set
              (dolist (field fields-to-set)
                (bibtex-set-field (car field) (cdr field))))
            ;; push the result ((NEW-KEY . ENTRY) . VALIDP)
            (cl-pushnew
             ;; TODO: for testing until implemented
             (cons (cons new-key formatted-entry) validp) refs))
          (bibtex-end-of-entry)
          ;; go to next entry
          (bibtex-skip-to-valid-entry)))
      (orb-pdf-scrapper--sort-refs refs))
    (write-region (orb--buffer-string) nil
                  (orb-pdf-scrapper--get :temp-bib) nil -1)
    (set-buffer-modified-p nil)))

(defun orb-pdf-scrapper-sanitize-text (&optional contents)
  "Run string processing in current buffer.
Try to get every reference onto newline.  Return this buffer's
contents (`orb--buffer-string').

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
      (orb--buffer-string))))
;;
;; TODO: Should be a general function

(defun orb-pdf-scrapper-return-to-txt ()
  "Return to editing text references in Orb PDF Scrpapper."
  (interactive)
  (orb--when-current-context! 'bib
    ;; all progress in bib is lost because we re-generate the data
    (orb--with-scrapper-buffer!
      (orb-pdf-scrapper--put :bib-undo-list nil))
    (orb-pdf-scrapper-dispatcher 'start)))

(defun orb-pdf-scrapper-return-to-bib ()
  "Return to editing text references in Orb PDF Scrpapper."
  (interactive)
  (orb--when-current-context! 'org
    (orb-pdf-scrapper-dispatcher 'txt)))

(defun orb-pdf-scrapper-dispatcher (&optional context)
  "Run different Orb PDF Scrapper actions depending on CONTEXT.
CONTEXT can be passed directly or with `orb-pdf-scrapper--plist'
by setting `:context' property.  Passing CONTEXT to the dispatcher
means finish the current editing mode and proceed further.

Recognized contexts are:
==========
'start - start txt editing mode
'txt   - switch from txt to bib editing mode
'bib   - switch from bib to org editing mode
'org   - insert Org data into the note buffer and end the process

Passing or setting any other context will kill the process.

This function also checks `:prevent-concurring' property in
`orb-pdf-scrapper--plist' and will suggest to restart the process
if its value is non-nil."
  ;; TODO: check for whether the user killed any of the buffers
  (interactive)
  (let ((context (or context (orb-pdf-scrapper--get :context))))
    ;; in case context was passed as an argument
    (orb-pdf-scrapper--put :context context)
    (if
     ;; Prevent another Orb PDF Scrapper process
     ;; Ask user whether to kill currently running process
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
        ;; Do nothing
        (orb-pdf-scrapper--put :prevent-concurring nil))
      ;; Finilize the requested context otherwise
      (cl-case context
        ('start
         (orb-pdf-scrapper--edit-txt))
        ('txt
         ;; ;; TODO: Reimplement non-interactive process
         ;; (if (y-or-n-p "Review the bib file? ")

         ;;   ;; proceed to checkout; bib file was not edited interactively
         ;;   (orb-pdf-scrapper--put :context 'checkout-non-interactive)
         ;;   (orb-pdf-scrapper--checkout))
         (orb-pdf-scrapper--edit-bib))
        ;; edit org
        ('bib
         ;; if the buffer was modified, save the buffer and generate keys
         (orb--with-scrapper-buffer!
           (when (> (cl-random 100) 98)
             (orb--with-message! "Pressing the RED button"))
           (when (buffer-modified-p)
             (orb-pdf-scrapper-generate-keys))
           (write-region (orb--buffer-string)
                         nil (orb-pdf-scrapper--get :temp-bib) nil 1)
           (orb-pdf-scrapper--put :bib-undo-list (copy-tree buffer-undo-list)))
         (orb-pdf-scrapper--edit-org))
        ('org
         ;; currently, this is unnecessary but may be useful
         ;; if some recovery options are implemented
         (orb--with-scrapper-buffer!
           (write-region (orb--buffer-string)
                         nil (orb-pdf-scrapper--get :temp-org) nil 1))
         (orb-pdf-scrapper--checkout))
        (t
         ;; 1 in 100 should not be too annoying
         (when (> (cl-random 100) 98)
           (message "Oops...")
           (sleep-for 1)
           (message "Oops...Did you just ACCIDENTALLY press the RED button?")
           (sleep-for 2)
           (message "Activating self-destruction subroutine...")
           (sleep-for 2)
           (message "Activating self-destruction subroutine...Bye-bye")
           (sleep-for 2))
         (orb-pdf-scrapper--cleanup))))))

(defun orb-pdf-scrapper-kill ()
  "Kill the interactive orb-pdf-scrapper process."
  (interactive)
  (orb-pdf-scrapper--put :context 'kill)
  (orb-pdf-scrapper-dispatcher))


;; * Main functions

;; entry point

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
    (setq orb-pdf-scrapper--sorted-refs nil)
    (orb-pdf-scrapper--put :context 'start
                           :current-key key
                           :new-key nil
                           :pdf-file (file-truename
                                      (orb-process-file-field key))
                           :running t
                           :prevent-concurring nil
                           :caller 'run
                           :global-bib bibtex-completion-bibliography
                           :original-buffer (current-buffer)
                           :window-conf (current-window-configuration))
    (orb-pdf-scrapper-dispatcher)))

(provide 'orb-pdf-scrapper)
;;; orb-pdf-scrapper.el ends here
;; Local Variables:
;; fill-column: 79
;; End:
