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

;; TODO: make it defcustom
(defvar orb-pdf-scrapper-keygen-function #'orb-pdf-scrapper--YAP-keygen-fn
  "Function to generate citation key.
It should take a bibtex entry as returned by
`bibtex-completion-get-entry' and return a plist of form
\(:key generated_citekey
 :validp non-nil_if_key_is_valid_according_to_some_internal_logic
 :fields cons_cell_(field_name_._field_value)_to_write_into_entry
 :entry formatted_entry)
Technically, only :key and :entry are strictly required.")

;; * Helper functions

(defun orb-pdf-scrapper--tsv-to-list (file)
  "Convert tab-separated values in FILE into a list."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((orig-list (split-string (orb--buffer-string) "\n" t)))
      (--map (--> it
                  (split-string it "\t" t)
                  (cons (nth 2 it) (car it)))
             orig-list))))

(defun orb-pdf-scrapper--str-to-regexp (str)
  "Convert STR with punctuation into its regexp exression.
Usefull for matching short journal names.
Example:
========
\"abc. def-ghi, jkl;\" =>
\"\\(abc[ ,.;–-]def[ ,.;–-]ghi[ ,.;–-]jkl[ ,.;–-]\\)\"."
  (format "\\(%s[ ,.;–-]*\\)"
          (--reduce (format "%s[ ,.;–-]+%s" acc it)
                    (split-string str "[ ,.;–-]+" t "[ ]+"))))

(defvar orb-pdf-scrapper--journal-titles)
;; read journal title abbreviations
(setq orb-pdf-scrapper--journal-titles
      (orb-pdf-scrapper--tsv-to-list
       (f-join orb-data-dir "journal_titles.tsv")))

(defvar orb-pdf-scrapper--sorted-refs nil)

(defvar orb-pdf-scrapper--buffer "*Orb PDF Scrapper*"
  "Orb PDF Scrapper special buffer.")

(defvar orb-pdf-scrapper--plist nil
  "Communication channel for Orb PDF Scrapper.")

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
                            (list 'valid) (list 'invalid))))
    (dolist (citation refs)
      (cond ((org-roam-db-query [:select [ref]
                                 :from refs
                                 :where (= ref $s1)]
                                (format "%s" (caar citation)))
             (cl-pushnew
              (format "cite:%s" (caar citation))
              (cdr (assoc 'in-roam sorted-refs))))
            ((bibtex-completion-get-entry (caar citation))
             (cl-pushnew
              (format "cite:%s" (caar citation))
              (cdr (assoc 'in-bib sorted-refs))))
            ((cdr citation)
             (cl-pushnew
              (format "cite:%s %s" (caar citation) (cdar citation))
              (cdr (assoc 'valid sorted-refs))))
            (t
             (cl-pushnew
              (format "%s" (cdar citation))
              (cdr (assoc 'invalid sorted-refs))))))
    (setq orb-pdf-scrapper--sorted-refs sorted-refs)))

(defun orb-pdf-scrapper--YAP-keygen-fn (entry)
  "Generate `year-abbreviaton-page' key from ENTRY."
  (let* ((authors
          (or (bibtex-completion-get-value "author" entry) "N/A"))
         (journal
          (or (bibtex-completion-get-value "journal" entry) "N/A"))
         (year
          (or (bibtex-completion-get-value "date" entry) "N/A"))
         (volume
          (or (bibtex-completion-get-value "volume" entry) "N/A"))
         (pages
          (replace-regexp-in-string
           "\\([ –-]+\\)" "--"
           (or (bibtex-completion-get-value "pages" entry) "N/A")
           nil nil 1))
         ;; TODO: would be nice to already have this regex in
         ;; `orb-pdf-scrapper--journal-titles'
         ;; regexp similar "J[ ,.;–-]Am[ ,.;–-]Chem[ ,.;–-]Soc[ ,.;–-]"
         (journal-regexp (format "^%s$" (orb-pdf-scrapper--str-to-regexp journal)))
         ;; (journal . abbrev)
         ;; instead of comparing strings, try to match the key with
         ;; the above regexp
         (journal-cons (or (assoc journal-regexp
                                  orb-pdf-scrapper--journal-titles
                                  (lambda (a b) (string-match b a)))
                           ;; if not, just return (journal . nil)
                           (list journal)))
         ;; use verb field to store journal abbreviation
         (verb (or (bibtex-completion-get-value "verb" entry)
                   (cdr journal-cons)))
         ;; Generate new key similar to "2020-JACS-1999"
         ;; or "2020-JFC-199-1999"
         (new-key (concat year "-"
                          verb
                          (and verb (s-suffix? "-" verb) volume) "-"
                          (and pages
                               (string-match "\\([0-9]*\\)" pages)
                               (match-string 1 pages))))
         (invalid-key-regexp "N/A\\|--")
         (validp (unless (string-match invalid-key-regexp new-key)
                   'valid))
         (fields (list
                  ;; Perhaps replace journal with a match from the
                  ;; database: in most cases, fix punctuation
                  ;; characters
                  (cons "journal" (car journal-cons))
                  ;; Set verb to the retrieved abbreviation or journal
                  ;; otherwise
                  (if verb
                      (cons "verb" verb)
                    (cons "verb" "N/A"))
                  (cons "pages" pages))))
    (list :key new-key
          :validp validp
          :fields fields
          :entry (format "%s %s %s %s %s"
                         authors journal year volume pages))))


;; * Dispatcher functions

(defun orb-pdf-scrapper--refresh-mode (context)
  "Restart `orb-pdf-scrapper-mode' in new CONTEXT."
  (cl-case context
    ('edit-bib
     (text-mode))
    ('checkout-interactive
     (bibtex-mode)
     (bibtex-set-dialect 'BibTeX t))
    (t
     (unwind-protect
         (error "Oops...something went wrong.  \
Pressing the RED button, just in case")
       (orb-pdf-scrapper-dispatcher 'error))))
  (orb-pdf-scrapper-mode -1)
  (orb-pdf-scrapper--put :context context)
  (orb-pdf-scrapper-mode +1)
  (goto-char (point-min)))

(defun orb-pdf-scrapper--current-context-p (context)
  "Return t if CONTEXT is current context."
  (eq context (orb-pdf-scrapper--get :context)))

(defun orb-pdf-scrapper--edit-txt ()
  "Edit text references in `orb-pdf-scrapper--buffer'."
  (orb--when-current-context! 'edit-txt
    (if-let ((temp-txt (orb-pdf-scrapper--get :txt-file)))
        (progn
          (pop-to-buffer orb-pdf-scrapper--buffer)
          (erase-buffer)
          (insert-file-contents temp-txt)
          (setq buffer-file-name nil)
          (setq mark-active nil)
          (orb-pdf-scrapper--refresh-mode 'edit-bib))
      (let ((temp-txt (orb--temp-file "orb-pdf-scrapper-" ".txt"))
            (pdf (orb-pdf-scrapper--get :pdf-file)))
        (orb-pdf-scrapper--put :txt-file temp-txt)
        (let ((same-window-buffer-names (list orb-pdf-scrapper--buffer)))
          (pop-to-buffer orb-pdf-scrapper--buffer))
        (erase-buffer)
        (setq buffer-file-name nil)
        (setq mark-active nil)
        (orb--with-message! (format "Scrapping %s.pdf" (f-base pdf))
          (orb-anystyle 'find
            :format 'ref
            :layout nil
            :finder-model orb-anystyle-finder-model
            :input pdf
            :stdout t
            :buffer orb-pdf-scrapper--buffer))
        (orb-pdf-scrapper--refresh-mode 'edit-bib)))))

(defun orb-pdf-scrapper--edit-bib ()
  "Generate and edit BibTeX data in `orb-pdf-scrapper--buffer'."
  (orb--when-current-context! 'edit-bib
    (let* ((temp-bib (or (orb-pdf-scrapper--get :bib-file)
                         (orb--temp-file "orb-pdf-scrapper-" ".bib"))))
      (orb-pdf-scrapper--put :bib-file temp-bib)
      (pop-to-buffer orb-pdf-scrapper--buffer)
      (setq buffer-file-name nil)
      (orb--with-message! "Generating BibTeX data"
        (orb-anystyle 'parse
          :format 'bib
          :parser-model orb-anystyle-parser-model
          :input (orb-pdf-scrapper--get :txt-file)
          :stdout t
          :buffer orb-pdf-scrapper--buffer)
        (write-region (orb--buffer-string) nil temp-bib nil -1)))
    (orb-pdf-scrapper--refresh-mode 'checkout-interactive)))

(defun orb-pdf-scrapper--checkout ()
  "Parse bib buffer and populate `orb-pdf-scrapper--sorted-refs'."
  (let ((context (orb-pdf-scrapper--get :context)))
    (cond ((equal context 'checkout-non-interactive)
           (orb--with-message! "Generating citation keys"
             (orb--with-scrapper-buffer!
               (orb-anystyle 'parse
                 :format 'bib
                 :parser-model orb-anystyle-parser-model
                 :input (orb-pdf-scrapper--get :txt-file)
                 :stdout t
                 :buffer orb-pdf-scrapper--buffer)
               (bibtex-mode)
               (bibtex-set-dialect 'BibTeX t)
               (condition-case nil
                   (orb-pdf-scrapper-generate-keys)
                 (t
                  (orb-pdf-scrapper--put :context 'checkout-interactive)
                  (orb-pdf-scrapper--checkout)))))
           (orb-pdf-scrapper--insert-org))
          ((equal context 'checkout-interactive)
           (when (> (cl-random 100) 98)
             (orb--with-message! "Pressing the RED button"))
           (when (buffer-modified-p (get-buffer orb-pdf-scrapper--buffer))
             (orb-pdf-scrapper-generate-keys))
           (orb-pdf-scrapper--insert-org))
          (t
           (orb-pdf-scrapper-dispatcher 'error)))))

(defun orb-pdf-scrapper--insert-org ()
  "Checkout from Orb PDF Scrapper interactive mode."
  (pop-to-buffer (orb-pdf-scrapper--get :original-buffer))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (insert "\n* References\n")
      (dolist (ref-group orb-pdf-scrapper--sorted-refs)
        (insert (format "** %s\n" (car ref-group)))
        (dolist (ref (cdr ref-group))
          (insert (format "- %s\n" ref)))
        (insert "\n"))))
  (orb-pdf-scrapper--cleanup))

(defun orb-pdf-scrapper--cleanup ()
  "Clean up."
  (and (get-buffer orb-pdf-scrapper--buffer)
       (kill-buffer orb-pdf-scrapper--buffer))
  (set-window-configuration (orb-pdf-scrapper--get :window-conf))
  (dolist (prop (list :running :context :current-key
                      :prevent-concurring :pdf-file :txt-file :bib-file
                      :global-bib :window-conf :original-buffer))
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
;;; End of code adopted from org-capture.el

(defun orb-pdf-scrapper--format-header-line ()
  "Return formatted buffer header line depending on context."
  (substitute-command-keys
   (format "\\<orb-pdf-scrapper-mode-map>Orb PDF Scrapper: %s.  %s"
           (orb-pdf-scrapper--get :current-key)
           (cl-case (orb-pdf-scrapper--get :context)
             ('edit-bib
              "\
Generate BibTeX `\\[orb-pdf-scrapper-dispatcher]', \
sanitize text `\\[orb-pdf-scrapper-sanitize-text]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             ('checkout-interactive
              "\
Finish `\\[orb-pdf-scrapper-dispatcher]', \
generate keys `\\[orb-pdf-scrapper-generate-keys]', \
return to text `\\[orb-pdf-scrapper-return-to-txt]', \
abort `\\[orb-pdf-scrapper-kill]'.")
             (t
              "\
Press the RED button `\\[orb-pdf-scrapper-kill]'.")))))

(defun orb-pdf-reference-scrapper--update-keymap ()
  "Update `orb-pdf-scrapper-mode-map' according to context.
Context is read from `orb-pdf-scrapper--plist' property `:context'."
  ;; context is a "future action"
  (cl-case (orb-pdf-scrapper--get :context)
    ;; context is opportunistic
    ;; we are actually editing txt
    ('edit-bib
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-u" #'orb-pdf-scrapper-sanitize-text))
    ;; we are editing bib
    ('checkout-interactive
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-u" #'orb-pdf-scrapper-generate-keys)
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-r" #'orb-pdf-scrapper-return-to-txt))
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
                (orb-pdf-scrapper--get :bib-file) nil -1)
  (orb--with-message! "Generating citation keys"
    (let ((bibtex-completion-bibliography
           (or (orb-pdf-scrapper--get :bib-file)
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
                 (fields-to-set (plist-get key-plist :fields))
                 (formatted-entry (plist-get key-plist :entry)))
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
             (cons (cons new-key formatted-entry) validp) refs))
          (bibtex-end-of-entry)
          ;; go to next entry
          (bibtex-skip-to-valid-entry)))
      (orb-pdf-scrapper--sort-refs refs))
    ;; TODO: this is not needed perhaps
    (write-region (orb--buffer-string) nil
                  (orb-pdf-scrapper--get :bib-file) nil -1)))

(defun orb-pdf-scrapper-sanitize-text (&optional contents)
  "Run string processing in current buffer.
Try to get every reference onto newline.  Return this buffer's
contents (`orb--buffer-string').

If optional string CONTENTS was specified, run processing on this
string instead.  Return modified CONTENTS."
  (interactive)
  (let* ((rx1 '(and "(" (** 1 2 (any "0-9")) ")"))
         (rx2 '(and "[" (** 1 2 (any "0-9")) "]"))
         (rx3 '(and "(" (any "a-z") ")"))
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

(defun orb-pdf-scrapper-return-to-txt ()
  "Return to editing text references in Orb PDF Scrpapper."
  (interactive)
  (orb--when-current-context! 'checkout-interactive
    (orb-pdf-scrapper-dispatcher 'edit-txt)))

(defun orb-pdf-scrapper-dispatcher (&optional context)
  "Run different Orb PDF Scrapper actions depending on CONTEXT.
CONTEXT can be passed directly or with `orb-pdf-scrapper--plist'
by setting `:context' property.  Contexts are sort of
opportunistic and designate a future intended action or editing mode.

Recognized contexts are:
==========
'edit-txt                  - switch to txt editing mode
'edit-bib                  - switch to bib editing mode
'checkout-interactive     - finalize bib editing after interactive session
'checkout-non-interactive - generate bib omiting interactive bib editing

Passing or setting any other context will kill the process.

This function also checks `:prevent-concurring' property in
`orb-pdf-scrapper--plist' and will suggest to restart the process
if its value is non-nil."
  ;; TODO: check for whether user killed any of the buffers
  (interactive)
  (let ((context (or context (orb-pdf-scrapper--get :context))))
    ;; in case context was passed as an argument
    (orb-pdf-scrapper--put :context context)
    (cond
     ;; Prevent another Orb PDF Scrapper process
     ;; Ask user whether to kill currently running process
     ((orb-pdf-scrapper--get :prevent-concurring)
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
        (orb-pdf-scrapper--put :prevent-concurring nil)))
     ((equal context 'edit-txt)
      (orb-pdf-scrapper--edit-txt))
     ((equal context 'edit-bib)
      (orb--with-scrapper-buffer!
        (let ((data (orb--buffer-string)))
          (write-region data nil
                        (orb-pdf-scrapper--get :txt-file) nil -1)
          (orb-pdf-scrapper--put :txt-data data)
          (orb-pdf-scrapper--put :txt-undo-list buffer-undo-list)))
      (if (y-or-n-p "Review the bib file? ")
          (orb-pdf-scrapper--edit-bib)
        ;; proceed to checkout; bib file was not edited interactively
        (orb-pdf-scrapper--put :context 'checkout-non-interactive)
        (orb-pdf-scrapper--checkout)))
     ;; proceed to checkout; bib file was edited interactively
     ((equal context 'checkout-interactive)
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
      (orb-pdf-scrapper--cleanup)))))

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
    (orb-pdf-scrapper--put :context 'edit-txt
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
;; fill-column: 70
;; End:
