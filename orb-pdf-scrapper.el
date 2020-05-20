;;; orb-pdf-scrapper.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex, org-ref
;; Version: 0.2.3
;; Package-Requires: ((emacs "26.1") (f "0.20.0") (s "1.12.0") (org "9.3") (org-roam "1.0.0") (bibtex-completion "2.0.0"))

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

(require 'org-roam-bibtex)
(require 'orb-macs)
(require 'orb-anystyle)

;; * Customize definitions

;; TODO: make it defcustom
(defvar orb-pdf-scrapper-keygen-fun #'orb-pdf-scrapper--YAP-keygen-fn
  "Function to generate citation key.
It should take a bibtex entry as returned by
`bibtex-completion-get-entry' and return a plist of form
\(:key generated_citekey
 :validp non-nil_if_key_is_valid_according_to_some_internal_logic
 :fields cons_cell_(field_name_._field_value)_to_write_into_entry
 :entry formatted_entry)
Technically, only :key and :entry are strictly required.")

;; * Helper functions

(defun orb--tsv-to-list (file)
  "Convert tab-separated values FILE into a list."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((orig-list (split-string (buffer-string) "\n" t)))
      (--map (--> it
                  (split-string it "\t" t)
                  (cons (nth 2 it) (car it)))
             orig-list))))

(defvar orb-pdf-scrapper--journal-title-abbrevs)
;; read journal title abbreviations
(setq orb-pdf-scrapper--journal-title-abbrevs
      (orb--tsv-to-list
       (f-join
        (f-dirname
         (or load-file-name buffer-file-name))
        "journal_titles.tsv")))

(defvar orb-pdf-scrapper--validated-refs nil)

(defvar orb-pdf-scrapper--buffer "*Orb PDF Scrapper*"
  "Orb PDF Scrapper special buffer.")

(defvar orb-pdf-scrapper--plist nil
  "Communication channel for Orb PDF Scrapper.")

(defmacro orb-with-scrapper-buffer (&rest body)
  "Execute BODY with `orb-pdf-scrapper--buffer' as current.
If the buffer does not exist it will be created."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (set-buffer (get-buffer-create orb-pdf-scrapper--buffer))
     ,@body))

(defun orb-pdf-scrapper--validate-refs (refs)
  "Validate references REFS.
Auxiliary function for `orb-pdf-scrapper-generate-keys'.
REFS should be an alist of form ((CITEKEY . FORMATTED-ENTRY) . VALIDP).

References marked by `orb-pdf-scrapper-keygen-fun' function
as valid are sorted into four groups:
'in-roam - those that are in the `org-roam' database;
'in-bib - those that are in `bibtex-completion-bibliography' file(s);
'valid - marked valid by the keygen function but are not
  in the above two groups;
'invalid - marked invalid by the keygen function."
  (let ((bibtex-completion-bibliography (orb-pdf-scrapper--get :global-bib))
        ;; When using a quoted list here, validated-refs is not erased
        ;; upon consecutive runs
        (validated-refs (list (list 'in-roam) (list 'in-bib)
                              (list 'valid) (list 'invalid))))
    (dolist (citation refs)
      (cond ((org-roam-db-query [:select [ref]
                                 :from refs
                                 :where (= ref $s1)]
                                (format "%s" (caar citation)))
             (cl-pushnew
              (format "cite:%s" (caar citation))
              (cdr (assoc 'in-roam validated-refs))))
            ((bibtex-completion-get-entry (caar citation))
             (cl-pushnew
              (format "cite:%s" (caar citation))
              (cdr (assoc 'in-bib validated-refs))))
            ((cdr citation)
             (cl-pushnew
              (format "cite:%s %s" (caar citation) (cdar citation))
              (cdr (assoc 'valid validated-refs))))
            (t
             (cl-pushnew
              (format "%s" (cdar citation))
              (cdr (assoc 'invalid validated-refs))))))
    (setq orb-pdf-scrapper--validated-refs validated-refs)))

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
           "[– ]" "--" (or (bibtex-completion-get-value "pages" entry)
                        "N/A")))
         ;; TODO: would be nice to already have this regex in
         ;; `orb-pdf-scrapper--journal-title-abbrevs'
         ;; regexp similar "J[ ,.;-]Am[ ,.;-]Chem[ ,.;-]Soc[ ,.;-]"
         (journal-regexp
          (format "^%s[ ,.;-]*$"
                  (--reduce (format "%s[ ,.;-]+%s" acc it)
                            (split-string journal "[ ,.;-]+" t "[ ]+"))))
         ;; (journal . abbrev)
         ;; instead of comparing strings, try to match the key with
         ;; the above regexp
         (journal-cons (or (assoc journal-regexp
                                  orb-pdf-scrapper--journal-title-abbrevs
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
                    (cons "verb" (car journal-cons)))
                  (cons "pages" pages))))
    (list :key new-key
          :validp validp
          :fields fields
          :entry (format "%s %s %s %s %s"
                         authors journal year volume pages))))


;; * Dispatcher functions

(defun orb-pdf-scrapper--edit-txt ()
  "Get references from pdf and edit them in a temporary buffer."
  (if-let ((temp-txt (orb-pdf-scrapper--get :txt-file)))
      (progn
        (orb-pdf-scrapper--put :txt-file temp-txt)
        (switch-to-buffer orb-pdf-scrapper--buffer)
        (erase-buffer)
        (insert-file-contents temp-txt)
        (setq buffer-file-name nil)
        (setq mark-active nil))
    (let ((temp-txt (orb-temp-file "orb-pdf-scrapper-" ".txt"))
          (pdf (orb-pdf-scrapper--get :pdf-file)))
      (orb-pdf-scrapper--put :txt-file temp-txt)
      (switch-to-buffer-other-window
       (get-buffer-create orb-pdf-scrapper--buffer))
      (erase-buffer)
      (setq buffer-file-name nil)
      (setq mark-active nil)
      (orb-with-message (format "Scrapping %s.pdf" (f-base pdf))
        (orb-anystyle 'find
          :format 'ref
          :layout nil
          :input pdf
          :stdout t
          :buffer orb-pdf-scrapper--buffer))))
  (goto-char (point-min))
  (text-mode)
  (orb-pdf-scrapper-mode -1)
  (orb-pdf-scrapper--put :context 'edit-bib)
  (orb-pdf-scrapper-mode +1))

(defun orb-pdf-scrapper--review-bib ()
  "Review the generated temporary bib file."
  (if (not (equal (orb-pdf-scrapper--get :context) 'edit-bib))
      (progn
        (orb-pdf-scrapper--put :context 'error)
        (orb-pdf-scrapper-dispatcher))
    (let* ((temp-bib (or (orb-pdf-scrapper--get :bib-file)
                         (orb-temp-file "orb-pdf-scrapper-" ".bib"))))
      (orb-pdf-scrapper--put :bib-file temp-bib)
      (orb-with-scrapper-buffer
        (orb-with-message "Generating BibTeX data"
          (orb-anystyle 'parse
            :format 'bib
            :input (orb-pdf-scrapper--get :txt-file)
            :stdout t
            :buffer orb-pdf-scrapper--buffer)
          (write-region (buffer-string) nil temp-bib nil -1)))
      (switch-to-buffer orb-pdf-scrapper--buffer)
      (setq buffer-file-name nil)
      (bibtex-mode)
      (bibtex-set-dialect 'BibTeX t)
      (orb-pdf-scrapper-mode -1)
      (orb-pdf-scrapper--put :context 'parse-bib-interactive)
      (orb-pdf-scrapper-mode +1)
      (goto-char (point-min)))))

(defun orb-pdf-scrapper--checkout ()
  "Parse bib buffer and populate `orb-pdf-scrapper--validated-refs'."
  (let ((context (orb-pdf-scrapper--get :context)))
    (cond ((equal context 'parse-bib-non-interactive)
           (orb-with-message "Generating citation keys"
             (orb-with-scrapper-buffer
               (orb-anystyle 'parse
                 :format 'bib
                 :input (orb-pdf-scrapper--get :txt-file)
                 :stdout t
                 :buffer orb-pdf-scrapper--buffer)
               (bibtex-mode)
               (bibtex-set-dialect 'BibTeX t)
               (condition-case nil
                   (orb-pdf-scrapper-generate-keys)
                 (t
                  (orb-pdf-scrapper--put :context 'parse-bib-interactive)
                  (orb-pdf-scrapper--checkout)))))
           (orb-pdf-scrapper--insert))
          ((equal context 'parse-bib-interactive)
           (when (> (cl-random 100) 98)
             (orb-with-message "Pressing the RED button"))
           (when (buffer-modified-p (get-buffer orb-pdf-scrapper--buffer))
             (orb-pdf-scrapper-generate-keys))
           (orb-pdf-scrapper--insert))
          (t
           (orb-pdf-scrapper--put :context 'error)
           (orb-pdf-scrapper-dispatcher)))))

(defun orb-pdf-scrapper--insert ()
  "Checkout from Orb PDF Scrapper interactive mode."
  (set-buffer (orb-pdf-scrapper--get :original-buffer))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (insert "\n* References\n")
      (dolist (ref-group orb-pdf-scrapper--validated-refs)
        (insert (format "** %s\n" (car ref-group)))
        (dolist (ref (cdr ref-group))
          (insert (format "- %s\n" ref)))
        (insert "\n"))))
  (orb-pdf-scrapper--cleanup))

(defun orb-pdf-scrapper--cleanup ()
  "Clean up."
  ;; (let ((txt (orb-pdf-scrapper--get :txt-file))
  ;;       (bib (orb-pdf-scrapper--get :bib-file)))
  ;;   (dolist (buf (list txt bib))
  ;;     (and buf
  ;;          (setq buf (find-buffer-visiting buf))
  ;;          (set-buffer buf)
  ;;          (not (set-buffer-modified-p nil))
  ;;          (kill-buffer buf))))
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
;;
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
  nil " ORS" orb-pdf-scrapper-mode-map
  (when orb-pdf-scrapper-mode
    (orb-pdf-reference-scrapper--update-keymap)
    (setq-local
     header-line-format
     (orb-pdf-scrapper--format-header-line))))

(defun orb-pdf-scrapper--put (&rest props)
  "Add properties PROPS to `orb-pdf-scrapper--plist'."
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
   (cl-case (orb-pdf-scrapper--get :context)
     ('edit-bib
      "\\<orb-pdf-scrapper-mode-map>Orb PDF Scrapper.  \
Proceed to bibtex `\\[orb-pdf-scrapper-dispatcher]', \
sanitize text `\\[orb-pdf-scrapper-sanitize-text]', \
abort `\\[orb-pdf-scrapper-kill]'.")
     ('parse-bib-interactive
      "\\<orb-pdf-scrapper-mode-map>Orb PDF Scrapper.  \
Finish `\\[orb-pdf-scrapper-dispatcher]', \
generate keys `\\[orb-pdf-scrapper-generate-keys]', \
return to text `\\[orb-pdf-scrapper-return-to-txt]', \
abort `\\[orb-pdf-scrapper-kill]'.")
     (t
      "\\<orb-pdf-scrapper-mode-map>Orb PDF Scrapper.  \
Press the RED button `\\[orb-pdf-scrapper-kill]'."))))

(defun orb-pdf-reference-scrapper--update-keymap ()
  "Update `orb-pdf-scrapper-mode-map' according to context."
  (cl-case (orb-pdf-scrapper--get :context)
    ('edit-bib
     (define-key orb-pdf-scrapper-mode-map
       "\C-c\C-u" #'orb-pdf-scrapper-sanitize-text))
    ('parse-bib-interactive
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
Validate and push the retreived references to
`orb-pdf-scrapper--validate-refs'."
  (interactive)
  (orb-with-scrapper-buffer
    (write-region (buffer-string) nil
                  (orb-pdf-scrapper--get :bib-file) nil -1))
  (orb-with-message "Generating citation keys"
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
                 (key-plist (funcall orb-pdf-scrapper-keygen-fun entry))
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
            (cl-pushnew
             (cons (cons new-key formatted-entry) validp) refs))
          (bibtex-end-of-entry)
          (bibtex-skip-to-valid-entry)))
      (orb-pdf-scrapper--validate-refs refs))
    (orb-with-scrapper-buffer
      (write-region (buffer-string) nil
                    (orb-pdf-scrapper--get :bib-file) t -1))))

(defun orb-pdf-scrapper-sanitize-text (&optional contents)
  "Run string processing on CONTENTS.
Try to get every reference into newline and remove reference
numbers."
  (interactive)
  (let* ((contents (or contents (buffer-string)))
         (rx1 '(and "(" (** 1 2 (any "0-9")) ")"))
         (rx2 '(and "[" (** 1 2 (any "0-9")) "]"))
         (rx3 '(and "(" (any "a-z") ")"))
         (regexp (rx-to-string
                 `(group-n 1 (or (or (and ,rx1 " " ,rx3)
                                     (and ,rx2 " " ,rx3))
                                 ,rx1 ,rx2 ,rx3)) t))
         (result (--> contents
                      (s-replace "\n" " " it)
                      (s-replace-regexp regexp "\n\\1" it))))
    (erase-buffer)
    (insert result)
    (goto-char (point-min))))

(defun orb-pdf-scrapper-return-to-txt ()
  "Return to editing text references in Orb PDF Scrpapper."
  (interactive)
  (if (not (eq  (orb-pdf-scrapper--get :context) 'parse-bib-interactive))
      (progn
        (orb-pdf-scrapper--put :context 'error)
        (orb-pdf-scrapper-dispatcher))
    (orb-pdf-scrapper--put :context 'edit-txt)
    (orb-pdf-scrapper-dispatcher)))

(defun orb-pdf-scrapper-dispatcher ()
  "Finalize editing Orb PDF Scrapper intermediate buffer."
  ;; TODO: check for whether user killed any of the buffers
  (interactive)
  (let ((context (orb-pdf-scrapper--get :context)))
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
            (orb-with-message "Killing current process"
              (orb-pdf-scrapper--cleanup))
            (orb-pdf-scrapper-run (orb-pdf-scrapper--get :new-key)))
        ;; Do nothing
        (orb-pdf-scrapper--put :prevent-concurring nil)))
     ((equal context 'edit-txt)
      (orb-pdf-scrapper--edit-txt))
     ((equal context 'edit-bib)
      (orb-with-scrapper-buffer
        (let ((data (buffer-string)))
          (write-region data nil
                        (orb-pdf-scrapper--get :txt-file) nil -1)
          (orb-pdf-scrapper--put :txt-data data)))
      (if (y-or-n-p "Review the bib file? ")
          (orb-pdf-scrapper--review-bib)
        ;; proceed to checkout; bib file was not edited interactively
        (orb-pdf-scrapper--put :context 'parse-bib-non-interactive)
        (orb-pdf-scrapper--checkout)))
     ;; proceed to checkout; bib file was edited interactively
     ((equal context 'parse-bib-interactive)
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
    (setq orb-pdf-scrapper--validated-refs nil)
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
