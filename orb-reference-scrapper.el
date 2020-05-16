;;; orb-reference-scrapper.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

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

;;; Commentary:
;;

;;; Code:
;; * Library requires

(require 'org-roam-bibtex)

(defun orb--tsv-to-list (file)
  "Convert tab-separated values FILE into a list."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((orig-list (split-string (buffer-string) "\n" t)))
      (--map (--> it
                  (split-string it "\t" t)
                  (cons (nth 2 it) (car it)))
             orig-list))))

(defvar orb-reference-scrapper--journal-title-abbrevs)
;; read journal title abbreviations
(setq orb-reference-scrapper--journal-title-abbrevs
      (orb--tsv-to-list
       (f-join
        (f-dirname
         (or load-file-name buffer-file-name))
        "journal_titles.tsv")))

(defvar orb-reference-scrapper--refs nil)
(defvar orb-reference-scrapper--validated-refs '((in-roam) (in-bib) (valid) (invalid)))

(defun orb-reference-scrapper--validate-refs ()
  "Validate references.
References marked by `orb-reference-scrapper-keygen-fun' function
as valid will are sorted into four groups:
'in-roam - those that are in the `org-roam' database;
'in-bib - those that are in `bibtex-completion-bibliography' file(s);
'valid - marked valid by the keygen function but are not
  in the above two groups;
'invalid - marked invalid by the keygen function."
  (dolist (citation orb-reference-scrapper--refs)
    (cond ((org-roam-db-query [:select [ref] :from refs :where (like ref $r1)] (format "%%%s%%" (caar citation)))
           ;; (setf (car citation) (format "cite:%s" (caar citation))
           ;;       (cdr citation) 'in-roam)
           (cl-pushnew (format "cite:%s" (caar citation)) (cdr (assoc 'in-roam orb-reference-scrapper--validated-refs)))
           )
          ((bibtex-completion-get-entry (caar citation))
           ;; (setf (car citation) (format "cite:%s" (caar citation))
           ;;       (cdr citation) 'in-bib)
           (cl-pushnew (format "cite:%s" (caar citation)) (cdr (assoc 'in-bib orb-reference-scrapper--validated-refs))))
          ((cdr citation)
           ;; (setf (car citation) (format "cite:%s %s" (caar citation) (cdar citation))
           ;;       (cdr citation) 'valid) ; in case it has been marked differently by the keygen-fun
           (cl-pushnew (format "cite:%s %s" (caar citation) (cdar citation)) (cdr (assoc 'valid orb-reference-scrapper--validated-refs))))
          (t
           ;; (setf (car citation) (format "%s" (cdar citation))
           ;;       (cdr citation) 'invalid)
           (cl-pushnew (format "%s" (cdar citation)) (cdr (assoc 'invalid orb-reference-scrapper--validated-refs))))))
  ;; (setq orb-reference-scrapper--validated-refs
  ;;       (--reduce-r-from (progn
  ;;                          (cl-pushnew (car it) (cdr (assoc (cdr it) acc)))
  ;;                          acc)
  ;;                        '((in-roam . nil) (in-bib . nil) (valid . nil) (invalid . nil))
  ;;                        (nreverse orb-reference-scrapper--refs)))
  )

;; TODO: make it defcustom
(defvar orb-reference-scrapper-keygen-fun #'orb-reference-scrapper--year-verb-page-keygen-fun
  "Function to generate citation key.
It should take a bibtex entry as returned by
`bibtex-completion-get-entry' and return a plist of form
\(:key generated_citekey
 :validp non-nil_if_key_is_valid_according_to_some_internal_logic
 :fields cons_cell_(field_name_._field_value)_to_write_into_entry
 :entry formatted_entry)
Technically, only :key and :entry are strictly required.")

(defun orb-reference-scrapper--year-verb-page-keygen-fun (entry)
  "Generate `year-verb-page' key from ENTRY."
  (let* ((authors (or (bibtex-completion-get-value "author" entry) "N/A"))
         (journal (or (bibtex-completion-get-value "journal" entry) "N/A"))
         (year (or (bibtex-completion-get-value "date" entry) "N/A"))
         (volume (or (bibtex-completion-get-value "volume" entry) "N/A"))
         (pages (or (bibtex-completion-get-value "pages" entry) "N/A"))
         ;; TODO: would be nice to already have this regex in
         ;; `orb-reference-scrapper--journal-title-abbrevs'
         ;; regexp similar "J[ ,.;]Am[ ,.;]Chem[ ,.;]Soc[ ,.;]"
         (journal-regexp
          (format "^%s[ ,.;]*$" (--reduce (format "%s[ ,.;]+%s" acc it)
                                          (split-string journal "[,.;]+" t "[ ]+"))))
         ;; (journal . abbrev)
         ;; instead of comparing strings, try to match the key with the above regexp
         (journal-cons (or (assoc journal-regexp orb-reference-scrapper--journal-title-abbrevs
                                  (lambda (a b) (string-match b a)))
                           ;; if not, just return (journal . nil)
                           (list journal)))
         ;; use verb field to store journal abbreviation
         (verb (cdr journal-cons))
         ;; Generate new key similar to "2020-JACS-1999" or "2020-JFC-199-1999"
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
                  ;; Perhaps replace journal with a match from the database:
                  ;; in most cases, fix punctuation characters
                  (cons "journal" (car journal-cons))
                  ;; Set verb to the retrieved abbreviation or journal otherwise
                  (if verb
                      (cons "verb" verb)
                    (cons "verb" (car journal-cons))))))
    (list :key new-key
          :validp validp
          :fields fields
          :entry (format "%s %s %s %s %s" authors journal year volume pages))))

(defun orb-reference-scrapper-generate-keys (&optional bib-file)
  "Generate citation keys in the current buffer.
Validate and push the retreived references to
`orb-reference-scrapper--validated-refs'.

Optional BIB-FILE is required when this function is called in a
temporary buffer that does not have corresponding file."
  (interactive)
  (save-excursion
    (let ((bibtex-completion-bibliography (or bib-file (buffer-file-name)))
          (bibtex-help-message nil))
      (goto-char (point-min))
      (bibtex-skip-to-valid-entry)
      (while (not (eobp))
        (let* ((key (bibtex-key-in-head))
               ;; TODO: consider using more more low-level parsebib
               (entry (bibtex-completion-get-entry key))
               (key-plist (funcall orb-reference-scrapper-keygen-fun entry))
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
          (cl-pushnew (cons (cons new-key formatted-entry) validp) orb-reference-scrapper--refs))
        (bibtex-end-of-entry)
        (bibtex-skip-to-valid-entry))))
  (orb-reference-scrapper--validate-refs))


;; * Minor mode

(defvar orb-reference-scrapper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'orb-reference-scrapper--dispatcher)
    (define-key map "\C-c\C-k" #'orb-reference-scrapper--kill)
    map)
  "Keymap for `orb-reference-scrapper-mode' minor mode.")

(defvar orb-reference-scrapper-mode-hook nil
  "Hook for the `orb-reference-scrapper-mode' minor mode.")

(define-minor-mode orb-reference-scrapper-mode
  "Minor mode for special key bindings in a orb-reference-scrapper buffer.

Turning on this mode runs the normal hook `orb-reference-scrapper-mode-hook'."
  nil " ORS" orb-reference-scrapper-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<orb-reference-scrapper-mode-map>Orb reference scrapper.  Finish \
`\\[orb-reference-scrapper--dispatcher]', abort `\\[orb-reference-scrapper--kill]'.")))

(defvar orb-reference-scrapper--buffer "*Orb Reference Scrapper*")

(defvar orb-reference-scrapper--mode-plist '(:temp-dir "orb-reference-scrapper/"))

(defun orb-reference-scrapper--put (&rest props)
  "Add properties PROPS to `orb-reference-scrapper--mode-plist'."
  (while props
    (setq orb-reference-scrapper--mode-plist
          (plist-put orb-reference-scrapper--mode-plist (pop props) (pop props)))))

(defun orb-reference-scrapper--get (prop)
  "Get PROP from `orb-reference-scrapper--mode-plist'."
  (plist-get orb-reference-scrapper--mode-plist prop))

(defun orb-reference-scrapper--dispatcher ()
  "Finalize editing Orb Reference Scrapper intermediate buffer."
  ;; TODO: check for whether user killed any of the buffers
  (interactive)
  (let ((context (orb-reference-scrapper--get :context)))
    (cond
     ((equal context 'edit-txt)
      (orb-reference-scrapper--edit-txt))
     ((equal context 'edit-bib)
      (with-current-buffer orb-reference-scrapper--buffer
        (write-region (buffer-string) nil
                      (orb-reference-scrapper--get :txt) nil -1))
      (if (y-or-n-p "Review the bib file? ")
          (orb-reference-scrapper--review-bib)
        ;; proceed to checkout; bib file was not edited interactively
        (orb-reference-scrapper--put :context 'parse-bib-non-interactive)
        (orb-reference-scrapper--checkout)))
     ;; proceed to checkout; bib file was edited interactively
     ((equal context 'parse-bib-interactive)
      (save-buffer)
      (orb-reference-scrapper--checkout))
     (t
      (when (> (cl-random 10) 9)
        (message "Oops...")
        (sleep-for 1)
        (message "Oops...Did you just ACCIDENTALLY press the RED button?")
        (sleep-for 2)
        (message "Activating self-destruction subroutine...")
        (sleep-for 1)
        (message "Activating self-destruction subroutine...Bye-bye")
        (sleep-for 2))
      (orb-reference-scrapper--cleanup)))))

(defun orb-reference-scrapper--edit-txt ()
  "Get references from pdf and edit them in a temporary buffer."
  (let ((temp-txt (make-temp-file "orb-ref-scrapper-" nil ".txt"))
        (pdf (orb-reference-scrapper--get :pdf)))
    (orb-reference-scrapper--put :txt temp-txt)
    (switch-to-buffer-other-window
     (get-buffer-create orb-reference-scrapper--buffer))
    (erase-buffer)
    (setq buffer-file-name nil)
    (setq mark-active nil)
    (message "Scrapping %s.pdf..." (f-base pdf))
    (shell-command
     (format "anystyle -f ref find --no-layout \"%s\" -" pdf)
     orb-reference-scrapper--buffer)
    (let* ((contents (buffer-string))
           (numbered-regex
            "\\(([0-9]\\{1,3\\}) \\|\\[[0-9]\\{1,3\\}] \\) ")
           (lettered-regex "([a-z]) ")
           (result (--> contents
                        (s-replace "\n" "" it)
                        (split-string it numbered-regex)
                        (--map (split-string it lettered-regex) it)
                        (-flatten it)
                        (s-join "\n" it))))
      (erase-buffer)
      (insert result))
    (goto-char (point-min))
    (orb-reference-scrapper--put :context 'edit-bib)
    (message "Scrapping %s.pdf...done" (f-base pdf))
    (orb-reference-scrapper-mode +1)))

(defun orb-reference-scrapper--review-bib ()
  "Review the generated temporary bib file."
  (if (not (equal (orb-reference-scrapper--get :context) 'edit-bib))
      (progn
        (orb-reference-scrapper--put :context 'error)
        (orb-reference-scrapper--dispatcher))
    (let* ((temp-bib (make-temp-file "orb-ref-scrapper-" nil ".bib")))
      (orb-reference-scrapper--put :bib temp-bib)
      (with-current-buffer orb-reference-scrapper--buffer
        (shell-command
         (format "anystyle -f bib parse \"%s\" -"
                 (orb-reference-scrapper--get :txt))
         orb-reference-scrapper--buffer)
        (write-region (buffer-string) nil temp-bib nil -1)
        (kill-buffer (current-buffer)))
      (find-file temp-bib)
      (bibtex-mode)
      (bibtex-set-dialect 'BibTeX t)
      (orb-reference-scrapper--put :context 'parse-bib-interactive)
      (orb-reference-scrapper-mode +1)
      (goto-char (point-min)))))

(defun orb-reference-scrapper--checkout ()
  "Parse bib file or buffer and populate `orb-reference-scrapper--validated-refs'."
  (message "Generating citation keys...")
  (cond ((equal (orb-reference-scrapper--get :context) 'parse-bib-non-interactive)
         (with-current-buffer (get-buffer-create orb-reference-scrapper--buffer)
           (shell-command
            (format "anystyle -f bib parse \"%s\" -"
                    (orb-reference-scrapper--get :txt))
            orb-reference-scrapper--buffer)
           (bibtex-mode)
           (bibtex-set-dialect 'BibTeX t)
           (orb-reference-scrapper-generate-keys (orb-reference-scrapper--get :bib))
           (message "Generating citation keys...done"))
         (orb-reference-scrapper--insert))
        ((equal (orb-reference-scrapper--get :context) 'parse-bib-interactive)
         (with-current-buffer (find-buffer-visiting (orb-reference-scrapper--get :bib))
           (when (> (cl-random 10) 9)
             (message "Generating citation keys...Pressing the RED button..."))
           (orb-reference-scrapper-generate-keys)
           (message "Generating citation keys...done"))
         (orb-reference-scrapper--insert))
        (t
         (message "Generating citation keys...Pressing the RED button...")
         (orb-reference-scrapper--put :context 'error)
         (orb-reference-scrapper--dispatcher))))

(defun orb-reference-scrapper--insert ()
  "Checkout from orb reference scrapper interactive mode."
  ;; (find-file temp-bib)
  ;; (kill-buffer)
  (set-buffer (orb-reference-scrapper--get :original-buffer))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (insert "\n* References\n")
      (dolist (group orb-reference-scrapper--validated-refs)
        ;; for a yet unclear reason, `orb-reference-scrapper--refs'
        ;; cdrs are not erased in consecutive runs despite setting it
        ;; to nil in multiple places.
        (insert (format "** %s\n" (car group)))
        (dolist (ref (cdr group))
          (insert (format "- %s\n" ref)))
        (insert "\n")
        ;; reset `orb-reference-scrapper--validate-refs' after using
        ;; it otherwise values get appended on consecutive runs even
        ;; when setting the variable to nil with setq. Maybe some
        ;; memory allocation/garbage collection specifics, most
        ;; likely though not enough understanding of global variables.
        (setf (cdr group) nil))))
  (orb-reference-scrapper--cleanup))

(defun orb-reference-scrapper--cleanup ()
  "Clean up."
  (let ((txt (find-buffer-visiting (orb-reference-scrapper--get :txt)))
        (bib (find-buffer-visiting (orb-reference-scrapper--get :bib))))
    (dolist (buf (list txt bib))
      (and buf
           (set-buffer buf)
           (set-buffer-modified-p nil)
           (kill-buffer buf))))
  (and (get-buffer orb-reference-scrapper--buffer)
       (kill-buffer orb-reference-scrapper--buffer))
  (set-window-configuration (orb-reference-scrapper--get :window-conf))
  (setq orb-reference-scrapper--mode-plist '(:temp-dir "orb-reference-scrapper/")))

(defun orb-reference-scrapper--kill ()
  "Kill the interactive orb-reference-scrapper process."
  (interactive)
  (orb-reference-scrapper--put :context 'kill)
  (orb-reference-scrapper--dispatcher))

;; * Main functions

;; entry point for (interactive)
(defun orb-reference-scrapper-scrap-pdf (key)
  "Scrap references from a pdf file associated with citation KEY."
  (interactive)
  (let ((pdf (file-truename (orb-process-file-field key))))
    (orb-reference-scrapper--put :context 'edit-txt
                                 :pdf pdf))
  (orb-reference-scrapper--dispatcher))

;;;###autoload
(defun orb-reference-scrapper-insert (key)
  "Insert scrapped references as Org-mode tree.
KEY is note's citation key."
  (interactive)
  (setq orb-reference-scrapper--refs nil)
  (orb-reference-scrapper--put :original-buffer (current-buffer)
                               :window-conf (current-window-configuration))
  (orb-reference-scrapper-scrap-pdf key))

(provide 'orb-reference-scrapper)
;;; orb-reference-scrapper.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
