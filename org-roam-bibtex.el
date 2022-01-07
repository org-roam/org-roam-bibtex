;;; org-roam-bibtex.el --- Org Roam meets BibTeX -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Mykhailo Shevchuk
;; Copyright © 2020 Leo Vivier

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;      Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: bib, hypermedia, outlines, wp
;; Version: 0.6.1
;; Package-Requires: ((emacs "27.2") (org-roam "2.0.0") (bibtex-completion "2.0.0"))

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
;; Org-roam-bibtex, ORB for short, offers integration of Org-roam with BibTeX
;; Emacs software: Org-ref, Helm/Ivy-bibtex and Citar.  The main task of ORB is
;; to seamlessly expose Org-roam as a note management solution to these
;; packages, shadowing their native facilities for taking bibliographic
;; notes. As its main feature, ORB enables expansion of BibTeX keywords in
;; Org-roam templates.
;;
;; Main usage:
;;
;; Call interactively `org-roam-bibtex-mode' or call (org-roam-bibtex-mode +1)
;; from Lisp.  Enabling `org-roam-bitex-mode' sets appropriate functions for
;; creating and retrieving Org-roam notes from Org-ref, Helm/Ivy-bibtex and
;; Citar.
;;
;; Other commands:
;;
;; - `orb-insert-link':  insert a link or citation to an Org-roam note that has
;;    an associated BibTeX entry
;; - `orb-note-actions': call a dispatcher of useful note actions
;;
;; Soft dependencies: Org-ref, Citar, Helm, Ivy, Hydra, Projectile, Persp-mode,
;; Refuse, Bibkey

;;; Code:
;;
;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'orb-core)


;; ============================================================================
;;; Org-roam-bibtex minor mode
;; ============================================================================

(defvar org-roam-bibtex-mode-map
  (make-sparse-keymap)
  "Keymap for `org-roam-bibtex-mode'.")

;;;###autoload
(define-minor-mode org-roam-bibtex-mode
  "Sets an appropriate function for editing bibliography notes.
Supports Org-ref, Helm-bibtex/Ivy-bibtex, and Citar.

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
  ;; TODO: Revert external variables to their original values rather than to
  ;; their defaults
  (cond (org-roam-bibtex-mode
         (setq orb--external-vars-original-values nil)
         (let ((var-alist
                '((citar-open-note-function . citar)
                  (bibtex-completion-edit-notes-function . bibtex-completion)
                  ;; Only for Org-ref v2
                  (org-ref-notes-function . org-ref))))
           (dolist (el var-alist)
             (let* ((var (car el))
                    (pkg (cdr el))
                    (val (and (require pkg nil t)
                              (boundp var)
                              (symbol-value var))))
               (when val
                 (push (cons var val) orb--external-vars-original-values)
                 (set var (intern (format "orb-%s-edit-note" pkg)))))))
         (add-to-list 'bibtex-completion-find-note-functions #'orb-find-note-file)
         (add-to-list 'bibtex-completion-key-at-point-functions #'orb-get-node-citekey)
         (add-hook 'org-capture-after-finalize-hook #'orb-make-notes-cache)
         (add-hook 'org-roam-capture-new-node-hook #'orb--insert-captured-ref-h)
         (orb-make-notes-cache))
        (t
         (dolist (var-alist orb--external-vars-original-values)
           (set (car var-alist) (cdr var-alist)))
         (setq bibtex-completion-find-note-functions
               (delq #'orb-find-note-file
                     bibtex-completion-find-note-functions))
         (setq bibtex-completion-key-at-point-functions
               (delq #'orb-get-node-citekey
                     bibtex-completion-key-at-point-functions))
         (remove-hook 'org-roam-capture-new-node-hook #'orb--insert-captured-ref-h)
         (remove-hook 'org-capture-after-finalize-hook #'orb-make-notes-cache))))

(provide 'org-roam-bibtex)

;;; org-roam-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
