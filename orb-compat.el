;;; org-roam-bibtex-compat.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
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

;;; Commentary:
;;
;; Obsolete definitions live here.  For a while.

;;; Code:

;; * org-roam-bibtex.el
(define-obsolete-variable-alias 'org-roam-bibtex-preformat-templates 'orb-preformat-templates "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-templates 'orb-templates "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-include-citekey-in-titles 'orb-include-citekey-in-titles "0.2" "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-preformat-keywords 'orb-preformat-keywords "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-citekey-format 'orb-citekey-format "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-persp-project 'orb-persp-project "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-switch-persp 'orb-switch-persp "0.2")

(define-obsolete-function-alias 'org-roam-bibtex-notes-fn 'orb-notes-fn "0.2")
(define-obsolete-function-alias 'org-roam-bibtex-edit-notes-ad 'orb-edit-notes-ad "0.2")
(define-obsolete-function-alias 'org-roam-bibtex-process-file-field 'orb-process-file-field "0.2")
(define-obsolete-function-alias 'org-roam-bibtex-edit-notes 'orb-edit-notes "0.2")
(define-obsolete-function-alias 'org-roam-bibtex-find-non-ref-file 'orb-find-non-ref-file "0.2")
(define-obsolete-function-alias 'org-roam-bibtex-insert-non-ref 'orb-insert-non-ref "0.2")

;; * org-roam-bibtex-note-actions.el

(define-obsolete-variable-alias 'org-roam-bibtex-note-actions-frontend 'orb-note-actions-frontend "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-note-actions-extra 'orb-note-actions-extra "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-note-actions-user 'orb-note-actions-user "0.2")
(define-obsolete-variable-alias 'org-roam-bibtex-note-actions-default 'orb-note-actions-default "0.2")

(define-obsolete-function-alias 'org-roam-bibtex-note-actions 'orb-note-actions "0.2")

(provide 'orb-compat)
;;; orb-compat.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
