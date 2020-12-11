;;; org-roam-bibtex-compat.el --- Org Roam BibTeX: obsolete definitions -*- lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
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
;; Obsolete definitions live here.  For a while.

;;; Code:

;; * org-roam-bibtex.el
(define-obsolete-variable-alias
  'orb-insert-frontend 'orb-insert-interface "0.5")
(define-obsolete-variable-alias
  'orb-note-actions-frontend 'orb-note-actions-interface "0.5")

(define-obsolete-variable-alias
  'orb-pdf-scrapper-refsection-headings 'orb-pdf-scrapper-grouped-export "0.5")
(define-obsolete-variable-alias
  'orb-pdf-scrapper-export-fields 'orb-pdf-scrapper-table-export-fields "0.5")

(provide 'orb-compat)
;;; orb-compat.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
