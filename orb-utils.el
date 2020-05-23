;;; orb-utils.el --- Org Roam BibTeX: Utility macros and functions -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex, org-ref
;; Version: 0.2.3
;; Package-Requires: ((emacs "26.1"))

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
;; This file contains utility macros and helper functions used accross
;; different org-mode-bibtex modules.  This library may be required
;; directly or through orb-core.el.  Definitions in this file should
;; only depend on built-in Emacs libraries.

;;; Code:
;; * Library requires
(require 'orb-compat)

(defvar orb-citekey-format)

;; * Macros


;; * Functions

(defun orb-unformat-citekey (citekey)
  "Remove format from CITEKEY.
Format is `orb-citekey-format'."
  (string-match "\\(.*\\)%s\\(.*\\)" orb-citekey-format)
  (let ((beg (match-end 1))
        (end (+ (length citekey)
                (- (match-beginning 2)
                   (length orb-citekey-format)))))
    (substring citekey beg end)))

(provide 'orb-utils)
;;; orb-utils.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
