;;; orb-anystyle.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: org-mode, roam, convenience, bibtex, helm-bibtex, ivy-bibtex, org-ref
;; Version: 0.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (f "0.20.0"))

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
(require 'dash)
(require 'f)

(require 'orb-macs)

(eval-when-compile
  (require 'subr-x)
  (require 'cl-macs))

;; * Customize definitions

(defvar orb-anystyle-executable "anystyle") ; TODO: make it defcustom
(defvar orb-anystyle-pdfinfo-executable "pdfinfo") ; TODO: make it defcustom
(defvar orb-anystyle-pdftotext-executable "pdftotext") ; TODO: make it defcustom
(defvar orb-anystyle-parser-model nil)
(defvar orb-anystyle-finder-model nil)
(defvar orb-anystyle-default-buffer "*Orb Anystyle Output*") ; TODO: make it defcustom

;; * Main functions

;;;###autoload
(defun orb-anystyle (command &rest args)
  "Run anystyle COMMAND with `shell-command'.
ARGS is a plist with the following recognized keys:

Anystyle CLI options
==========

1) :command          => symbol or string
- valid values: find parse help check license train

2a) :global-options  => string
- apart from stringp type check, no other checks are made!

2b) Alternatively, global options can be passed as plist keys.
- these keys are completely ignored if :global-options is non-nil

:finder-model => string (valid file path)
:parser-model => string (valid file path)
:pdfinfo      => string (valid executable)
:pdftotext    => string (valid executable)
:adapter      => anything
:stdout       => boolean
:help         => boolean
:verbose      => boolean
:version      => boolean
:overwrite    => boolean
:format       => string, symbol or list of unquoted symbols
- must be one or more formats accepted by anystyle commands:
  check => ttx xml
  parse => bib csl json ref txt xml
  find  => bib csl json ref txt ttx xml
- string must be space- or comma-separated, additional spaces are
  ignored

3a) :command-options => string
- apart from stringp type check, no other checks are made!
- ignored for commands other than find or parse

3b) Alternatively, command options can be passed as plist keys:

:crop         => integer or cons cell of integers
:layout       => boolean
:solo         => boolean

- help -c flag is not supported
4) :input string (file path)

5) :output string (file path)

`shell-command'-related keys
==========

1) :buffer buffer-or-name

- `shell-command''s OUTPUT-BUFFER
- can be a cons cell (OUTPUT-BUFFER . ERROR-BUFFER)
- when nil, defaults to (`orb-anystyle-default-buffer' . nil)

anystyle CLI command synopsis:
anystyle [global options] command [command options] [arguments...].

Homepage: https://anystyle.io
Github: https://github.com/inukshuk/anystyle-cli
Courtesy of its authors."
  (declare (indent 1))
  (-let* (((&plist :exec exec
                   :global-options global-options
                   :finder-model fmodel
                   :parser-model pmodel
                   :adapter adapter
                   :format format
                   :pdfinfo pdfinfo
                   :pdftotext pdftotext
                   :help help
                   :stdout stdout
                   :verbose verbose
                   :version version
                   :overwrite overwrite
                   :command-options command-options
                   :crop crop
                   :layout layout
                   :solo solo
                   :input input
                   :output output
                   :buffer buffer) args)
          (commands '(list find parse check train help license))
          (exec (executable-find
                 (or exec orb-anystyle-executable)))
          (buf (if (consp buffer)
                   buffer
                 (list (or buffer orb-anystyle-default-buffer))))
          ;; '(a b c) => "a,b,c"
          (collapse (lambda (str)
                         (--reduce-from
                          (format "%s,%s" acc it)
                          (car str) (cdr str))))
          ;; debug
          ;; (shell-run (lambda (str)
          ;;              (message "command: %s \nbuffers: %s and %s" str (car buf) (cdr buf))))
          (shell-run (lambda (str)
                      (shell-command str (car buf) (cdr buf))))
          anystyle)
    ;; executable is a must
    (unless exec
      (user-error "Anystyle executable not found!  \
Install anystyle-cli before running Orb PDF Scrapper"))
    ;; we must process :version and :help before checking command
    ;; but still ignore them if :global-options
    (unless global-options
      (cond
       ;; help flag takes priority
       (help
        (setq global-options " --help"
              command-options ""
              input nil
              output nil))
       ;; anystyle ignores everything with --version flag except the
       ;; --help flag, which we've just resolved above
       (version
        (setq global-options "--version"
              command nil
              command-options ""
              input nil
              output nil))))
    ;;
    ;; command is a must
    (unless version
      (unless command
        (user-error "Anystyle command required: \
find, parse, check, train, help or license"))
      ;; check if command is a valid command
      (when (stringp command)
        (setq command (intern command)))
      (unless (memq command commands)
        (user-error "Invalid command %s.  Valid commands are \
find, parse, check, train, help and license" command)))
    ;;
    ;; process help and license before global commands
    (cl-case command
      ('help
       (when (stringp input)
         (setq input (intern input)))
       (unless (or (and global-options
                        (string= global-options " --help"))
                   (memq input commands))
         (user-error "Invalid input %s.  Valid input for 'anystyle help': \
find, parse, check, train, help or license" input)))
      ('license
       (setq input nil
             output nil
             global-options ""
             command-options "")))

    ;; * Global options
    ;;
    (if global-options
        ;; If :global-options value is non-nil, only stringp sanity
        ;; check is performed
          (unless (stringp global-options)
            (user-error "String is expected for :global-options"))
      ;; Otherwise, construct global-options from plist keys.
      ;;
      ;; If an option does nothing for the command, ignore it silently
      ;;
      ;; find, parse, and check:
      ;;
      ;; 1) format option should be one of accepted types if present
      ;; 2) finder and parser models should be valid file paths
      ;; if present
      (when (memq command '(find parse))
        ;; format
        (when format
          (when (stringp format)
            (setq format
                  (-map #'intern
                        (split-string (string-trim format)
                                      "[, ]" t " "))))
          (unless (listp format)
            (setq format (list format)))
          (let ((accepted-formats
                 (cl-case command
                   ('find '(bib csl json ref txt ttx xml))
                   ('parse '(bib csl json ref txt xml)))))
            (when (--none? (memq it accepted-formats) format)
              (user-error
               "Invalid format(s) %s.  Valid formats for command %s: %s"
               (funcall collapse format)
               command
               (funcall collapse accepted-formats)))
            ;; convert format to a comma-separated string and append
            ;; it to global options
            (setq global-options
                  (orb-format "%s" global-options
                              " -f %s" (funcall collapse format)))))
        ;; finder and parser models
        (when (and fmodel (not (f-exists? fmodel)))
          (user-error "Finder model file not found: %s" fmodel))
        (when (and pmodel (not (f-exists? pmodel)))
          (user-error "Parser model file not found: %s" pmodel))
        (setq global-options (orb-format "%s" global-options
                                         " -F %s" fmodel
                                         " -P %s" pmodel)))
      ;;
      ;; find and train:
      ;;
      ;; 1) pdfinfo and pdftotext must be present in the system
      (when (memq command '(find train))
        (unless (executable-find
                 (or pdfinfo orb-anystyle-pdfinfo-executable))
          (user-error "Could not find pdfinfo executable: %s"
                      (or pdfinfo
                          orb-anystyle-pdfinfo-executable)))
        (unless (executable-find
                 (or pdftotext orb-anystyle-pdftotext-executable))
          (user-error "Could not find pdftotext executable: %s"
                      (or pdftotext
                          orb-anystyle-pdftotext-executable)))
        (setq global-options
              (orb-format "%s" global-options
                          " --pdfinfo=\"%s\"" pdfinfo
                          " --pdftotext=\"%s\"" pdftotext)))
      ;; find, train, parse and check:
      ;;
      ;; 1) require input, which should be a valid path
      ;; 2) something called ruby adapter, probably a right place here
      ;; 3) --verbose, --stdout, --overwrite if non-nil
      (when (memq command '(find train parse check))
        (unless input
          (user-error "Input required for command %s" command))
        (unless (and (stringp input) (f-exists? input))
          (user-error "Invalid input file or directory %s" input))
        (setq global-options
              (orb-format
               "%s" global-options
               " --verbose" (cons verbose " --no-verbose")
               " --stdout" (cons stdout " --no-stdout")
               " --adapter=\"%s\"" adapter
               " --overwrite" (cons overwrite " --no-overwrite")))))
    ;;
    ;; * Command options
    ;;
    ;; find:
    ;;
    ;; N.B. Help command accepts a command option -c but it's totally
    ;; irrelevant for us:
    ;;
    ;; [COMMAND OPTIONS]
    ;; -c - List commands one per line, to assist with shell completion
    ;;
    ;; so we do not implement it as a plist key
    ;; it can, however, be called with :global-options
    (when (eq command 'find)
      (if command-options
            (unless (stringp command-options)
              (user-error ":command-options value type must be string"))
        ;; :crop's value should be a number or string
        ;; nil is equivalent to 0
        (when crop
          (unless (consp crop)
            (setq crop (list crop)))
          (let ((x (or (car crop) 0))
                (y (or (cdr crop) 0)))
            (unless (and (or (numberp x)
                             (stringp x))
                         (or (numberp y)
                             (stringp y)))
              (user-error "Invalid value %s,%y.  Number or string expected"
                          x y))
            (setq crop (format "%s,%s" x y))))
        ;; parse only accepts --[no]-layout, so we ignore the rest
        (setq command-options (orb-format
                               " --crop=%s" crop
                               " --layout" (cons layout " --no-layout")
                               " --solo" (cons solo " --no-solo"))))
      ;; append command options to command
      (setq command (orb-format "%s" command
                                "%s" command-options)))

    (setq output (if (eq command 'check) nil output))
    ;;
    ;; Run the program
    ;;
    (setq anystyle (orb-format "%s" exec
                               "%s" global-options
                               " %s" command
                               " \"%s\"" input
                               " \"%s\"" output))
    (funcall shell-run anystyle)))

(provide 'orb-anystyle)
;;; orb-anystyle.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
