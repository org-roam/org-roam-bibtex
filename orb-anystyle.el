;;; orb-anystyle.el --- Orb Roam BibTeX: Elisp interface to anystyle  -*- coding: utf-8; lexical-binding: t -*-

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

(eval-when-compile
  (require 'subr-x)
  (require 'cl-macs))

;; * Customize definitions

(defvar orb-anystyle-executable "anystyle") ; TODO: make it defcustom
(defvar orb-anystyle-pdfinfo-executable nil) ; TODO: make it defcustom
(defvar orb-anystyle-pdftotext-executable nil) ; TODO: make it defcustom
(defvar orb-anystyle-parser-model nil)
(defvar orb-anystyle-finder-model nil)
(defvar orb-anystyle-find-crop nil)
(defvar orb-anystyle-find-solo nil)
(defvar orb-anystyle-find-layout nil)
(defvar orb-anystyle-default-buffer "*Orb Anystyle Output*") ; TODO: make it defcustom
;; --crop is currently broken

;; * Main functions

;;;###autoload
(cl-defun orb-anystyle (command
                        &key (exec orb-anystyle-executable)
                        verbose help version adapter
                        ((:finder-model fmodel) orb-anystyle-finder-model)
                        ((:parser-model pmodel) orb-anystyle-parser-model)
                        (pdfinfo orb-anystyle-pdfinfo-executable)
                        (pdftotext orb-anystyle-pdftotext-executable)
                        format stdout overwrite
                        (crop orb-anystyle-find-crop)
                        (solo orb-anystyle-find-solo)
                        (layout orb-anystyle-find-layout)
                        input output
                        (buffer orb-anystyle-default-buffer))
  "Run anystyle COMMAND with `shell-command'.
ARGS is a plist with the following recognized keys:

Anystyle CLI options
==========
1) :exec      => string (valid executable)
- default value can be set int `orb-anystyle-executable'

2) :command   => symbol or string
- valid values: find parse help check license train

3) Global options can be passed with the following keys.

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

- must be one or more output formats accepted by anystyle commands:
  parse => bib csl json ref txt xml
  find  => bib csl json ref txt ttx xml
- string must be space- or comma-separated, additional spaces are
  ignored

Default values for some of these options can be set globally via
the following variables: `orb-anystyle-finder-model',
`orb-anystyle-parser-model', `orb-anystyle-pdfinfo-executable',
`orb-anystyle-pdftotext-executable'.

4) Command options can be passed with the following keys:

:crop         => integer or cons cell of integers
:layout       => boolean
:solo         => boolean

- ignored for commands other than find or parse
- help -c flag is not supported

Default values for these options can be set globally via the
following variables: `orb-anystyle-find-crop',
`orb-anystyle-find-layout', `orb-anystyle-find-solo'.

5) :input string (file path)

6) :output string (file path)

`shell-command'-related options
==========

7) :buffer buffer-or-name

- `shell-command''s OUTPUT-BUFFER
- can be a cons cell (OUTPUT-BUFFER . ERROR-BUFFER)
- when nil, defaults to `orb-anystyle-default-buffer'

anystyle CLI command synopsis:
anystyle [global options] command [command options] [arguments...].

Homepage: https://anystyle.io
Github: https://github.com/inukshuk/anystyle-cli
Courtesy of its authors."
  (declare (indent 1))
  (let* ((commands '(list find parse check train help license))
         (exec (executable-find exec))
         (buf (if (consp buffer) buffer (list buffer)))
         ;; '(a b c) => "a,b,c"
         (to-string (lambda (str)
                      (--reduce-from
                       (format "%s,%s" acc it)
                       (car str) (cdr str))))
         ;; debug
         ;; (shell-run (lambda (str)
         ;;              (message "command: %s \nbuffers: %s and %s" str (car buf) (cdr buf))))
         (shell-run (lambda (str)
                      (shell-command str (car buf) (cdr buf))))
         global-options command-options anystyle)
    ;; executable is a must
    (unless exec
      (user-error "Anystyle executable not found!  \
Install anystyle-cli before running Orb PDF Scrapper"))
    ;; we process :version and :help before checking command
    ;; since with this global flag command is not required
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
            output nil))
     ;; otherwise command is a must
     ((not command)
      (user-error "Anystyle command required: \
find, parse, check, train, help or license")))
    (when (stringp command)
      (setq command (intern command)))
    ;; command must be a valid command
    (unless (memq command commands)
      (user-error "Invalid command %s.  Valid commands are \
find, parse, check, train, help and license" command))
    ;;
    ;; command specific arguments
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
             command-options ""))
      ('check
       (setq output nil))
      ('find
       ;; find command options
       ;;
       ;; N.B. Help command accepts a command option -c but it's totally
       ;; irrelevant for us:
       ;;
       ;; [COMMAND OPTIONS]
       ;; -c - List commands one per line, to assist with shell completion
       ;;
       ;; so we do not implement it
       ;; :crop's value should be a number or string
       ;; nil is equivalent to 0
       ;; if no value was explicitely supplied, use
       ;; the default from user option
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
       ;; append command options to command
       (setq command-options
             (orb--format " --crop=%s" crop
                          " --layout" (cons layout " --no-layout")
                          " --solo" (cons solo " --no-solo")))))
    ;; Arguments relevant for more than one command
    ;;
    ;; find, parse:
    ;; format option should be one of accepted types if present
    (when (and (memq command '(find parse))
               format)
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
           (funcall to-string format)
           command
           (funcall to-string accepted-formats)))
        ;; convert format to a comma-separated string and append
        ;; it to global options
        (setq global-options
              (orb--format "%s" global-options
                           " -f %s" (funcall to-string format)))))
    ;; find and train:
    ;; 1) pdfinfo and pdftotext must be present in the system
    (when (memq command '(find train))
      (when (and pdfinfo (not (executable-find pdfinfo)))
        (user-error "Could not find pdfinfo executable: %s" pdfinfo))
      (when (and pdftotext (not (executable-find pdftotext)))
        (user-error "Could not find pdftotext executable: %s" pdftotext))
      (setq global-options
            (orb--format "%s" global-options
                         " --pdfinfo=\"%s\"" pdfinfo
                         " --pdftotext=\"%s\"" pdftotext)))
    ;; find, parse, check accept
    ;; finder and parser models
    (when (memq command '(find parse check))
      (when (and fmodel (not (f-exists? fmodel)))
        (user-error "Finder model file not found: %s" fmodel))
      (when (and pmodel (not (f-exists? pmodel)))
        (user-error "Parser model file not found: %s" pmodel))
      (setq global-options (orb--format "%s" global-options
                                        " -F \"%s\"" fmodel
                                        " -P \"%s\"" pmodel)))
    ;; find, train, parse and check:
    ;; 1) require input, which should be a valid path
    ;; 2) something called ruby adapter, probably a right place here
    ;; 3) --verbose, --stdout, --overwrite if non-nil
    (when (memq command '(find train parse check))
      (unless input
        (user-error "Input required for command %s" command))
      (unless (and (stringp input) (f-exists? input))
        (user-error "Invalid input file or directory %s" input))
      (setq global-options
            (orb--format
             "%s" global-options
             " --verbose" (cons verbose " --no-verbose")
             ;; this flag does nothing for check
             " --stdout" (cons stdout " --no-stdout")
             " --adapter=\"%s\"" adapter
             " --overwrite" (cons overwrite " --no-overwrite"))))
    ;; Set arguments and run the program
    ;;
    (setq anystyle (orb--format "%s" exec
                                "%s" global-options
                                " %s" command
                                "%s" command-options
                                " \"%s\"" input
                                " \"%s\"" output))
    (funcall shell-run anystyle)))

(provide 'orb-anystyle)
;;; orb-anystyle.el ends here
;; Local Variables:
;; fill-column: 79
;; End:
