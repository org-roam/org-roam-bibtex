;;; orb-anystyle.el --- Connector between Org-roam, BibTeX-completion, and Org-ref -*- coding: utf-8; lexical-binding: t -*-

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
(require 'orb-macs)
(require 'orb-pdf-scrapper)             ;for now

(defvar orb-pdf-scrapper-anystyle-exec "anystyle") ; TODO: make it defcustom

(defun orb-pdf-scrapper-anystyle (buffer-or-name &rest args)
  "Run anystyle-cli with `shell-command'.
BUFFER-OR-NAME is buffer or buffer name, which will be passed to
`shell-command' as OUTPUT-BUFFER.  It can also be a cons cell
\(OUTPUT-BUFFER . ERROR-BUFFER).  If BUFFER-OR-NAME is nil,
`orb-pdf-scrapper--buffer' will be used.

ARGS is a plist with following recognized keys.

1) :command symbol or string
  - valid values are find,parse,help,check,license,train

2a) :global-options string
  - no checks are made!

2b) Alternatively, the following global options are supported as
plist keys:

:finder-model string (file path) :parser-model string (file path)
:stdout boolean :format symbol or list of unquoted symbols - if
:global-options is present, these keywords will be ignored

3a) :command-options string
  - no checks made
  - ignored when :command is anything else find

3b) Alternatively, command options can be passed as plist keys:

:crop integer or cons cell of integers
:layout boolean
:solo boolean

4) :input string (file path)

5) :output string (file path)

Homepage: https://anystyle.io
Github: https://github.com/inukshuk/anystyle-cli
Courtesy of its authors."

  ;; command synopsis:
  ;; anystyle [global options] command [command options] [arguments...]
  ;;
  ;; * command
  ;; :command - symbol or string, one of:
  ;; check find help license parse train
  ;;
  ;; * global options
  ;; :global-options - a string as per anystyle command line options,
  ;; no checks are made!
  ;;
  ;; -F, --finder-model=file - set the finder model file (default: none)
  ;; -P, --parser-model=file - set the parser model file (default: none)
  ;; --adapter=name          - set the dictionary adapter (default: ruby)
  ;; -f, --format=name       - set the output format (default: ["json"])
  ;; --pdfinfo=path          - set the path for pdfinfo (default: none)
  ;; --pdftotext=path        - set the path for pdftotext (default: none)
  ;; --help                  - show this message
  ;; --[no-]stdout           - print results directly to stdout
  ;; --[no-]verbose          - print status messages to stderr
  ;; --version               - display the program version
  ;; -w, --[no-]overwrite    - allow overwriting existing files
  ;;
  ;; Alternatively, the following global options are supported as
  ;; individual keys:
  ;;
  ;; :finder-model "/path/to/finder/model"
  ;; :parser-model "/path/to/parser/model"
  ;; :adapter symbol or string
  ;; :format 'symbol or list of several formats ('bib 'csl 'json 'ref 'txt 'ttx 'xml)
  ;; :pdfinfo "/path/to/pdfinfo/exec"
  ;; :pdftotext "/path/to/pdftotext/exec"
  ;; :help boolean
  ;; :stdout boolean
  ;; :verbose boolean
  ;; :version boolean
  ;; :overwrite boolean
  ;;
  ;; find supports all the above formats
  ;; parse supports ('bib 'csl 'json 'ref 'txt 'xml)
  ;; check supports ('ttx 'xml)
  ;;
  ;; if :global-options is present, these keywords will be ignored
  ;;
  ;; * command options
  ;;
  ;; :command-options only relevant for :command 'find, will be
  ;; ignored otherwise. same as with :global-options, no checks are
  ;; made.
  ;;
  ;; -c, --crop=pt - set cropping boundary for text extraction (default: none)
  ;; --[no-]layout - use layout mode for pdf text extraction (default: enabled)
  ;; --[no-]solo   - include references outside of reference sections
  ;;
  ;; Alternatively, they can be passed as individual keys:
  ;;
  ;; :crop integer or (integer . integer)
  ;; :layout boolean
  ;; :solo boolean
  ;;
  ;; * input and output
  ;;
  ;; todo: should multiple documents be supported here?
  ;; :input - "/path/to/input/file"
  ;;
  ;; :output - "/path/to/output/directory" - find and parse
  ;;           "/path/to/output/file" - train

  ;;
  ;; "anystyle -f bib parse \"%s\" -"

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
                   :command command
                   :command-options command-options
                   :crop crop
                   :layout layout
                   :solo solo
                   :input input
                   :output output) args)
          (commands '(list find parse check train help license))
          (formats '(bib csl json ref txt ttx xml))
          (exec (executable-find
                 (or exec orb-pdf-scrapper-anystyle-exec "anystyle")))
          (buf (or (if (consp buffer-or-name)
                       buffer-or-name
                     (list buffer-or-name))
                   (list orb-pdf-scrapper--buffer)))
          (shell-run (lambda (str)
                       (message "command: %s \nbuffers: %s and %s" str (car buf) (cdr buf))))
          (make-format (lambda (str)
                         (--reduce-from
                          (format "%s,%s" acc it)
                          (car str) (cdr str))))
          ;; (shell-run (lambda (str)
          ;;             (shell-command str (car buf) (cdr buf))))
          options acceptable-formats anystyle)
    ;; executable is a must
    (cond
     ((not exec)
      (user-error "Anystyle executable not found!  \
Install anystyle-cli before running Orb PDF Scrapper"))
     ;; :command is a must
     ((not command)
      (user-error "Anystyle command required: \
find, parse, check, train, help or license"))
     ;; requested models, a must when requested
     ((and fmodel (not (f-exists? fmodel)))
      (user-error "Finder model file not found: %s" fmodel))
     ((and pmodel (not (f-exists? pmodel)))
      (user-error "Parser model file not found: %s" pmodel))
     (t
      ;; command may be a string, convert it into symbol
      (when (stringp command)
        (setq command (intern command)))
      ;; check if command is valid
      (unless (memq command commands)
        (user-error "Invalid command %s.  Valid commands are \
find, parse, check, train, help and license" command))
      ;; Set the global options
      (if global-options
          ;; If :global-options value is provided only stringp sanity
          ;; check is performed
          (progn
            (unless (stringp global-options)
              (user-error ":global-options value type must be string"))
            (setq global-options (format " %s" (s-trim global-options))))
        ;; Otherwise, construct global-options from plist keys
        ;; First, options relevant for all commands
        (setq global-options (orb-format
                              " --help" help
                              " --version" version
                              " --verbose" (cons verbose " --no-verbose")
                              " --stdout" (cons stdout " --no-stdout")))
        ;;
        ;; Then make checks for specific commands
        ;; If an option is irrelevant for the command,
        ;; ignore it silently
        ;;
        ;; find, parse, and check require format
        (when (memq command '(find parse check))
          (when (and format (stringp format))
            (setq format (-map #'intern
                               (s-split "," (s-trim format)))))
          (let ((accepted-formats
                 (case command
                   ('find '(bib csl json ref txt ttx xml))
                   ('parse '(bib csl json ref txt xml))
                   ('check '(ttx xml)))))
            ;; TODO: compare list agains list
            (unless (--none? (memq it acceptable-formats)
                             format)
              (user-error
               "Invalid format(s) %s.  Valid formats for %s: %s"
               (funcall make-format format)
               command
               (funcall make-format acceptable-formats)))
            ;; convert format to a comma-separated string
            ;; and attach it to global options
            (setq global-options
                  (orb-format "%s" global-options
                              " -f %s" (funcall make-format format)))))
        ;; commands find and train use pdfinfo and pdftotext, so
        ;; these must be present in the system
        (when (memq command '(find train))
          (unless (executable-find (or pdfinfo "pdfinfo"))
            (user-error "Could not find pdfinfo executable"))
          (unless (executable-find (or pdftotext "pdftotext"))
            (user-error "Could not find pdftotext executable"))
          (setq global-options
                (orb-format "%s" global-options
                            " --pdfinfo=\"%s\"" pdfinfo
                            " --pdftotext=\"%s\"" pdftotext)))
        ;; find, train, parse and check require input, which should be a valid path
        (when (memq command '(find train parse check))
          (unless input
            (user-error "Input required for command %s" command))
          (unless (f-exists? input)
            (user-error "Invalid input file or directory %s" input))
          (setq global-options
                (orb-format
                 "%s" global-options
                 " --adapter=\"%s\"" adapter
                 " --overwrite" (cons overwrite " --no-overwrite")))))
      ;; find and parse also accept command options
      ;; well, actually help also does but it's totally irrelevant for us:
      ;; -c - List commands one per line, to assist with shell completion
      (when (memq command '(find parse))

        )
      (if command-options)
      ;; build command specific anystyle cli-strings
      ;; and perform some additional checks
      (cl-case command
        ('license
         (setq input nil
               output nil))
        ('help
         ;; help's input should be other command's name
         (when (stringp input)
           (setq input (intern input)))
         (unless (memq input commands)
           (user-error "Invalid input.  Valid input for 'anystyle help': \
find, parse, check, train, help or license"))
         (setq output nil))
        ('check
         (setq output nil))
        ('find
         ;; find also accepts command options
         (unless command-options
           (when crop
             (setq crop (if (consp crop)
                            (format "%s,%s" (car crop) (cdr crop))
                          (format "%s" crop))))
           (setq command-options (orb-format
                                  " --crop=%s" crop
                                  " --layout" (cons layout " --no-layout")
                                  " --solo" (cons solo " --no-solo"))))
         (setq command (orb-format "%s" command
                                   " %s" command-options))))
      ;; Run the program
      (setq anystyle (orb-format "%s" exec
                                 "%s" global-options
                                 " %s" command
                                 " \"%s\"" input
                                 " \"%s\"" output))
      (funcall shell-run anystyle)))))

(provide 'orb-anystyle)
;;; orb-anystyle.el ends here
;; Local Variables:
;; fill-column: 70
;; End:
