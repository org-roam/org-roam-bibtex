[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
[![GitHub Release](https://img.shields.io/github/release/org-roam/org-roam-bibtex.svg)](https://github.com/org-roam/org-roam-bibtex/releases/)
[![MELPA](https://melpa.org/packages/org-roam-bibtex-badge.svg)](https://melpa.org/#/org-roam-bibtex)

org-roam-bibtex
============

<img alt="ORB logo" src="doc/logo-r500.png" width="150">

Description
---------------

`org-roam-bibtex` is a library which offers a tighter integration
between [`org-roam`](https://github.com/jethrokuan/org-roam),
[`helm-bibtex`](https://github.com/tmalsburg/helm-bibtex), and
[`org-ref`](https://github.com/jkitchin/org-ref).

It allows users to access their bibliographical notes in
`org-roam-directory` via `helm-bibtex`, `ivy-bibtex`, or by opening
`org-ref`’s `cite:` links and running `3. Add notes`.  If the note
does not exist, it is created.


Quick Demonstration 🎬
---------------
Click the picture to go to the video:

<a href="https://www.youtube.com/watch?v=Wy9WvF5gWYg">
<img alt="Quick presentation (video)" src="doc/demo-thumbnail.jpg" width="700">
</a>

#### Another GIF demonstration

<img alt="Demo (gif)" src="doc/demo.gif" width="700">

Articles
---------------
Here is a selection of articles that you may find interesting.

#### Introduction to Org-roam
- [How To Take Smart Notes With Org-mode](https://blog.jethro.dev/posts/how_to_take_smart_notes_org/) (by [@jethrokuan](https://github.com/jethrokuan))

#### Workflow
- [An Orgmode Note Workflow](https://rgoswami.me/posts/org-note-workflow/) (by [@HaoZeke](https://github.com/HaoZeke))


A word of warning 🚧
---------------

`org-roam-bibtex` is in **Alpha**.

This means that a lot of things may change in the future
(e.g. renaming variables, rewriting functions).  As a result, the
package will be unstable for a while.  This will change when we
release v1.0, but for now, you will have to be on the lookout for a
few things:
1. If you encounter a problem with the package, start by making sure
   that you have the latest
   version. (cf. [Installation](#installation))
2. If there is a problem with your configuration, most notably
   variables which do not exist anymore or functions which are not
   called with the right number of arguments, you will need to check
   this page to see what has changed.
3. If neither 1. nor 2. resolved your problem, please open an
   **issue**.  (This is what an Alpha version is for!)

##### WARNING! 

- **In version 0.2, the `org-roam-bibtex` namespace was renamed to
  `orb`, except for `org-roam-bibtex-mode` and other definitions that
  match the `*-mode*` pattern. The existing functions and variables
  bearing the old prefix will be supported for a while but support
  will be dropped eventually. All new functions and variables will
  have the new prefix except those matching the `*-mode*` pattern,
  which will retain the `org-roam-bibtex` namespace.**

Installation
---------------

### Via MELPA

The package is on [MELPA](https://github.com/melpa/melpa).  You can
install `org-roam-bibtex` using `package.el`:

```
M-x package-install RET org-roam-bibtex RET
```

You can also install it with `M-x package-list-packages`.

If you do not know how MELPA works, check their
[Usage](https://github.com/melpa/melpa#usage) section.

### Via cloning

You can also clone the repository somewhere in your `load-path`.  If
you would like to assist with development, this is the way to go.

To do that:
1. Create a directory where you’d like to clone the repository,
   e.g. `mkdir ~/projects`.
2. `cd ~/projects`
3. `git clone https://github.com/org-roam/org-roam-bibtex.git`

You now have the repository cloned in `~/projects/org-roam-bibtex/`.
See [Quick-start](#quick-start-) to learn how to add it to your
`load-path` and to get started with the package.

You can also copy
[`org-roam-bibtex.el`](https://github.com/org-roam/org-roam-bibtex/blob/improve-readme/org-roam-bibtex.el)
somewhere where `load-path` can access it, but you’d have to update
the file manually.

### Spacemacs

Assuming `org-roam` is installed via a private layer as described
[here](https://org-roam.readthedocs.io/en/master/installation/#spacemacs),
add `org-roam-bibtex` to `org-roam-packages:

``` el
(defconst org-roam-packages
  '(org-roam org-roam-bibtex))

```

and this after `org-roam/init-org-roam`:

``` el
(defun org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map
          (("C-c n a" . orb-note-actions)))))
```

### Doom Emacs

Put this in `$DOOMDIR/packages.el`:

```el
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag
(unpin! org-roam company-org-roam)

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)
```

Then run `bin/doom sync`. The package can be configured with
`use-package` (or alternatively `Doom`'s `use-package!`) as described
below.

Quick-start 🚀
---------------

You can get `org-roam-bibtex` up and running by pasting the following
sexps in your
[init-file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html):

### With `use-package`

```el
;; If you installed via MELPA
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))

;; If you cloned the repository
(use-package org-roam-bibtex
  :after org-roam
  :load-path "~/projects/org-roam-bibtex/" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))
```

### Without `use-package`

```el
;; If you installed via MELPA
(require 'org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)

;; If you cloned the repository
(add-to-list 'load-path "~/projects/org-roam-bibtex/") ;Modify with your own path
(require 'org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)
```

Usage
---------------

You can now access your bibliographical notes in your
`org-roam-directory` via `helm-bibtex`/`ivy-bibtex` or by opening
`org-ref` links.  This is done in the background by modifying the
`Edit notes` actions used by these packages to use `orb-edit-notes`
instead of their defaults.

To revert those actions to their defaults, disable
`org-roam-bibtex-mode`.

### Commands

#### `orb-find-non-ref-file`

Similar to `org-roam-find-file`, but it excludes your bibliographical
notes from the completion-candidates.  This is useful if you have a
lot of them and do not want to clutter up your other notes.

#### `orb-insert-non-ref`

Similar to `org-roam-insert`, but it excludes your bibliographical
notes from the completion-list.

#### `orb-note-actions`

Type `M-x orb-note-actions` to easily access additional commands useful
in note's context.  These commands are run with the note's BibTeX key
as an argument. The key is taken from the `#+ROAM_KEY:` file property.
See section [`Orb Note Actions`](#orb-note-actions-section) for
details.

Configuration
---------------

### Org Roam BibTeX - BibTeX aware capture template expansion

#### `orb-templates`

This variable specifies the templates to use when creating a new
bibliographical note.  It builds on the syntax of
`org-roam-capture-templates` (which, in turn, is powered by
`org-capture-templates`) by allowing you to expand predefined
variables with values of BibTeX fields.

See `orb-edit-notes` and
[`orb-preformat-keywords`](#orb-preformat-keywords) for details.  (You
can access the docstrings of any loaded function/variable from within
Emacs with `C-h f` for functions/commands, and `C-h v` for variables.)

Here’s the default value of `orb-templates`:
```el
(("r" "ref" plain (function org-roam-capture--get-point) ""
      :file-name "${citekey}"
      :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
      :unnarrowed t))
```

You can modify it with `setq`.  For instance, if you want to add the
cite-key in the title of the notes, you can modify the code like this
(pay attention to the line with `:head`):

```el
(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
         :unnarrowed t)))
```

If you have more than one template in `orb-templates`, you will be
prompted for the key of the template you want to use (`r` in the
example above).  If you only have one template, it will use this one
without prompting you.

#### `orb-preformat-keywords`

The template prompt wildcards for preformatting.  Only relevant when
`orb-preformat-templates` is set to `t` (default).  This can be a
string, a list of strings or a cons-cell alist, where each element is
`(STRING . STRING)`.

Use only alphanumerical characters, dash and underscore. See
`orb-edit-notes` for implementation details.

1. If the value is a string, a single keyword, it is treated as a
   BibTeX field name, such as `=key=`. In the following example all
   the prompts with the `=key=` keyword will be preformatted, as well
   as the corresponding match group `%\1`.

```el
(setq orb-preformat-keywords "=key=")
(setq org-roam-capture-templates
      ’(("r" "reference" plain (function org-roam-capture--get-point)
         "#+ROAM_KEY: %^{=key=}%? fullcite: %\1"
         :file-name "references/${=key=}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))
```

2. If the value is a list of strings they are also treated as BibTeX
   field names. The respective prompts will be preformatted.

```el
(setq orb-preformat-keywords ’("=key=" "title"))
(setq org-roam-capture-templates
      ’(("r" "reference" plain (function org-roam-capture--get-point)
         "#+ROAM_KEY: %^{=key=}%? fullcite: %\1"
         :file-name "references/${=key=}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))
```

3. If the value is a list of cons cells, then the car of the cons cell
   is treated as a prompt keyword and the cdr as a BibTeX field-name,
   and the latter will be used to retrieve the relevant value from the
   BibTeX entry. If cdr is omitted, then the car is treated as the
   field-name.

```el
(setq orb-preformat-keywords
      '(("citekey" . "=key=")
        ("type" . "=type=")
       "title"))
(setq org-roam-capture-templates
      '(("r" "reference" plain (function org-roam-capture--get-point)
         "#+ROAM_KEY: %^{citekey}%? fullcite: %\1
          #+ROAM_TAGS: %^{type}
          This %\2 deals with ..."
         :file-name "references/%<%Y-%m-%d-%H%M%S>_${title}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))
```

By default, `org-preformat-keywords` is configured to expand the
following BibTeX fields: "citekey", "date", "type", "pdf?", "note?",
"author", "editor", "author-abbrev", "editor-abbrev",
"author-or-editor-abbrev".

Consult the [`helm-bibtex`](https://github.com/tmalsburg/helm-bibtex)
package for additional information about BibTeX field names.

#### Handling long templates

Long templates can be placed in a separate file, with template
expansion of BibTeX fields working as usual:

```el
(setq org-roam-capture-templates
      '(("r" "reference" plain (function org-roam-capture--get-point)
         (file "/path/to/template.org") ; <--
         :file-name "test/${citekey}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))
```

Content of `path/to/template.org`:
```org
#+ROAM_KEY: %^{citekey}
#+PROPERTY: type %^{type}
#+ROAM_TAGS: %^{keywords}
#+PROPERTY: authors %^{author}

In this %\2 %\4 concluded that %?

fullcite:%\1
```

You can also use a function to generate the template on the fly, see
`org-capture-templates` for details.

#### Org-noter integration `%(orb-process-file-field \"${=key=}\")`

The convenience function `orb-process-file-field` allows to find
documents associated with the BibTeX entry.  It is intended to be used
in a template via a `%`-escape form for sexp (`%(sexp)`).  See
`org-capture-templates` for details.

Below shows how this can be used to integrate with
[org-noter](https://github.com/weirdNox/org-noter) or
[interleave](https://github.com/rudolfochrist/interleave):

```el
(setq orb-preformat-keywords
   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:")))
```

Do not forget to escape the quotes inside the `%`-escapes form!

### <a name="orb-note-actions-section"></a>Orb Note Actions - BibTeX record-related commands 
#### Overview

Type `M-x orb-note-actions` or bind this command to a key such as `C-c
n a` to quickly access additional commands that take the note's BibTeX
key as an input and process it to perform some useful actions.

Note actions are divided into three groups: `default`, `extra`, and
`user` set via `orb-note-actions-default`, `orb-note-actions-extra`,
`orb-note-actions-user`, respectively. There is no big conceptual
difference between the three except that the `default` note actions
are commands provided by `bibtex-completion`, `extra` note actions are
extra commands provided by `org-roam-bibtex`, and `user` note actions
are left for user customization.

#### Note actions interface

There is a number of interfaces available for displaying the available
note actions: `default` (using `completing-read`), `ido`, `ivy`,
`helm` and `hydra`.  The interface can be set via the
`orb-note-actions-frontend` user variable.

``` el
(setq orb-note-actions-frontend 'hydra)
```

Alternatively, `orb-note-actions-frontend` can be set to a custom
function that will provide completion for available note actions. The
function must take one argument CITEKEY, which is a list whose `car`
is the current note's citation key:

``` el
(setq orb-note-actions-frontend #'my-orb-note-actions-frontend)
```

``` org
#+ROAM_KEY: cite:Doe2020
```

``` el
(defun my-orb-note-actions-frontend (citekey)
  ;;; For the above note, (car citekey) => "Doe2020"
  ...)
```

#### Adding new note actions

To install a note action, add a cons
cell of format `(DESCRIPTION . FUNCTION)` to one of the note actions
variables:

``` el
(with-eval-after-load 'orb-note-actions
  (add-to-list 'orb-note-actions-user (cons "My note action" #'my-note-action)))
```

A note action must take a single argument CITEKEY, which is a list
whose car is the current note's citation key:

``` el
(defun my-note-action (citekey)
  (let ((key (car citekey)))
    ... 
    ))
```

Community
---------------
For help, support, or if you just want to hang out with us, you can find us here:

* **IRC**: channel **#org-roam** on [freenode](https://freenode.net/kb/answer/chat)
* **Slack**: channel **#org-roam-bibtex** on [Org Roam](https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg)
* **Discourse**: [Org Roam Discourse group](https://org-roam.discourse.group)

Changelog
---------------
Changelog is being maintained [here](https://github.com/org-roam/org-roam-bibtex/blob/master/CHANGELOG.md).
