org-roam-bibtex
============

<img src="https://raw.githubusercontent.com/Zaeph/org-roam-bibtex/master/doc/logo-r500.png" width="150">

Description
---------------

`org-roam-bibtex` is a library which offers a tighter integration between [`org-roam`](https://github.com/jethrokuan/org-roam), [`helm-bibtex`](https://github.com/tmalsburg/helm-bibtex), and [`org-ref`](https://github.com/jkitchin/org-ref).

It allows users to access their bibliographical notes in `org-roam-directory` via `helm-bibtex`, `ivy-bibtex`, or by opening `org-ref`’s `cite:` links and running `3. Add notes`.  If the note does not exist, it is created.

Demo
---------------

<img src="https://raw.githubusercontent.com/Zaeph/org-roam-bibtex/master/doc/demo.gif" width="700">

Installation
---------------

The package is not on MELPA yet, but a request has been filed.

For now, the only way to try the package is to clone the repository somewhere which `load-path` can access.

To do that:
1. Create a directory where you’d like to clone the repository, e.g. `mkdir ~/projects`.
2. `cd ~/projects`
3. `git clone https://github.com/Zaeph/org-roam-bibtex.git`

You now have the repository cloned in `~/projects/org-roam-bibtex/`.  See [Quick-start](#quick-start) to learn how to add it `load-path` and to get started with the package.

(You can also copy [`org-roam-bibtex.el`](https://github.com/Zaeph/org-roam-bibtex/blob/improve-readme/org-roam-bibtex.el) somewhere where `load-path` can access it, but you’d have to update the file manually.)

Quick-start 🚀
---------------

You can get `org-roam-bibtex` up and running by pasting the following sexps in your [init-file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html):

### With `use-package`
```el
(use-package org-roam-bibtex
  :load-path "~/projects/org-roam-bibtex/" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode))
  ```
  
### Without `use-package`
```el
(add-to-list 'load-path "~/projects/org-roam-bibtex/") ;Modify with your own path

(require 'org-roam-bibtex)

(add-hook 'after-init-hook #'org-roam-bibtex-mode)
```

### Usage

You can now access your bibliographical notes in your `org-roam-directory` via `helm-bibtex`/`ivy-bibtex` or by opening `org-ref` links.  This is done in the background by modifying the `Edit notes` actions used by these packages to use `org-roam-bibtex-edit-notes` instead of their defaults.

To revert to the default `Edit notes` action for these packages, disable `org-roam-bibtex-mode`.

### Commands

#### `org-roam-bibtex-find-non-ref-file`

Similar to `org-roam-find-file`, but it excludes your bibliographical notes from the completion-list.  This is useful if you have a lot of them and do not want to clutter up your other notes.

#### `org-roam-bibtex-insert-non-ref`

Similar to `org-roam-insert`, but it excludes your bibliographical notes from the completion-list.

Configuration
---------------

### Variables

#### `org-roam-bibtex-templates`

This variable specifies the templates to use when creating a new bibliographical note.  It builds on the syntax of `org-roam-capture-templates` by allowing you to expand predefined variables to the value of a BibTeX fields.

See `org-roam-bibtex-edit-notes` and [`org-roam-bibtex-preformat-keywords`](#org-roam-bibtex-preformat-keywords) for details.  (You can access the docstrings of any loaded function/variable from within Emacs with `C-h f` for functions/commands, and `C-h v` for variables.)

Here’s the default value of `org-roam-bibtex-templates`:
```el
(("r" "ref" plain (function org-roam-capture--get-point) ""
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
      :unnarrowed t))
```

You can modify it with `setq`.  For instance, if you want to add the cite-key in the title of the notes, you can modify the code like this (pay attention to the line with `:head`):
```el
(setq org-roam-bibtex-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${slug}"
         :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
         :unnarrowed t)))
```

If you have more than one template in `org-roam-bibtex-templates`, you will be prompted for the key of the template you want to use (`r` in the example above).  If you only have one template, it will use this one without prompting you.

See [Modifying templates](#modifying-templates) for more info.

#### `org-roam-bibtex-preformat-keywords`

The template prompt wildcards for preformatting.  Only relevant when `org-roam-bibtex-preformat-templates` is set to `t` (default).  This can be a string, a list of strings or a cons-cell alist, where each element is `(STRING . STRING)`.

Use only alphanumerical characters, dash and underscore. See `org-roam-bibtex-edit-notes` for implementation details.

1. If the value is a string, a single keyword, it is treated as a BibTeX field name, such as `=key=`. In the following example all the prompts with the `=key=` keyword will be preformatted, as well as the corresponding match group `%\1`.

```el
(setq org-roam-bibtex-preformat-keywords "=key=")
(setq org-roam-capture-templates
      ’(("r" "reference" plain (function org-roam-capture--get-point)
         "#+ROAM_KEY: %^{=key=}%? fullcite: %\1"
         :file-name "references/${=key=}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))
```

2. If the value is a list of strings they are also treated as BibTeX field names. The respective prompts will be preformatted.

```el
(setq org-roam-bibtex-preformat-keywords ’("=key=" "title"))
(setq org-roam-capture-templates
      ’(("r" "reference" plain (function org-roam-capture--get-point)
         "#+ROAM_KEY: %^{=key=}%? fullcite: %\1"
         :file-name "references/${=key=}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))
```

3. If the value is a list of cons cells, then the car of the cons cell is treated as a prompt keyword and the cdr as a BibTeX field-name, and the latter will be used to retrieve the relevant value from the BibTeX entry. If cdr is omitted, then the car is treated as the field-name.

```el
(setq org-roam-bibtex-preformat-keywords
      '(("citekey" . "=key=")
       ("type" . "=type=")
       "title"))
(setq org-roam-capture-templates
      '(("r" "reference" plain (function org-roam-capture--get-point)
         "#+ROAM_KEY: %^{citekey}%? fullcite: %\1
          #+TAGS: %^{type}
          This %\2 deals with ..."
         :file-name "references/%<%Y-%m-%d-%H%M%S>_${title}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))
```

Consult the [`helm-bibtex`](https://github.com/tmalsburg/helm-bibtex) package for additional information about BibTeX field names.

### Modifying templates

#### Handling long templates

If your template is long, you can also place the template inside a file (with the same expansion of BibTeX fields):
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
#+TAGS: %^{keywords}
#+PROPERTY: authors %^{author}

In this %\2 %\4 concluded that %?

fullcite:%\1
```

You can also use a function to generate the the template if you need something more advanced.  

#### `%(org-roam-bibtex-process-file-field \"${=key=}\")`

The convenience-function `org-roam-bibtex-process-file-field` has been added to find documents associated with the BibTeX entry.  It is intended to be used inside your template via a `%`-escapes form for sexp (`%(sexp)`).  See `org-capture-templates` for details.

Below shows how this can be used to integrate with [org-noter](https://github.com/weirdNox/org-noter) or [interleave](https://github.com/rudolfochrist/interleave):

```el
(setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))

(setq org-roam-bibtex-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
         :file-name "${slug}"
         :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(org-roam-bibtex-process-file-field \"${=key=}\")
:NOTER_PAGE:
:END:")))
```

Do not forget to escape the quotes inside the `%`-escapes form!

Community
---------------
For help, support, or if you just want to hang out with us, you can find us here:
* **IRC**: channel **#org-roam** on [freenode](https://freenode.net/kb/answer/chat)
* **Slack**: channel **#org-roam-bibtex** on [Org Roam](https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg)
