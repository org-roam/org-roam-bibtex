### Description

`org-roam-bibtex` is a library which offers a tighter integration between [`org-roam`](https://github.com/jethrokuan/org-roam), [`helm-bibtex`](https://github.com/tmalsburg/helm-bibtex), and [`org-ref`](https://github.com/jkitchin/org-ref).

It allows users to access their bibliographical notes in `org-roam-directory` via `helm-bibtex`, `ivy-bibtex`, or by opening `org-ref`’s `cite:` links and running `3. Add notes`.  If the note does not exist, it is created.

### Demo

<img src="https://raw.githubusercontent.com/Zaeph/org-roam-bibtex/master/doc/demo.gif" width="700">

### Installation

The package is not on MELPA yet, but a request has been filed.

For now, the only way to try the package is to clone the repository somewhere which `load-path` can access.

To do that:
1. Create a directory where you’d like to clone the repository, e.g. `mkdir ~/projects`.
2. `cd ~/projects`
3. `git clone https://github.com/Zaeph/org-roam-bibtex.git`

You now have the repository cloned in `~/projects/org-roam-bibtex/`.  See [Quick-start](#quick-start) to learn how to add it `load-path` and to get started with the package.

(You can also copy [`./org-roam-bibtex.el`](https://github.com/Zaeph/org-roam-bibtex/blob/improve-readme/org-roam-bibtex.el) somewhere where `load-path` can access it, but you’d have to update the file manually.)

### Quick-start

You can get `org-roam-bibtex` up and running by pasting the following codes in your [init-file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html):

#### With `use-package`
```el
(use-package org-roam-bibtex
  :load-path "~/projects/org-roam-bibtex/" ;Modify with your own path
  :config
  (org-roam-bibtex-mode))
  ```
  
#### Without `use-package`
```el
(add-to-list 'load-path "~/projects/org-roam-bibtex/") ;Modify with your own path

(require 'org-roam-bibtex)

(org-roam-bibtex-mode)
```

### Configuration

#### Variables

##### `org-roam-bibtex-template`

This variable specifies the template to use when creating a new note.  It builds on the syntax of `org-roam-capture-templates` by allowing you to expand predefined variables to the value of a BibTeX fields.

See `org-roam-bibtex-edit-notes` and `org-roam-bibtex-preformat-keywords` for details.  After having loaded the package, you can access the docstrings of those symbols with `C-h f` `org-roam-bibtex-edit-notes` (`f` for function) and `C-h v` `org-roam-bibtex-preformat-keywords` (`v` for variable).

Here’s the default value of `org-roam-bibtex-template`:
```el
(("r" "ref" plain (function org-roam-capture--get-point) ""
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
      :unnarrowed t))
```

You can modify it with `setq`.  For instance, if you want to add the cite-key in the title of the notes, you can modify the code like this (pay attention to the line with `:head`):
```el
(setq org-roam-bibtex-template
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${slug}"
         :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
         :unnarrowed t)))
```

