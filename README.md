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

### Quick start

#### With `use-package`
```el
(use-package org-roam-bibtex
  :load-path "~/projects/org-roam-bibtex/" ;Modify with your own path
  :custom
  (org-roam-bibtex-template
   '(("r" "ref" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "${slug}"
      :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n"
      :unnarrowed t)))
  :config
  (org-roam-bibtex-mode))
  ```
  
#### Without `use-package`
```el
(add-to-list 'load-path "~/projects/org-roam-bibtex/") ;Modify with your own path

(require 'org-roam-bibtex)

(setq org-roam-bibtex-template
      '(("r" "ref" plain
         (function org-roam-capture--get-point)
         ""
         :file-name "${slug}"
         :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n"
         :unnarrowed t)))

(org-roam-bibtex-mode)
```
