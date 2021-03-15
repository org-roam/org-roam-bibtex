[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/org-roam/org-roam-bibtex/graphs/commit-activity)
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
3. If neither 1. nor 2. resolved your problem, read the following section on
   [how to get help](#orb-help-me).

<a name="orb-help-me"></a>HELP ME!!!
---------------

Please read carefully this section before installing Org Roam BibTeX.  There
are good chances you'll experience different problems and issues when running
or even installing Org Roam BibTeX.  These chances primarily correlate with
your level of comfort with Emacs, Lisp, Org mode, Org Roam, BibTeX and other
such things.  There may also be bugs that obviously do not depend on your
skills.  We hunt them, eliminate them, then introduce new ones, and so on.  In
any case, it's very probable that you'll need some help.  We are glad to help
you, sure, and kindly ask you to follow these simple rules when asking for
help:

1. Read carefully this README file.
2. Read it once again, please.
3. Check our [community resources](#community) and describe your problem there.
4. Open an issue on the bug tracker.
5. Take your time to describe your problem and we'll take ours to help you solve it.
6. Describe your problem clearly, in a procedural way: "I run `this command`,
then I run `that command`, and finally `this one`.  I expect `this` to happen but
instead happens `that`.  Here is my `configuration`."
7. Thank you!

Installation
---------------

### Hard dependencies

Org Roam BibTeX depends on [Org Roam](https://github.com/org-roam/org-roam)
[BibTeX Completion](https://github.com/tmalsburg/helm-bibtex), and [Org
Ref](https://github.com/jkitchin/org-ref).  Users cloning ORB directly from
GitHub also need to install the above packages and to ensure that
`bibtex-completion` is autoloaded.  When installing ORB with a package manager
(MELPA) or from within a configuration framework (Doom, Spacemacs), no
additional steps are required.

Org Roam BibTeX deliberately does not load Org Ref and delegates this tasks to
the user.  However, this package _must_ be loaded before running any Org Roam
BibTeX-related commands.  See below for examples.

### Soft dependencies

Some parts of Org Roam BibTeX will not work without: 

* [AnyStyle CLI](https://github.com/inukshuk/anystyle-cli) — ORB PDF Scrapper
requires this external tool, see [Orb Anystyle](#orb-anystyle) for more
details.

* ORB offers basic completion functions based on Emacs native completion
  interfaces, but user experience can be enhanced with Helm, Ivy and Hydra
  packages.
* Users of Projectile and Persp-mode will be pleased to learn that ORB can
  automatically switch perspective to the Org Roam project when creating a
  note.

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

You can also clone the repository somewhere in your `load-path`.  If you would
like to assist with development, this is the way to go.

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

Most probably you already have a private `org-roam` layer, if not, see examples
[here](https://org-roam.discourse.group/t/orb-helm-bibtex-open-notes-wont-create-new-note/690)
and
[here](https://www.reddit.com/r/emacs/comments/f6erh0/total_noob_how_do_i_install_orgroam_in_spacemacs/).
Add `org-roam-bibtex` to `org-roam-packages`:

``` el
(defconst org-roam-packages
  '(org-roam org-roam-bibtex))

```

add this after `org-roam/init-org-roam`:

``` el
(defun org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :init 
    (add-hook 'org-roam-mode #'org-roam-bibtex-mode)
    :config
    (require 'org-ref))
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

Then run `bin/doom sync`. The package can be configured with `use-package` (or
alternatively `Doom`'s `use-package!`) as described below.

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
  :init
  (add-hook 'org-roam-mode #'org-roam-bibtex-mode)
  :config
  (require 'org-ref))

;; If you cloned the repository
(use-package org-roam-bibtex
  :after org-roam
  :load-path "~/projects/org-roam-bibtex/" ;Modify with your own path
  :init
  (add-hook 'org-roam-mode #'org-roam-bibtex-mode)
  :config
  (require 'org-ref))
```

### Without `use-package`

```el
;; If you installed via MELPA
(require 'org-roam-bibtex)
(add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode)

;; If you cloned the repository
(add-to-list 'load-path "~/projects/org-roam-bibtex/") ;Modify with your own path
(require 'org-roam-bibtex)
(add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode)
```

Quick Usage
---------------

You can now access your bibliographical notes in your `org-roam-directory` via
`helm-bibtex`/`ivy-bibtex` or by opening `org-ref` links.  This is done in the
background by modifying the `Edit notes` actions used by these packages to use
`orb-edit-notes` instead of their defaults.

To revert those actions to their defaults, disable
`org-roam-bibtex-mode`.

#### `orb-insert` (`C-c ) i`)

Select a bibliography entry and insert a link to a note associated with it.  If
the note does not exist yet, create it.  Similar to `org-roam-insert`, if a
region is selected, it becomes the link description.

#### `orb-note-actions` (`C-c ) a`)

Type `M-x orb-note-actions` to easily access additional commands useful in
note's context.  These commands are run with the note's BibTeX key as an
argument. The key is taken from the `#+ROAM_KEY:` file property.

#### `orb-find-non-ref-file`

Similar to `org-roam-find-file`, but it excludes your bibliographical notes
from the completion-candidates.  This is useful if you have a lot of them and
do not want to clutter up your other notes.  Default keybinding `C-c ) C-f`.

#### `orb-insert-non-ref`

Similar to `org-roam-insert`, but it excludes your bibliographical notes from
the completion-list.  Default keybinding `C-c ) C-i`.

Configuration
---------------

See [the ORB Manual](doc/orb-manual.org) for configuration options and
advanced usage.

Community
---------------
For help, support, or if you just want to
hang out with us, you can find us here:

* **IRC**: channel **#org-roam** on [freenode](https://freenode.net/kb/answer/chat)
* **Slack**: channel **#org-roam-bibtex** on [Org Roam](https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg)
* **Discourse**: [Org Roam Discourse group](https://org-roam.discourse.group)

Changelog
---------------
Changelog is being maintained [here](https://github.com/org-roam/org-roam-bibtex/blob/master/CHANGELOG.md).
