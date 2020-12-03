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

Org Roam BibTeX depends on [Org Roam](https://github.com/org-roam/org-roam) and
[BibTeX Completion](https://github.com/tmalsburg/helm-bibtex).  Users cloning
ORB directly from GitHub also need to install the above packages and to ensure
that `bibtex-completion` is autoloaded.  When installing ORB with a package
manager (MELPA) or from within a configuration framework (Doom, Spacemacs), no
additional steps are required.

### Soft dependencies

Some parts of Org Roam BibTeX will not work without: 

* [Org Ref](https://github.com/jkitchin/org-ref) — consider installing the
package for citation links it offers and to be able to use ORB's Note Actions
functionality.
* [AnyStyle CLI](https://github.com/inukshuk/anystyle-cli) — ORB PDF Scrapper
requires this external tool, see [Orb Anystyle](#orb-anystyle) for more
details.

Optionally:

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
    :hook (org-roam-mode . org-roam-bibtex-mode))
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
  :hook (org-roam-mode . org-roam-bibtex-mode))

;; If you cloned the repository
(use-package org-roam-bibtex
  :after org-roam
  :load-path "~/projects/org-roam-bibtex/" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode))
```

### Without `use-package`

```el
;; If you installed via MELPA
(require 'org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)

;; If you cloned the repository
(add-to-list 'load-path "~/projects/org-roam-bibtex/") ;Modify with your own path
(require 'org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
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

#### `orb-insert` (`C-c ) i`)

Select a bibliography entry and insert a link to a note associated with it.  If
the note does not exist yet, create it.  Similar to `org-roam-insert`, if a
region is selected, it becomes the link description.  Check also [orb-insert
configuration](#orb-insert-configuration) for a few configuration options.

#### `orb-note-actions` (`C-c ) a`)

Type `M-x orb-note-actions` to easily access additional commands useful
in note's context.  These commands are run with the note's BibTeX key
as an argument. The key is taken from the `#+ROAM_KEY:` file property.
See section [ORB Note Actions](#orb-note-actions-section) for
details.

#### `orb-find-non-ref-file`

Similar to `org-roam-find-file`, but it excludes your bibliographical
notes from the completion-candidates.  This is useful if you have a
lot of them and do not want to clutter up your other notes.
Default keybinding `C-c ) C-f`.

#### `orb-insert-non-ref`

Similar to `org-roam-insert`, but it excludes your bibliographical
notes from the completion-list.
Default keybinding `C-c ) C-i`.

Configuration
---------------

The following sections use Emacs Lisp examples to configure Org Roam
BibTeX.  If you are not comfortable with Lisp yet, remember you can
always use the Customize interface to achieve the same, run `M-x
customize` or from menu click `Options -> Customize Emacs -> Top Level
Customization Group` and search for `org-roam-bibtex`.

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

#### `orb-insert` configuration
##### `orb-insert-interface`

Interface to use with `orb-insert`.  Supported interfaces are `helm-bibtex`,
`ivy-bibtex`, and `generic` (`orb-insert-generic`)

##### `orb-insert-link-description`

What piece of information should be used as the link description:

* `title`    - entry's title
* `citekey`  - entry's citation key
* `citation` - insert Org-ref citation (default "cite:") instead of a file link.

##### `orb-insert-follow-link`

Whether to follow a newly created link.

##### `orb-insert-generic-candidates-format`

How the selection candidates should be presented when using `generic` interface:

* `key`   - only citation keys.  Fast and pretty, but too little contextual information
* `entry` - formatted entry.  More information, but not particluarly
pretty. Consider using `helm-bibtex` or `ivy-bibtex` instead.

#### Tips and tricks
##### Handling long templates

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

##### Org-noter integration.  Special treatment of the "file" keyword

If `orb-process-file-keyword` is non-nil, the "file" field will be treated
specially.  If the field contains only one file name, its value will be used
for template expansion.  If it contains several file names, the user will be
prompted to choose one.  The file names can be filtered based on their
extensions by setting the `orb-file-field-extensions` variable, so that only
those matching the extension or extensions will be considered for retrieval.
The "file" keyword must be set for preformatting as usual.  Consult the
docstrings of these variables for additional customization options.

Below shows how this can be used to integrate with
[org-noter](https://github.com/weirdNox/org-noter) or
[interleave](https://github.com/rudolfochrist/interleave):

```el
(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-field t
      orb-file-field-extensions "pdf")

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
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:")))
```

Do not forget to escape the quotes inside the `%`-escapes form!

### <a name="orb-note-actions-section"></a>ORB Note Actions - BibTeX record-related commands 
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
`orb-note-actions-interface` user variable.

``` el
(setq orb-note-actions-interface 'hydra)
```

Alternatively, `orb-note-actions-interface` can be set to a custom
function that will provide completion for available note actions. The
function must take one argument CITEKEY, which is a list whose `car`
is the current note's citation key:

``` el
(setq orb-note-actions-interface #'my-orb-note-actions-interface)
```

``` org
#+ROAM_KEY: cite:Doe2020
```

``` el
(defun my-orb-note-actions-interface (citekey)
  ;;; For the above note, (car citekey) => "Doe2020"
  ...)
```

#### Adding new note actions

To install a note action, add a cons cell of format `(DESCRIPTION
. FUNCTION)` to one of the note actions variables:

``` el
(with-eval-after-load 'orb-note-actions
  (add-to-list 'orb-note-actions-user (cons "My note action" #'my-note-action)))
```

A note action must take a single argument CITEKEY, which is a list
whose car is the current note's citation key:

``` el
(defun my-note-action (citekey)
  (let ((key (car citekey)))
    ...))
```
### <a name="orb-pdf-scrapper-section"></a>ORB PDF Scrapper - Retrieve references from PDFs
#### <a name="orb-pdf-scrapper-overview"></a>Overview

ORB PDF Scrapper is an Emacs interface to
[`anystyle`](https://github.com/inukshuk/anystyle), an open-source software
based on powerful machine-learning algorithms.  It requires `anystyle-cli`,
which can be installed with `[sudo] gem install anystyle-cli`.  Note that
`ruby` and `gem` must already be present in the system.  `ruby` is shipped
with MacOS, but you will have to install it on other operating systems; please
refer to the relevant section in the official documentation for `ruby`.  You
may also want to consult the [`anystyle`
documentation](https://rubydoc.info/gems/anystyle) to learn more about how it
works.

Once `anystyle-cli` is installed, ORB PDF Scrapper can be launched with
`orb-note-actions` while in an Org-roam buffer containing a `#+ROAM_KEY:`
BibTeX key.  References are retrieved from a PDF file associated with the note
which is retrieved from the corresponding BibTeX record.

The reference-retrieval process consists of three interactive steps described
below.

#### <a name="orb-pdf-scrapper-text-mode"></a>Text mode
In the first step, the PDF file is searched for references, which are
eventually output in the ORB PDF Scrapper buffer as plain text. The
buffer is in the `text-mode` major-mode for editing general text
files.

You need to review the retrieved references and prepare them for the next step
in such a way that there is only one reference per line.  You may also need to
remove any extra text captured together with the references.  Some PDF files
will produce a nicely-formed list of references that will require little to no
manual editing, while others will need a different degree of manual
intervention.
    
Generally, it is possible to train a custom `anystyle` finder model
responsible for PDF-parsing to improve the output quality, but this is
not currently supported by ORB PDF Scrapper.  As a small and somewhat
naïve aid, the `sanitize text` command bound to `C-c C-u` may assist
in putting each reference onto a separate line.

After you are finished with editing the text data, press `C-c C-c` to
proceed to the second step.

Press `C-c C-k` anytime to abort the ORB PDF Scrapper process.

#### <a name="orb-pdf-scrapper-bibtex-mode"></a>BibTeX mode
In the second step, the obtained list of plain text references, one
reference per line, is parsed and converted into BibTeX format.  The
resulting BibTeX records are presented to the user in the ORB PDF
Scrapper buffer replacing the text references.  The buffer's major
mode switches to `bibtex-mode`, which is helpful for reviewing and
editing the BibTeX data and correcting possible parsing errors.

Again, depending on the citation style used in the particular book or article,
the parsing quality can vary greatly and might require more or less manual
post-editing.  It is possible to train a custom `anystyle` parser model to
improve the parsing quality.  See [Training a Parser
model](#training-a-parser-model) for more details.

Press `C-c C-u` to generate BibTeX keys for the records in the buffer or `C-u
C-c C-u` to generate a key for the record at point.  See [ORB Autokey
configuration](#orb-autokey-configuration) on how to configure the BibTeX key
generation.  During key generation, it is also possible to automatically set
the values of BibTeX fields: see `orb-pdf-scrapper-set-fields` docstring for
more details.

Press `C-c C-r` to return to the text-editing mode in its last state.  Note
that all the progress in BibTeX mode will be lost.

Press `C-c C-c` to proceed to the third step.

#### <a name="orb-pdf-scrapper-org-mode"></a>Org mode
In the third step, the BibTeX records are processed internally by ORB PDF
Scrapper, and the result replaces the BibTeX data in the ORB PDF Scrapper,
which switches to `org-mode`.

The processing involves sorting the references into four groups under
the respective Org-mode headlines: `in-roam`, `in-bib`, `valid`, and
`invalid`, and inserting the grouped references as either an Org-mode
plain-list of `org-ref`-style citations, or an Org-mode table with
columns corresponding to different BibTeX fields.

* `in-roam` --- These references have notes with the respective
  `#+ROAM_KEY:` citation keys in the `org-roam` database.
* `in-bib` --- These references are not yet in the `org-roam` database
  but they are present in user BibTeX file(s) (see
  `bibtex-completion-bibliography`).
* `invalid` --- These references matched against
  `orb-pdf-scrapper-invalid-key-pattern` and are considered invalid.
  Adjust this variable to your criteria of validity.
* `valid` --- All other references fall into this group.  They look
  fine but are not yet in user Org-roam and BibTeX databases.

Review and edit the generated Org-mode data, or press `C-c C-c` to
insert the references into the note's buffer and finish the ORB PDF
Scrapper.

Press `C-c C-r` to return to BibTeX editing mode in its last state.
Note that all the progress in current mode will be lost.

The following user variables control the appearance of the generated
Org-mode data: `orb-pdf-scrapper-refsection-headings`,
`orb-pdf-scrapper-export-fields`.  These variables can be set through
the Customize interface or with `setq`.  Refer to their respective
docstrings in Emacs for more information.

#### Training a Parser model
##### <a name="parser-model-prerequisites"></a>Prerequisites
Currently, the core data set (explained below) must be installed manually by the user as follows:

1. Use `find`, `locate` or similar tools to find the file `core.xml` buried in
   `res/parser/` subdirectory of `anystyle` gem, e.g. `locate core.xml | grep
   anystyle`.  On MacOS, with `anystyle` installed as a system gem, the file
   path would look similar to:

   `"/Library/Ruby/Gems/2.6.0/gems/anystyle-1.3.11/res/parser/core.xml"`

   The actual path will vary slightly depending on the currently-installed
   versions of `ruby` and `anystyle`.

   On Linux and Windows, this path will be different.
2. Copy this file into the location specified in
   `orb-anystyle-parser-training-set`, or anywhere else where you have
   disk-write access, and adjust the aforementioned variable accordingly.

##### <a name="parser-model-running"></a>Running a training session
Training a custom parser model on custom user data will greatly improve the
parsing of plain-text references.  A training session can be initiated by
pressing `C-c C-t` in the ORB PDF Scrapper buffer in either text-mode or
BibTeX-mode.  In each case, the plain-text references obtained in the `text
mode` step described above will be used to generate source XML data for
a training set.

The generated XML data replaces the text or the BibTeX references in the
ORB PDF Scrapper buffer, and the major-mode switches to `xml-mode`.

The XML data must be edited manually---this is the whole point of creating
a custom training model---which usually consists in simply correcting the
placement of bibliographic data within the XML elements (data fields).  It is
extremely important to review the source data carefully since any mistakes
here will make its way into the model, thereby leading to poorer parsing in
the future.

It would be quite tedious to create the whole data-set by hand--- hundreds or
thousands of individual bibliographic records---so the best workflow for
making a good custom data-set is to use the core data-set shipped with
`anystyle` and append to it several data-sets generated in ORB PDF Scrapper
training sessions from individual PDF files, incrementally re-training the
model in between.  This approach is implemented in ORB PDF Scrapper.  From
personal experience, adding references data incrementally from 4--5 PDF files
raises the parser success rate to virtually 100%.  Follow the instructions
described in [Prerequisites](#parser-model-prerequisites) to install the core
data-set.

Once the editing is done, press `C-c C-c` to train the model.  The XML data in
the ORB PDF Scrapper buffer will be automatically appended to the custom
`core.xml` file which will be used for training.  Alternatively, press `C-c
C-t` to review the updated `core.xml` file and press `C-c C-c` when finished.

The major mode will now switch to `fundamental-mode`, and the `anystyle`
`stdout` output will appear in the buffer.  Training the model can take
_several minutes_, depending on the size of the training data-set and the
computing resources available on your device.  The process is run in a shell
subprocess, so you will be able to continue your work and return to ORB PDF
Scrapper buffer later.

Once the training is complete, press `C-c C-c` to return to the previous
editing-mode.  You can now re-generate the BibTeX data and see the
improvements achieved with the re-trained model.

#### ORB Autokey configuration
#### `orb-autokey-format`
You can specify the format of autogenerated BibTeX keys by setting the
`orb-autokey-format` variable through the Customize interface, or by adding
a `setq` form in your Emacs configuration file.

ORB Autokey format currently supports the following wildcards:

###### Basic

| Wildcard   | Field  | Description                            |
|:-----------|:-------|:---------------------------------------|
| %a         | author | first author's (or editor's) last name |
| %t         | title  | first word of title                    |
| %f{field}  | field  | first word of arbitrary field          |
| %y         | year   | year YYYY (date or year field)         |
| %p         | page   | first page                             |
| %e{(expr)} | elisp  | elisp expression                       |

``` el
(setq orb-autokey-format "%a%y") => "doe2020"
```

###### Extended

1. Capitalized versions:

| Wildcard  | Field  | Description                          |
|:----------|:-------|:-------------------------------------|
| %A        | author |                                      |
| %T        | title  | Same as %a,%t,%f{field} but          |
| %F{field} | field  | preserve the original capitalization |

``` el
(setq orb-autokey-format "%A%y") => "Doe2020"
```

2. Starred versions

| Wildcard | Field  | Description                                            |
|:---------|:-------|:-------------------------------------------------------|
| %a, %A   | author | - include author's (editor's) initials                 |
| %t, %T   | title  | - do not ignore words in orb-autokey-titlewords-ignore |
| %y       | year   | - year's last two digits __YY                          |
| %p       | page   | - use "pagetotal" field instead of default "pages"     |

``` el
(setq orb-autokey-format "%A*%y") => "DoeJohn2020"
```

3. Optional parameters

| Wildcard           | Field  | Description                                       |
|:-------------------|:-------|:--------------------------------------------------|
| %a[N][M][D]        | author |                                                   |
| %t[N][M][D]        | title  | > include first N words/names                     |
| %f{field}[N][M][D] | field  | > include at most M first characters of word/name |
| %p[D]              | page   | > put delimiter D between words                   |

`N` and `M` should be a single digit `1-9`. Putting more digits or any
other symbols will lead to ignoring the optional parameter and those
following it altogether.  `D` should be a single alphanumeric symbol or
one of `-_.:|`.

Optional parameters work both with capitalized and starred versions
where applicable.

``` el
(setq orb-autokey-format "%A*[1][4][-]%y") => "DoeJ2020"
(setq orb-autokey-format "%A*[2][7][-]:%y") => "DoeJohn-DoeJane:2020"
```

4. Elisp expression

* can be anything
* should return a string or nil
* will be evaluated before expanding other wildcards and therefore can
  be used to insert other wildcards
* will have entry variable bound to the value of BibTeX entry the key
  is being generated for, as returned by
  bibtex-completion-get-entry. The variable may be safely manipulated
  in a destructive manner.

``` el
%e{(or (bibtex-completion-get-value "volume" entry) "N/A")} 
%e{(my-function entry)}
```

##### Other variables

Check variables `orb-autokey-invalid-symbols`,
`orb-autokey-empty-field-token`, `orb-autokey-titlewords-ignore` for
additional settings.

#### Orb Anystyle

The function `orb-anystyle` provides a convenient Elisp key--value interface
to `anystyle-cli`, and can be used anywhere else within Emacs. Check its
docstring for more information.  You may also want to consult [`anystyle-cli`
documentation](https://rubydoc.info/gems/anystyle).

###### Example
This Elisp expression:
``` el
(orb-anystyle 'parse
              :format 'bib
              :stdout nil
              :overwrite t
              :input "Doe2020.txt "
              :output "bib"
              :parser-model "/my/custom/model.mod")
```

…executes the following anystyle call:

``` sh
anystyle --no-stdout --overwrite -F "/my/custom/model.mod" -f bib parse "Doe2020.txt" "bib"
```

The following variables can be used to configure `orb-anystyle` and
the default command-line options that will be passed to `anystyle`:

###### `orb-anystyle`
* `orb-anystyle-executable`
* `orb-anystyle-user-directory`
* `orb-anystyle-default-buffer`

###### Default command-line options
* `orb-anystyle-find-crop`
* `orb-anystyle-find-layout`
* `orb-anystyle-find-solo`
* `orb-anystyle-finder-training-set`
* `orb-anystyle-finder-model`
* `orb-anystyle-parser-model`
* `orb-anystyle-parser-training-set`
* `orb-anystyle-pdfinfo-executable`
* `orb-anystyle-pdftotext-executable`

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

<a href="https://www.buymeacoffee.com/mshevchuk" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" height="60" width="217" ></a>

