# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Well, at least we try!

## [Unreleased]

### Changed
- Remove keybindigns in `org-roam-bibtex-mode-map`.

## [0.5.0] - 2021-03-17
### Added
- ORB PDF Scrapper export functionality was completely revisited.  It can be
  now controlled by the user and allows for export of intermediate text and
  BibTeX data in addition to Org data.  All the data can be exported into
  external files.  There is also some preliminary key filtering when exporting
  BibTeX data to an existing bib file
- Save progress in ORB PDF Scrapper buffers using `C-x C-s` and `C-x C-w`
- `orb-pdf-scrapper-group-references` user option to control whether the
  references extracted by ORB PDF Scrapper should be sorted into different groups
- ORB PDF Scrapper now supports different list styles including numbered lists
  for Org output, see `orb-pdf-scrapper-list-style` and
  `orb-pdf-scrapper-reference-numbers` for more details
- `orb-pdf-scrapper-prompt-to-generate-keys` allows to suppress any (annoying)
  prompts asking to generate citation keys before proceeding to Org mode
- New user option `orb-abbreviate-file-name` to force abbreviated file names
  retrieved by `orb-process-file-field`, thanks @emacsbliss
- ORB now automatically selects a template for capture if its the only one on the list.
- New issue and feature request templates for Github
- A link where you can buy me a coffee :)

Special thanks to @j-steinbach for the ideas of how to improve ORB PDF Scrapper
and fruitful discussions on export, reference grouping and reference numbers!

### Changed
- `orb-preformat-keywords` now supports only a list of strings, although cons
  cell values are supported for backward compatibility, they do not work as
  they used to previously.  `orb-bibtex-field-aliases` user option (former
  internal `orb--virtual-fields-alist`) now controls the mapping between BibTeX
  (virtual) fields and ORB keywords
- README.md was split into a short README.md and a longer `orb-manual.org`
- The main README file was revisited to clarify installation instructions
- Error messages in several places were improved
- Old variable names deprecated in v0.2 were removed
- The following symbols were marked as deprecated:

| old                                    | new                                    |
|----------------------------------------|----------------------------------------|
| `orb-insert-frontend`                  | `orb-insert-interface`                 |
| `orb-note-actions-frontend`            | `orb-note-actions-interface`           |
| `orb-pdf-scrapper-refsection-headings` | `orb-pdf-scrapper-grouped-export`      |
| `orb-pdf-scrapper-export-fields`       | `orb-pdf-scrapper-table-export-fields` |

- File layout of `org-roam-bibtex` package: `orb-note-actions.el` was merged
  with `org-roam-bibtex.el`, `ivy` and `helm`-related functions were isolated
  into separate files
- Updated copyright year to 2021
- Other internal refactoring

### Fixed
- Buggy behaviour of `orb-process-file-field`, thanks @PhDyellow
- Performance issue in `ivy-bibtex` and `helm-bibtex` caused by database
  queries in `orb-find-note-file`, thanks to @MichielCottaar for pointing this
  out.  Thanks to @cdlm for fixing another bug in `orb-find-note-file` that was
  introduced when the previous bug was fixed
- Buggy behaviour of `orb-edit-notes` in some case due to a typo, thanks
  @brabalan
- Consistent formatting between Org headings in ORB PDF Scrapper export
- Maintenance badge in the README, thankgs @cdlm

## [0.4.0] - 2020-11-07
### Added
- `orb-insert` user command to insert links to bibliography notes.  If a note does
  not exist, create it.  Thanks @garrettdreyfus for the idea!

- user options associated with `orb-insert`: `orb-insert-frontend`,
  `orb-insert-follow-link`, `orb-insert-link-description`,
  `orb-insert-generic-candidates-format`.

- user option `orb-use-as-slug` to allow for specifying what should be used to
  expand the ${slug} keyword in templates.

- Default keybindings for `orb-insert`, `orb-insert-non-ref`,
  `orb-find-file-non-ref`, `orb-note-actions`.

### Changed
- Internals of `orb-edit-notes`.  The original function was split into two.
  Template pre-selection happens in `orb--edit-notes`, not in `org-capture`,
  which obviates the need to pre-format all the templates.

### Fixed
- Fix typo in `orb--autokey-format-field` leading to the function's incorrect
  behaviour in some cases.
- Fix behaviour `orb-edit-notes` throwing an error when the BibTeX field "file"
  was not present.

## [0.3.1] - 2020-10-05
### Added
- Smart ${file} and %^{file} wildcards 
  If `orb-process-file-keyword` is non nil, process these wildcards with
  `orb-process-file-field`.  This allows to use `${file}` instead of more
  verbose `(orb-process-file-field \"${citekey}\")` in templates

- Only the files whose extensions match those specified in
  `orb-file-field-extensions` will be returned by `orb-process-file-field` or
  all if this variable is nil.

- Functions listed `orb-ignore-bibtex-store-link-functions` will be forced to
  return nil during note taking with ORB, e.g. *via* `org-ref` interface.  This
  has been introduced to achieve a better user experience with `org-ref`, which
  causes two functions, native `org-ref-bibtex-store-link` and `ol-bibtex`'
  `org-bibtex-store-link` to be defined at runtime.  With two functions,
  `org-capture` cannot decide which one to use and annoyingly prompts to choose
  one every time `org-capture` is initiated from a BibTeX buffer

### Changed
- Respect `org-roam-title-to-slug-function`, thanks to @Wetlize
- Remove `org-roam--with-template-error`, which was removed from Org Roam
- Improve ORB PDF Scrapper training session messages

### Fixed
- Broken link to Spacemacs instructions
- Ensure Anystyle receives absolute file paths

## [0.3.0] - 2020-07-29
### Added
- Feature: ORB PDF Scrapper

  ORB PDF Scrapper is an Emacs interface to
  [`anystyle`](https://github.com/inukshuk/anystyle) reference parser
  integrated into ORB.  ORB PDF Scrapper interactive process can be
  conveniently accessed via ORB Note Actions.

- Feature: ORB Anystyle

  ORB Anystyle is an Emacs Lisp wrapper for `anystyle` command line program
  that allows to conveniently supply arguments and call `anystyle` from Emacs
  Lisp programs.  Its primary use is within ORB PDF Scrapper but it can be used
  freely elsewhere.

- Feature: ORB Autokey

  ORB Autokey allows to generate citation keys from BibTeX data.  Currently,
  its primary use is within ORB PDF Scrapper but the functionality is
  independent of it and can be used elsewhere.

- Improved documentation (README)
  - Installation instructions for Spacemacs and Doom
  - Documentation for Note Actions 
  - Help me section
  
### Changed
- Internal organization of `org-roam-bibtex` files and file inter-dependencies
  towards a more modular approach
- Changes following `org-roam` upstream developments

### Fixed
- A couple of minor bugs

## [0.2.3] - 2020-05-10
### Added
- `orb--replace-virtual-fields` and `orb--virtual-fields-alist` for
  mapping `bibtex-completion` virtual field names to more conventional
  words, namely these: 
  ``` elisp
  ("=type=" . "type")
  ("=key=" . "citekey")
  ("=has-pdf=" . "pdf?")
  ("=has-note=" . "note?")
  ```

  From now on, `type`, `citekey`, `pdf?` and `note?` are recognized as
  aliases of, respectively, `=type=`, `=key=`, `=has-pdf?` and
  `=has-note=` in `orb-preformat-keywords`.
- Throw an error if an `orb-preformat-keywords` element is neither a
  string nor a cons cell.
- Github funding badge

### Changed
- No more no less: [the upstream URL](https://github.com/org-roam/org-roam-bibtex.git)! We are now a
  legit brethren of the Org-Roam family.

### Fixed
- `helm-bibtex` not showing the note indicator because
  `orb-find-note-file` wasn't adjusted to the upstream changes in
  `org-roam--get-ref-path-completions`

## [0.2.2] - 2020-05-06
### Added
- CHANGELOG (this document)
- Video demonstration of how to use org-roam-bibtex!
- Save citekey note action

### Fixed
- Bug in hydra frontend for note actions

## [0.2.1] - 2014-05-03
### Changed
- Defaults for `orb-preformat-keywords`

### Fixed
- Fixed bugs in note actions
- Fixed other bugs

## [0.2.0] - 2020-05-03
### Added
- New feature: note actions. A single interactive dispatcher command
  to perform useful commands in the notes buffer. The command displays
  a predefined set of actions using one of: Emacs default
  `completing-read`, Emacs built-in `ido`, third-party `helm`, `ivy`
  or `hydra` interfaces. User note actions can be installed.

### Changed 
- The package's namespace `org-roam-bibtex` was abbreviated to
  `orb`. The old namespace is retained only for the variables and
  functions matching `.+-mode.*` names.

### Deprecated
- `org-roam-bibtex` namespace

## [0.1.0] - 2020-04-26
### Added
- And so the journey begins...
- Meaning that it looks like a package, behaves like a package and is
  packaged like a package! On [MELPA].
- The package is meant to connect [org-roam] (note management) and
  [helm-bibtex/bibtex-completion] (bibtex management).
- Expand org-roam-capture-templates using bibtex-completion as a
  backend for retrieving bibliographic information associated with
  #+ROAM_KEY:, which is supposed to be a valid citation key in a
  bibtex file.
- org-roam-bibtex-mode: a minor mode that performs initial setup
- Filter function for org-roam-find-file to retrieve non-bibliographic
  notes (those that do not have #+ROAM_KEY:) and interactive user
  functions to use the filter.
- Find note function for bibtex-completion 
- Find note function for org-ref
- A few more things 
- And an excellent documentation!

[MELPA]: http://www.melpa.org/#/org-roam-bibtex
[org-roam]: https://github.com/jethrokuan/org-roam
[helm-bibtex/bibtex-completion]: https://github.com/tmalsburg/helm-bibtex

[Unreleased]: https://github.com/org-roam/org-roam-bibtex/compare/v0.5.0...HEAD
[0.4.0]: https://github.com/org-roam/org-roam-bibtex/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/org-roam/org-roam-bibtex/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/org-roam/org-roam-bibtex/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.3...v0.3.0
[0.2.3]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/org-roam/org-roam-bibtex/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/org-roam/org-roam-bibtex/releases/tag/v0.1.0

