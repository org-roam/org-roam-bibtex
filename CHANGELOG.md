# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Well, at least we try!

## [Unreleased]

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

[Unreleased]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.3...HEAD
[0.2.3]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/org-roam/org-roam-bibtex/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/org-roam/org-roam-bibtex/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/org-roam/org-roam-bibtex/releases/tag/v0.1.0

