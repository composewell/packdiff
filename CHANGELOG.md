# Revision history for packdiff

## Unreleased

* New command line interface with three commands: `diff`, `check` and
  `api`.
* Refs: git revisions, the working directory, Hackage releases and hoogle
  files. Git refs can be of another local or remote repository.
* Git refs are built in temporary work clones instead of checking them out
  in the working tree. `--in-place` checks them out in the working tree.
* Hoogle files are cached.
* New output format with `[+]`, `[-]`, `[~]`, `[D]` and `[U]` sigils, a
  summary, colors, filtering and grouping by change.
* `check` prints the PVP version bump required by the diff.
* Exit codes for CI: `--fail-on`, and distinct codes for usage errors and
  build failures.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
