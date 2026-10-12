# packdiff

packdiff compares two versions of a Haskell package and reports the API
changes: added, removed, changed and deprecated symbols. It builds the
hoogle file of each version with `cabal haddock` and compares the two
files. Only exposed modules are compared.

## Usage

```
packdiff diff  [REF1 [REF2]]   # API diff between two refs
packdiff check [REF1 [REF2]]   # PVP version bump required by the diff
packdiff api   [REF]           # API of one ref
```

In `diff` and `check`, `REF1` is the old version and `REF2` the new one.
`REF2` defaults to `.`; with no refs, `REF1` is `HEAD` and `REF2` is `.`.
The ref of `api` defaults to `.`.

| Ref | Meaning |
|-----|---------|
| `<rev>`, `git:<rev>` | A git commit, tag or branch. |
| `.` | The working directory, including uncommitted changes. |
| `hackage:<version>` | A published Hackage release. |
| `hackage`, `hackage:latest` | The latest Hackage release. |
| `file:<path>` | A hoogle file, e.g. one written by `packdiff api --format hoogle`. |

Examples:

```sh
packdiff diff v1.2.0 HEAD
packdiff diff                       # HEAD vs the working directory
packdiff diff hackage HEAD --fail-on breaking
packdiff diff -p streamly hackage:0.9.0 hackage:0.10.0
packdiff diff --repo https://github.com/composewell/streamly v0.9.0 v0.10.0
packdiff check v1.2.0 HEAD
packdiff api . --format hoogle > api/streamly.txt
packdiff diff file:api/streamly.txt .
```

In a repository with several packages, select the package with
`-p/--package`. `packdiff --help` and `packdiff <command> --help` list all
options.

## Output

```
[~] Streamly.Data.Fold
    [~] data Step s b
        [+] Partial :: s -> Step s b
    [~] toHashMapIO
        - toHashMapIO :: (MonadIO m, Hashable k, Ord k) => (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
        + toHashMapIO :: (MonadIO m, Hashable k) => (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
[+] Streamly.Data.Scanl (12 symbols)
[~] Streamly.Data.Stream.Prelude
    [+] useAcquire :: AcquireIO -> Config -> Config
    [D] parEval :: MonadAsync m => (Config -> Config) -> Stream m a -> Stream m a

Modules: 1 added [+], 2 changed [~]
Symbols: 2 added [+], 2 changed [~], 1 deprecated [D]
Breaking changes: 2
```

| Sigil | Meaning |
|-------|---------|
| `[+]` | Added in `REF2`. |
| `[-]` | Present in `REF1`, removed in `REF2`. |
| `[~]` | Definition changed. The `-` line is the `REF1` definition, the `+` line the `REF2` definition. |
| `[D]` | Deprecated in `REF2`, not deprecated in `REF1`. |
| `[U]` | Deprecated in `REF1`, not deprecated in `REF2`. |

Modules matching `--internal-module` (default `*.Internal.*` and
`*.Internal`) are listed separately under `Internal modules:` and do not
count towards `--fail-on`, `check` or the breaking change count.

## Exit codes

| Code | Meaning |
|------|---------|
| 0 | Success. |
| 1 | The `--fail-on` threshold was reached, or `check` found the declared version insufficient. |
| 2 | Usage error, including a missing external program. |
| 3 | Failure to obtain an API: a failed `git` or `cabal` command, a build failure or a hoogle file parse error. |

## Requirements

packdiff runs the following programs, found in `PATH`:

| Program | Needed for |
|---------|------------|
| `git` | Git refs. |
| `cabal` | Every ref except `file:`. |
| `ghc` | Every ref except `file:`. Selected with `-w`. |
| `haddock` | Every ref except `file:`. |

Network access is needed for `hackage:` refs (`cabal get`), for git refs
of a remote repository, and for building a ref whose dependencies are not
in the cabal store. Hackage versions are resolved from cabal's local
package index; run `cabal update` if a recent release is not found.

Git refs are built in temporary clones under
`$XDG_CACHE_HOME/packdiff/work`, so the working tree is never touched
unless `--in-place` is given. Hoogle files of git and Hackage refs are
cached in `$XDG_CACHE_HOME/packdiff/hoogle`; `--no-cache` disables the
cache.

## Limitations

1. Modules in `other-modules` are not compared.
2. A symbol re-exported from another module may be reported as removed.
   Use `--ignore-module` to suppress known cases.
3. Different GHC or haddock versions render signatures differently, which
   shows as false `[~]` changes. Build both refs with the same compiler,
   and do not compare `file:` refs made with different compilers.
4. Class methods are not marked in hoogle files. A function that directly
   follows a class and has the class as its first constraint is taken to
   be a method of that class.

See [dev/design.md](dev/design.md) for the design.
