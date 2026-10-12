# Upstream fixes and backports needed by packdiff

Workarounds in packdiff for missing facilities in the streamly ecosystem.
Each one is marked with an `XXX` comment at the place of use.

## streamly-core

* Backport `dropCommonPrefixBy` (`Streamly.Internal.Data.Stream`, on
  master, not in any release) to 0.3.x. packdiff uses it to highlight the
  differing parts of `-` and `+` lines; until then it drops the common
  prefix of two lists with local code (`PackDiff.Pretty.dropCommonPrefix`).

## streamly-process

* Wait for the child process when a process stream is aborted by an
  exception. `cleanupException` sends SIGTERM to the child and reaps it in
  a forked thread, so the code after the exception, e.g. deleting the work
  clone or restoring a branch, can run while the child is still running.
  An option such as `waitOnException` (with a timeout and SIGKILL) would
  fix this. packdiff works around it:
  * Ctrl-C: `interruptChildOnly True` delivers SIGINT to the child only;
    the process library raises `UserInterrupt` after the child exits.
  * SIGTERM/SIGHUP: deleting the work directory is retried a few times
    (`PackDiff.WorkDir.removeTree`).
* `ProcessFailure` carries only the exit code. The program name and
  arguments would make the error message useful; packdiff wraps every
  call to add them (`PackDiff.Process.withFailure`). The source already
  has a TODO for this.

## streamly-coreutils

* Release on Hackage. packdiff pins master (`9daf64d`) with a
  `source-repository-package` in `cabal.project`, which a Hackage release
  of packdiff cannot depend on.
* A process liveness check (`kill -0` on POSIX, `OpenProcess` on Windows)
  and the current process id. packdiff implements both with `unix`/`Win32`
  and CPP (`PackDiff.WorkDir`).
* Path versions of the `directory` functions packdiff uses:
  `getXdgDirectory`, `doesPathExist`, `doesFileExist`,
  `doesDirectoryExist` and `makeAbsolute`. coreutils functions take a
  streamly `Path` while these take a `FilePath`, so packdiff converts at
  each call.
* Converting signals to exceptions: installing SIGTERM/SIGHUP handlers
  (and the Windows console handler) that throw to the main thread so that
  `bracket` cleanups run (`PackDiff.Signals`). This is a general facility
  for any program that cleans up on exit.

## streamly-packages flake

* Update `streamly-coreutils` in `sources.nix` to a revision with the
  `Coreutils.*` namespace (`9daf64d` or later); the pinned `fa18006` still
  uses `Streamly.Coreutils.*`.
* Make `optparse-applicative`, `ansi-terminal` and `hspec` available to
  the packdiff dev shell.

Until then, build in the nix shell with the user cabal config, which can
fetch the missing packages from Hackage:

```sh
env -u CABAL_CONFIG cabal build all
env -u CABAL_CONFIG cabal test
```
