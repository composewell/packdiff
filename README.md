# packdiff

**Usage:**

```
packdiff diff <package-name> <rev1> <package-name> <rev2>
```

## API Diff Output

```
---------------------------------
API Annotations
---------------------------------

[A] : Added
[R] : Removed
[C] : Changed
[O] : Old definition
[N] : New definition
[D] : Deprecated

---------------------------------
API diff
---------------------------------

[C] Streamly.Data.Stream.Prelude
    [A] useAcquire :: AcquireIO -> Config -> Config
    [D] parEval :: MonadAsync m => (Config -> Config) -> Stream m a -> Stream m a
[C] Streamly.Data.Fold.Prelude
    [C] toHashMapIO
        [O] toHashMapIO :: (MonadIO m, Hashable k, Ord k) => (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
        [N] toHashMapIO :: (MonadIO m, Hashable k) => (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
```

## CI Integration

For CI integration check out packdiff github CI in the streamly repo:
https://github.com/composewell/streamly.

## Limitations

Packdiff uses the hoogle file created by haddock to generate and compare the
difference between multiple versions of a package

1. The API for modules in the `other-modules` (unexposed modules)
   sections is not generated or compared.
2. The API of a re-exported module isn't merged with the module that
   re-exports it.

The 2nd limitation might end up falsely reporting a diff even if the diff does
not exist. In our use-case where we have manual intervention this isn't a
problem and does the job well.
