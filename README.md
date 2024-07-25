# packdiff

**Usage:**

```
packdiff diff <package-name> <rev1> <package-name> <rev2>
```

**Limitations:**

Packdiff uses the hoogle file created by haddock to generate and compare the
difference between multiple versions of a package

1. The API for modules in the `other-modules` sections is not generated or
   compared.
2. The re-exported module is just considered a re-exported module. The API of
   the re-exported module isn't merged with the module that re-exports it.

The 2nd limitation might end up falsely reporting a diff even if the diff does
not exist. In our use-case where we have manual intervention this isn't a
problem and does the job well.
