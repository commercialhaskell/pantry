# Changelog for pantry

## Unreleased changes

* Upgrade to Cabal 3.0

## v0.2.0.0

Bug fixes:

* Don't compare the hashes of cabal files.
  Addresses bugs such as [Stack
  #5045](https://github.com/commercialhaskell/stack/issues/5045).
  Data type changes: removed the `pmCabal` and `rpmCabal` fields.

## v0.1.1.2

Bug fixes:

* Module mapping insertions into the database are now atomic. Previously, if
  you SIGTERMed at the wrong time while running a script, you could end up with
  an inconsistent database state.

## v0.1.1.1

Other changes:

* Support building with persistent-template-2.7


## v0.1.1.0

**Changes since 0.1.0.0**

Bug fixes:

* Fix to allow dependencies on specific versions of local git repositories. See
  [#4862](https://github.com/commercialhaskell/stack/pull/4862)

Behavior changes:

* By default, do not perform expiry checks in Hackage Security. See

  [#4928](https://github.com/commercialhaskell/stack/issues/4928).

Other changes:

* Rename `pantry-tmp` package back to `pantry`, now that we have gained
  maintainership (which had been used by someone else for a candidate-only test
  that made it look like the name was free but prevented uploading a real
  package).


## 0.1.0.0

* Initial release
