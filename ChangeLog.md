# Changelog for pantry

## v0.5.4

* Support aeson 2

## v0.5.3

* improve and expose `fetchRepos`/`fetchReposRaw`

## v0.5.2.3

* Support for GHC 9 [#39](https://github.com/commercialhaskell/pantry/pull/39)

## v0.5.2.2

* Support for Cabal 3.4 [#38](https://github.com/commercialhaskell/pantry/pull/38)

## v0.5.2.1

* Support persistent 2.13 [#35](https://github.com/commercialhaskell/pantry/issues/35)

## v0.5.2

* Fall back to BSD tar when type cannot be detected [#33](https://github.com/commercialhaskell/pantry/issues/33)

## v0.5.1.5
* Switch back to hackage.haskell.org [#30](https://github.com/commercialhaskell/pantry/pull/30)
* Pass through basic auth credentials specified in URLs [#32](https://github.com/commercialhaskell/pantry/pull/32)

## v0.5.1.4

* Allow building with persistent-2.11 [#28](https://github.com/commercialhaskell/pantry/pull/28)

## v0.5.1.3

* Handle case where tree exists in cache by blobs are missing [#27](https://github.com/commercialhaskell/pantry/issues/27)

## v0.5.1.2

* Skip a test for [#26](https://github.com/commercialhaskell/pantry/issues/26)

## v0.5.1.1

* Fix to allow mulitiple relative path of symlink.

## v0.5.1.0

* Catch all exceptions from Casa calls and recover

## v0.5.0.0

* Make the location of LTS/Nightly snapshots configurable

## v0.4.0.1

* Removed errant log message

## v0.4.0.0

* Add a deprecation warning when using a repo/archive without a cabal file, see [Stack #5210](https://github.com/commercialhaskell/stack/issues/5210)
* Do not include repo/archive dependencies which do not include cabal files in lock files
* Remove some no longer used functions

## v0.3.0.0

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
