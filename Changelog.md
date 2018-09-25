## Unreleased

### Breaking Changes

* `CompareDelta` has been renamed to `CompareAbsoluteDiff`
* Changes to `Config` record:
    * `chartTitle` field has been renamed to `title`.
    * The type of `outputDir` is now a `Maybe`.
    * The signature of `sortBenchmarks` has changed. If your old definition of
      `sortBenchmarks` was `f` it can be replaced with the following
      definition:
        `sortBenchmarks = \g -> f $ map fst (g (Index 0))`
    * `setYScale` field has been broken down into two fields `fieldRanges` and
      `fieldGranularities`. Now you also need to specify which fields' scale
      you want to set.
* Now field name being plotted is suffixed to the output file name
  automatically

### Enhancements

* Additions to `Config` record type:
  * `sortBenchFields` added to select the fields to be plotted and to change
    their presentation order.
  * `sortBenchmarks` can now sort the results based on values corresponding to
    any field or benchmark group.
* New comparison methods `ComparePercent` and `ComparePercentDiff` have been
  added.
* `bgraph` has been deprecated and replaced by `graphCmp`.

## 0.1.4

* Fix a bug resulting in a bogus error, something like "Field [time] found at
  different indexes.." even though the field has exactly the same index at all
  places.

## 0.1.3

* Add maxrss plotting support

## 0.1.2

* Fixed a bug that caused missing graphs in some cases when multiple iterations
  of a benchmark are present in the bechmark results file.

* Better error reporting to pinpoint errors when a problem occurs.

## 0.1.1

* Support GHC 8.4

## 0.1.0

* Initial release
