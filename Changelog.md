## Unreleased

### Breaking Changes

* `bgraph` has been removed and replaced by `graph`
* The way output file is generated has changed. Now field name or group name
  being plotted or both may be suffixed to the output file name automatically
* Changes to `Config` record:
    * `chartTitle` field has been renamed to `title`.
    * The type of `outputDir` is now a `Maybe`.
    * `comparisonStyle` has been replaced by `presentation`
    * `ComparisonStyle` has been replaced by `Presentation`
    * `sortBenchmarks` has been replaced by `selectBenchmarks`. The new
      function can be defined as follows in terms of an older definition:
        `selectBenchmarks = \g -> sortBenchmarks $ map fst (g (Index 0))`
    * `sortBenchGroups` has been replaced by `selectGroups`
    * `setYScale` field has been broken down into two fields `fieldRanges` and
      `fieldTicks`. Now you also need to specify which fields' scale
      you want to set.

### Enhancements

* `report` has been added to generate textual reports
* More ways to compare groups have been added, including percent and percent
  difference
* Now we can show multiple fields as columns in a single benchmark group report
* Field units are now automatically selected based on the range of values
* Additions to `Config` record type:
  * `selectFields` added to select the fields to be plotted and to change
    their presentation order.
  * `selectBenchmarks` can now sort the results based on values corresponding to
    any field or benchmark group.

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
