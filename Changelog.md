## 0.3.2 (Feb 2022)

* Show an empty report on the selection of no benchmarks instead of erroring
  out.
* Add a few flags to build a minimal version of the library.
* Expose all available modules under the `Internal` namespace.

## 0.3.1

### Bug Fixes

* `report` and `graph` generation now works even when number of
  samples is less than 3

## 0.3.0

### Breaking Changes

* The signature of `selectBenchmarks` has changed, use 'Nothing' as the second
  argument of the benchmark generator function to port old code without any
  impact.
* Removed the broken 'Percent' constructor from `GroupStyle`. Use `PercentDiff`
  instead to make relative comparisons.
* The behavior of `PercentDiff` has changed, it now computes the % from the
  lower value instead of from the baseline.
* The default `diffStrategy` has been changed to `SingleEstimator` instead of
  `MinEstimator`.

### Deprecations

* Config fields `title` and `titleAnnotations` have been deprecated, please use
  `mkTitle` instead.

### Bug Fixes

* `GroupStyle Absolute` now honors the `MinEstimator` setting. When
  `MinEstimator` is set, the groups being compared to baseline now display the
  value based on the estimator which provides closest estimate to the baseline.

### Enhancements

* Add a CLI executable to generate textual reports and graphs from criterion or
  gauge csv output file.
* Add `Multiples` as a comparison option, the group being compared is shown as
  a multiple of the baseline.
* Add ability to omit the baseline group from the results when we are doing a
  relative comparison among groups.
* Add the ability to sort the benchmarks using a different criterion than the
  one used to present the benchmarks in the final report output.
* Add `mkTitle` config option to use a function for generating a custom report
  title.

## 0.2.2

* Allow additional annotations to title to be controlled via config
* Better error handling

## 0.2.1

* Use new version of `statistics` package.

## 0.2.0

### Release Notes

* Due to a bug in the `statistics` package, reporting may crash on certain
  inputs with a `vector index out of bounds` message. The bug has been fixed
  and will be available in an upcoming release.

### Breaking Changes

* The package `bench-graph` has been renamed to `bench-show` to reflect the
  fact that it now includes text reports as well. This includes the change of
  module name `BenchGraph` to `BenchShow`.
* The `bgraph` API has been removed and replaced by `graph`
* The way output file is generated has changed. Now field name or group name
  being plotted or both may be suffixed to the output file name automatically.
  The estimator type (e.g. mean or median) is also suffixed to the filename.
* Changes to `Config` record:
    * `chartTitle` field has been renamed to `title`.
    * The type of `outputDir` is now a `Maybe`.
    * `comparisonStyle` has been replaced by `presentation`
    * `ComparisonStyle` has been replaced by `Presentation`
    * `sortBenchmarks` has been replaced by `selectBenchmarks`. The new
      function can be defined as follows in terms of an older definition:
        `selectBenchmarks = \g ->
            sortBenchmarks $ either error (map fst) $ f (ColumnIndex 0)`
    * `sortBenchGroups` has been replaced by `selectGroups`
    * `setYScale` field has been broken down into two fields `fieldRanges` and
      `fieldTicks`. Now you also need to specify which fields' scale
      you want to set.

### Enhancements

* A `report` API has been added to generate textual reports
* More ways to compare groups have been added, including percent and percent
  difference
* Now we can show multiple fields as columns in a single benchmark group report
* Field units are now automatically selected based on the range of values
* Additions to `Config` record type:
  * `selectFields` added to select the fields to be plotted and to change
    their presentation order.
  * `selectBenchmarks` can now sort the results based on values corresponding to
    any field or benchmark group.
  * new fields added: `diffStrategy`, `verbose`, `estimator`, `threshold`

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
