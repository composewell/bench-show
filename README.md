# bench-show

[![Hackage](https://img.shields.io/hackage/v/bench-show.svg?style=flat)](https://hackage.haskell.org/package/bench-show)
[![Build Status](https://travis-ci.com/composewell/bench-show.svg?branch=master)](https://travis-ci.org/composewell/bench-show)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/5u19xvm7sn7salrh?svg=true)](https://ci.appveyor.com/project/harendra-kumar/bench-show)

Generate text reports and graphical charts from the benchmark results generated
by `gauge` or `criterion` and stored in a CSV file. This tool is especially
useful when you have many benchmarks or if you want to compare benchmarks
across multiple packages. You can generate many interesting reports
including:

* Show individual reports for all the fields measured e.g. `time taken`, `peak
  memory usage`, `allocations`, among many other fields measured by `gauge`
* Sort benchmark results on a specified criterion e.g. you may want to see the
  biggest cpu hoggers or biggest memory hoggers on top
* Across two benchmark runs (e.g. before and after a change), show all the
  operations that resulted in a regression of more than x% in descending order,
  so that we can quickly identify and fix performance problems in our
  application.
* Across two (or more) packages providing similar functionality, show all the
  operations where the performance differs by more than 10%, so that we can
  critically analyze the packages and choose the right one.

## Quick Start

Use `gauge` or `criterion` to generate a `results.csv` file, and then use
either the `bench-show` executable or the library APIs to generate textual or
graphical reports.

## Executable

Use `bench-show` executable with `report` and `graph` sub-commands:

```
$ bench-show report results.csv
$ bench-show graph results.csv output
```

For advanced usage, control the generated report by the CLI flags.

## Library

Use `report` and `graph` library functions:

```
report "results.csv"  Nothing defaultConfig
graph  "results.csv" "output" defaultConfig
```

For advanced usage, control the generated report by modifying the
`defaultConfig`.

## Reports and Charts

`report` with `Fields` presentation style generates a multi-column report.  We
can select many fields from a `gauge` raw report.  Units of the fields are
automatically determined based on the range of values:

```
$ bench-show --presentation Fields report results.csv
```

```haskell
report "results.csv" Nothing defaultConfig { presentation = Fields }
```

```
Benchmark     time(μs) maxrss(MiB)
------------- -------- -----------
vector/fold     641.62        2.75
streamly/fold   639.96        2.75
vector/map      638.89        2.72
streamly/map    653.36        2.66
vector/zip      651.42        2.58
streamly/zip    644.33        2.59
```

`graph` generates one bar chart per field:

```
$ bench-show --presentation Fields graph results.csv
```

```
graph "results.csv" "output" defaultConfig
```

When the input file contains results from a single benchmark run, by default
all the benchmarks are placed in a single benchmark group named "default".

[![Median Time Grouped](https://github.com/composewell/bench-show/blob/master/docs/full-median-time.svg)](https://github.com/composewell/bench-show/blob/master/docs/full-median-time.svg)

## Grouping

Let's write a benchmark classifier to put the `streamly` and `vector`
benchmarks in their own groups:

```haskell
   classifier name =
       case splitOn "/" name of
           grp : bench -> Just (grp, concat bench)
           _          -> Nothing
```

Now we can show the two benchmark groups as separate columns. We can
generate reports comparing different benchmark fields (e.g. `time` and
`maxrss`) for all the groups:

```haskell
   report "results.csv" Nothing
     defaultConfig { classifyBenchmark = classifier }
```

```
(time)(Median)
Benchmark streamly(μs) vector(μs)
--------- ------------ ----------
fold            639.96     641.62
map             653.36     638.89
zip             644.33     651.42
```

We can do the same graphically as well, just replace `report` with `graph`
in the code above.  Each group is placed as a cluster on the graph. Multiple
clusters are placed side by side (i.e. on the same scale) for easy
comparison. For example:

[![Median Time Grouped](https://github.com/composewell/bench-show/blob/master/docs/grouped-median-time.svg)](https://github.com/composewell/bench-show/blob/master/docs/grouped-median-time.svg)

## Regression, Percentage Difference and Sorting

We can append benchmarks results from multiple runs to the same file. These
runs can then be compared. We can run benchmarks before and after a change
and then report the regressions by percentage change in a sorted order:

Given a results file with two runs, this code generates the report that
follows:

```haskell
   report "results.csv" Nothing
     defaultConfig
         { classifyBenchmark = classifier
         , presentation = Groups PercentDiff
         , selectBenchmarks = \f ->
              reverse
              $ map fst
              $ sortBy (comparing snd)
              $ either error id $ f $ ColumnIndex 1
         }
```

```
(time)(Median)(Diff using min estimator)
Benchmark streamly(0)(μs)(base) streamly(1)(%)(-base)
--------- --------------------- ---------------------
zip                      644.33                +23.28
map                      653.36                 +7.65
fold                     639.96                -15.63
```

It tells us that in the second run the worst affected benchmark is zip
taking 23.28 percent more time compared to the baseline.

Graphically:

[![Median Time Regression](https://github.com/composewell/bench-show/blob/master/docs/regression-percent-descending-median-time.svg)](https://github.com/composewell/bench-show/blob/master/docs/regression-percent-descending-median-time.svg)

## Full Documentation and examples

* See the [haddock documentation](http://hackage.haskell.org/package/bench-show) on Hackage
* See the [comprehensive tutorial](http://hackage.haskell.org/package/bench-show) module in the haddock docs
* For examples see the [test directory](https://github.com/composewell/bench-show/tree/master/test) in the package

## Contributions and Feedback

Contributions are welcome! Please see the [TODO.md](TODO.md) file or the
existing [issues](https://github.com/composewell/bench-show/issues) if you want
to pick up something to work on.

Any feedback on improvements or the direction of the package is welcome. You
can always send an email to the
[maintainer](https://github.com/composewell/bench-show/blob/master/bench-show.cabal)
or [raise an issue](https://github.com/composewell/bench-show/issues/new) for
anything you want to suggest or discuss, or send a PR for any change that you
would like to make.
