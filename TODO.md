The following are some of the features that can be considered for addition to
this package. These are just thoughts and not carved in stone, we may end up
implementing variations of these, or not implement them at all or implement
other features that are not listed here.

* Optionally annotate each benchmark with its description i.e. how the
  benchmark was performed and what parameters were used. This can be included
  in a detailed report. Other than individual benchmark descriptions we can
  also have a common description for the whole group of benchmarks in a report.

* Ability to generate graphs using a logarithmic axis. Also the abiliy to
  choose the log axis automatically when the range being plotted is too wide.

* Try [chart-unit](https://hackage.haskell.org/package/chart-unit) as an
  alternative charting backend in addition to the current
  `Chart`/`Chart-diagrams`.

* Add a custom column in the csv file during benchmarking. This column can
  record a parameter specified in the benchmark and then we can plot graphs
  based on this parameter. For example, we can measure an append benchmark for
  10, 100, 1000 and 10000 element streams and then plot the number of elements
  vs timing. In this case, the custom column may be "stream-size" and it may
  record the number of elements in the stream used for that measurement.
