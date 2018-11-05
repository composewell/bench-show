The following are some of the features that can be considered for addition to
this package. These are just thoughts and not carved in stone, we may end up
implementing variations of these, or not implement them at all or implement
other features that are not listed here.

* Along with the library APIs, it would be nice to have a convenient executable
  with command line options corresponding to the Config API to generate
  graphs and reports from a csv file, as requested, without writing a program.
  From the command line we can choose the type of the report (text or graph),
  the output file, the format of the report, the comparison strategy etc.

* For the graph (as well as report) generation when multiple graphs are
  generated we can wrap them all in a nicely formatted single html output file,
  and maybe also have an option to automatically open the file in the browser.

* Optionally annotate each benchmark with its description i.e. how the
  benchmark was performed and what parameters were used. This can be included
  in a detailed report. Other than individual benchmark descriptions we can
  also have a common description for the whole group of benchmarks in a report.

* Ability to generate graphs using a logarithmic axis. Also the abiliy to
  choose the log axis automatically when the range being plotted is too wide.

* Try [chart-unit](https://hackage.haskell.org/package/chart-unit) as an
  alternative charting backend in addition to the current
  `Chart`/`Chart-diagrams`.
