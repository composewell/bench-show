{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  bench-report = localOpts ./. ["--flag no-charts"] ["--flags no-charts"];
}
];
}
