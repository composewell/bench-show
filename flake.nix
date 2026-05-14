{
  description = "bench-show";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=69728978adc44f53b3dd907acb2eb5bd2415fd60";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "bench-show";
      sources = import ./sources.nix;
      # sources = basepkgs.nixpack.lib.localSource "bench-show" ./.;
      packages = basepkgs.nixpack.lib.devPackage "bench-show";
    };
}
