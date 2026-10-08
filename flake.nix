{
  description = "Packdiff Development Environment";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=69728978adc44f53b3dd907acb2eb5bd2415fd60";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "packdiff";
      sources = basepkgs.nixpack.lib.localSource "packdiff" ./.;
      packages = basepkgs.nixpack.lib.devPackage "packdiff";
      #sources = import ./sources.nix;
      #packages = import ./packages.nix;
    };
}
