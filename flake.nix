# WARNING:
# This is still a work in progress and not yet fully functional.
{
  description = "Transity";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = { };
        # TODO: PureScript deps are currently still installed via npm
        # overlays = builtins.attrValues self.overlays;
      });
    in {
      # TODO: PureScript deps are currently still installed via npm
      # overlays = {
      #   purescript = inputs.purescript-overlay.overlays.default;
      # };

      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.buildNpmPackage {
            name = "transity";
            buildInputs = [ nixpkgsFor.${system}.nodejs_20 ];
            src = ./.;
            npmDepsHash = "sha256-yBjzdVe2nNGT3ACXhz/ZsOCm7d4ELvHGed9ZD410LAY=";
            npmBuild = "make bundle";
            installPhase = ''
              mkdir $out
              cp index.js $out
            '';
          };
        });

      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = "transity";
            # TODO: This somehow makes it use nodejs_18
            # inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = with pkgs; [
              nodejs_20
              watchexec

              # TODO: PureScript deps are currently still installed via npm
              # purs
              # purs-backend-es
              # purs-tidy-bin.purs-tidy-0_10_1
              # spago-unstable
            ];
          };
        });
  };
}
