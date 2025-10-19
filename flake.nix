{
  description = "Transity";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            bash
            bun
            coreutils
            gnumake
            purescript
            watchexec
          ];
        };
        formatter = pkgs.nixfmt-tree; # Format this file with `nix fmt`
      }
    );
}
