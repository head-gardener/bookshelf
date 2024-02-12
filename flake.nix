{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ ];

      perSystem = { self', system, pkgs, lib, config, package, ... }: {
        _module.args.package = hpack: hpack.developPackage {
          root = ./.;
          name = "bookshelf";
        };

        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              haskellPackages = prev.haskellPackages.extend (_: super: {
                bookshelf = package super;
              });
            })
          ];
        };

        formatter = pkgs.nixpkgs-fmt;

        checks = {
          build = self'.packages.default;
        };

        devShells.default = with pkgs.haskellPackages; shellFor {
          nativeBuildInputs = [
            cabal-install
            hpack
            haskell-language-server
          ];

          packages = p: [ p.bookshelf ];

          withHoogle = true;
        };

        packages.default = pkgs.haskellPackages.bookshelf;
      };
    };
}
