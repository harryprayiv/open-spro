{
  description = "Open espresso machine: firmware, service, UI, CAD and PCB tooling";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, fenix, purescript-overlay }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          f {
            inherit system;
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ purescript-overlay.overlays.default ];
            };
          });

      # The ESP32-C6 is RISC-V, so upstream stable Rust builds the firmware.
      firmwareTarget = "riscv32imac-unknown-none-elf";

      rustToolchainFor = system:
        let fx = fenix.packages.${system};
        in fx.combine [
          fx.stable.cargo
          fx.stable.rustc
          fx.stable.rust-src
          fx.stable.clippy
          fx.stable.rustfmt
          fx.targets.${firmwareTarget}.stable.rust-std
        ];
    in
    {
      devShells = forAllSystems ({ system, pkgs }:
        let
          rustToolchain = rustToolchainFor system;

          firmwareTools = [
            rustToolchain
            pkgs.espflash
          ];

          serviceTools = [
            pkgs.ghc
            pkgs.cabal-install
          ];

          uiTools = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy
            pkgs.esbuild
            # spago runs the test suite with node
            pkgs.nodejs
          ];

          designTools = [
            pkgs.freecad
            pkgs.kicad
          ];
        in
        {
          # Everything. Large, because of FreeCAD and KiCad.
          default = pkgs.mkShell {
            name = "espresso";
            packages = firmwareTools ++ serviceTools ++ uiTools ++ designTools;
            CARGO_BUILD_TARGET = firmwareTarget;
          };

          # Code only, for machines that do not need the CAD tools.
          code = pkgs.mkShell {
            name = "espresso-code";
            packages = firmwareTools ++ serviceTools ++ uiTools;
            CARGO_BUILD_TARGET = firmwareTarget;
          };

          # CAD and PCB only.
          design = pkgs.mkShell {
            name = "espresso-design";
            packages = designTools;
          };
        });

      templates.default = {
        path = ./.;
        description = "Open espresso machine project skeleton";
      };
    };
}