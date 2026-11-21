{
  description = "Open-Spro: firmware, service, UI, CAD and PCB tooling";

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

      # The controller core is also built for wasm, which is how the
      # simulator runs the same code the microcontroller does.
      wasmTarget = "wasm32-unknown-unknown";

      rustToolchainFor = system:
        let fx = fenix.packages.${system};
        in fx.combine [
          fx.stable.cargo
          fx.stable.rustc
          fx.stable.rust-src
          fx.stable.clippy
          fx.stable.rustfmt
          # The host's own standard library, for build scripts and for
          # `cargo test` of the controller core.
          fx.stable.rust-std
          fx.targets.${firmwareTarget}.stable.rust-std
          fx.targets.${wasmTarget}.stable.rust-std
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

      # `nix run .#core-wasm` builds the controller core for wasm and
      # writes it, as base64 text, where the simulator imports it.
      apps = forAllSystems ({ system, pkgs }:
        let
          coreWasm = pkgs.writeShellApplication {
            name = "core-wasm";
            runtimeInputs = [ (rustToolchainFor system) pkgs.stdenv.cc pkgs.coreutils pkgs.git ];
            text = ''
              root="$(git rev-parse --show-toplevel)"
              cd "$root/firmware/spro-core"
              cargo build --release --target ${wasmTarget}
              wasm="target/${wasmTarget}/release/spro_core.wasm"
              out="$root/sim/src/Sim/CoreWasm.js"
              {
                printf '%s\n' '// Generated from firmware/spro-core by nix run .#core-wasm. Do not edit.'
                printf '%s' 'export const base64 = "'
                base64 -w0 "$wasm"
                printf '%s\n' '";'
              } > "$out"
              echo "wrote $out"
            '';
          };
        in
        {
          core-wasm = {
            type = "app";
            program = "${coreWasm}/bin/core-wasm";
          };
        });

      templates.default = {
        path = ./.;
        description = "Open espresso machine project skeleton";
      };
    };
}