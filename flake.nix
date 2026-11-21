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

      # `nix build .#demo` builds the simulator as a static site, ready
      # for GitHub Pages, with no network inside the build. The
      # PureScript packages are the ones sim/spago.lock names, each
      # fetched from the PureScript registry and checked against the
      # hash the lock file holds for it. The compiler, esbuild and node
      # come from nixpkgs and the overlay. Nothing comes from npm.
      packages = forAllSystems ({ system, pkgs }:
        let
          lock = builtins.fromJSON (builtins.readFile ./sim/spago.lock);

          # Every package in the lock comes from the registry. One from
          # anywhere else stops the build here, where it can be seen.
          fetchPackage = name: package:
            assert package.type == "registry";
            pkgs.fetchurl {
              name = "purescript-${name}-${package.version}.tar.gz";
              url = "https://packages.registry.purescript.org/${name}/${package.version}.tar.gz";
              hash = package.integrity;
            };

          # The packages unpacked side by side, one folder each.
          dependencies = pkgs.runCommand "espresso-sim-dependencies" { } (
            ''
              mkdir -p $out
            ''
            + nixpkgs.lib.concatStrings (nixpkgs.lib.mapAttrsToList
              (name: package: ''
                mkdir $out/${name}
                tar -xzf ${fetchPackage name package} -C $out/${name} --strip-components=1
              '')
              lock.packages)
          );
        in
        {
          demo = pkgs.stdenv.mkDerivation {
            pname = "open-spro-demo";
            version = "0.1.0";
            src = ./sim;

            nativeBuildInputs = [ pkgs.purs pkgs.esbuild pkgs.nodejs ];

            # The scenarios run before the bundle is made, so a
            # simulator that fails them is not built.
            buildPhase = ''
              runHook preBuild
              purs compile 'src/**/*.purs' 'test/**/*.purs' '${dependencies}/*/src/**/*.purs' --output output
              # The compiler writes ES modules into files named .js, and
              # this tells node to read them as such.
              printf '%s\n' '{ "type": "module" }' > package.json
              printf '%s\n' 'import { main } from "./output/Test.Main/index.js";' 'main();' > scenarios.mjs
              node scenarios.mjs
              printf '%s\n' 'import { main } from "./output/Main/index.js";' 'main();' > entry.mjs
              esbuild entry.mjs --bundle --platform=browser --outfile=app.js
              runHook postBuild
            '';

            # The page loads app.js from beside itself and its textures
            # from ../imgs/textures, so the site keeps those two folders
            # side by side. The front page sends a visitor on to dev/.
            installPhase = ''
              runHook preInstall
              mkdir -p $out/dev $out/imgs/textures
              cp dev/index.html app.js $out/dev/
              cp imgs/textures/sugsugiBan.png $out/imgs/textures/
              cp imgs/textures/brutalistWall.png $out/imgs/textures/
              cp imgs/textures/bushedAlum.png $out/imgs/textures/
              cp imgs/textures/woodPanel01.png $out/imgs/textures/
              cp imgs/textures/brutalistTexture.png $out/imgs/textures/
              # The frames of the display's character, if any have been
              # drawn (Sim.Mascot).
              if [ -d imgs/screen ]; then
                cp -r imgs/screen $out/imgs/
              fi
              printf '%s\n' \
                '<!DOCTYPE html>' \
                '<meta charset="utf-8">' \
                '<title>Open-Spro simulator</title>' \
                '<meta http-equiv="refresh" content="0; url=dev/">' \
                '<a href="dev/">Open the simulator</a>' \
                > $out/index.html
              runHook postInstall
            '';
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