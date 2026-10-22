{
  description = "Haskell bindings for Playwright";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hpkgs = pkgs.haskell.packages.ghc9103;
          playwrightSrc = pkgs.fetchFromGitHub {
            owner = "microsoft";
            repo = "playwright";
            rev = "v${pkgs.playwright-driver.version}";
            hash = "sha256-oWpJTU3Sob674bLfz0gfHK+vqOoYhK1wmYmVKhsPitM=";
          };
          cabalWrapped = pkgs.writeShellScriptBin "cabal" ''
            exec ${pkgs.lib.getExe hpkgs.cabal-install} -v0 "$@"
          '';
          stackWrapped = pkgs.writeShellScriptBin "stack" ''
            export STACK_IN_NIX_SHELL=1
            exec ${pkgs.lib.getExe pkgs.stack} --system-ghc --no-install-ghc --skip-ghc-check "$@"
          '';
          playwrightTest = pkgs.writeShellScriptBin "playwright-test" (
            builtins.readFile ./scripts/playwright-test
          );
          playwrightTestHeaded = pkgs.writeShellScriptBin "playwright-test-headed" (
            builtins.readFile ./scripts/playwright-test-headed
          );
          headedLibs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            pkgs.libGL
            pkgs.libglvnd
            pkgs.mesa
            pkgs.vulkan-loader
          ];
        in
        {
          default = pkgs.mkShell {
            # With WAYLAND_DISPLAY set, the browser's ANGLE asks for a Wayland
            # surface that SwiftShader does not provide, so WebGL is disabled.
            # The tests run headless and do not need Wayland.
            shellHook = ''
              unset WAYLAND_DISPLAY
            '';
            packages = [
              hpkgs.ghc
              cabalWrapped
              hpkgs.hpack
              hpkgs.haskell-language-server
              stackWrapped
              playwrightTest
              playwrightTestHeaded
              pkgs.nodejs
              pkgs.playwright-driver
              pkgs.playwright-driver.browsers
              pkgs.pkg-config
            ];
            buildInputs = [
              pkgs.libyaml
              pkgs.zlib
              pkgs.gmp
            ]
            ++ headedLibs;
            PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
            PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
            PLAYWRIGHT_NODEJS_PATH = "${pkgs.nodejs}/bin/node";
            PLAYWRIGHT_CORE_DIR = "${pkgs.playwright-driver}";
            PLAYWRIGHT_PROTOCOL_DIR = "${playwrightSrc}/packages/protocol/spec";
            PLAYWRIGHT_TEST_ASSETS = "${playwrightSrc}/tests/assets";
            PW_LANG_NAME = "javascript";
          };
        }
      );
    };
}
