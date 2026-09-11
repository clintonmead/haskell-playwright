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
            hash = "sha256-FC3Sjh4LCTqftudcwt7KO3g3c2uyWv7PixhWqSZZR4Y=";
          };
          cabalWrapped = pkgs.writeShellScriptBin "cabal" ''
            exec ${pkgs.lib.getExe hpkgs.cabal-install} -v0 "$@"
          '';
          stackWrapped = pkgs.writeShellScriptBin "stack" ''
            export STACK_IN_NIX_SHELL=1
            exec ${pkgs.lib.getExe pkgs.stack} --system-ghc --no-install-ghc --skip-ghc-check "$@"
          '';
        in
        {
          default = pkgs.mkShell {
            packages = [
              hpkgs.ghc
              cabalWrapped
              hpkgs.hpack
              stackWrapped
              pkgs.nodejs
              pkgs.playwright-driver
              pkgs.playwright-driver.browsers
              pkgs.pkg-config
            ];
            buildInputs = [
              pkgs.libyaml
              pkgs.zlib
              pkgs.gmp
            ];
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
