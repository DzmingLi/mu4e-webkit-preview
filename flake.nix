{
  description = "mu4e WebKit preview lifecycle regression tests";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/b6c98e9e6633ee64753b594ff4a5febf0367fc00";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
    in {
      checks = nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          emacs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages
            (epkgs: [ epkgs.mu4e ]);
        in {
          lifecycle = pkgs.runCommand "mu4e-webkit-preview-tests" { } ''
            export HOME="$TMPDIR"
            ${emacs}/bin/emacs --batch --eval '(package-activate-all)' \
              -L ${self} \
              -l ${self}/tests/mu4e-webkit-preview-test.el \
              -f ert-run-tests-batch-and-exit
            touch "$out"
          '';
        });
    };
}
