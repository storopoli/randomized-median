{
  description = "Randomized Median";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.git-hooks.url = "github:cachix/git-hooks.nix";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-hooks,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs = pkgs.haskell.packages."ghc984"; # 25.05 GHC version

        myDevTools = with pkgs; [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          hPkgs.cabal-install
          stack-wrapped
          zlib # External C library needed by some Haskell packages
          nil # Nix LSP
          nixfmt-rfc-style # Nix formatter
        ];

        haskellDeps = [
          hPkgs.random
          hPkgs.random-shuffle
          hPkgs.array
          hPkgs.time
          hPkgs.deepseq
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          inherit (self.checks.${system}.git-hooks-check) shellHook;

          buildInputs = myDevTools ++ haskellDeps;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };

        checks.git-hooks-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            nixfmt-rfc-style.enable = true; # Nix formatter
            fourmolu.enable = true; # Haskell formatter
            cabal-fmt.enable = true; # cabal formatter
          };
        };

      }
    );
}
