{
  description = "hal-game";

  inputs = {
    nixpkgs.url = "nixpkgs/d3d2d80a2191a73d1e86456a751b83aa13085d7";
    typed-systems = {
      url = "github:YellowOnion/nix-typed-systems";
      flake = false;
    };
  };

  outputs = { self, typed-systems, nixpkgs }:
    let
      pkgName = "hal-game";
      inherit (import typed-systems) id genAttrsMapBy systems';
      systems = [ systems'.x86_64-linux systems'.aarch64-linux ];

      eachSystem = genAttrsMapBy id (system:
        let pkgs = import nixpkgs {
              inherit system;
            };
            hsLib = pkgs.haskell.lib.compose;
        in {
          inherit system pkgs;
          haskellPkgs = pkgs.haskellPackages.override {
            overrides = _: super: {
                gltf-codec = hsLib.doJailbreak
                  (hsLib.dontCheck
                    (hsLib.markUnbroken (super.gltf-codec )));
            };
          };
        }) systems;
    in {
      packages = eachSystem ({ pkgs, haskellPkgs, ... }:
        let
          pkg = haskellPkgs.callCabal2nix pkgName self {};
        in {
          ${pkgName} = pkg;
          default = pkg;
          wrapper = pkgs.writeShellScriptBin "wrapper.sh" ''
            export SDL_VIDEODRIVER=x11
            export LD_LIBRARY_PATH="$(patchelf --print-rpath ${pkgs.sdl2-compat}/lib/libSDL2.so):$(patchelf --print-rpath ${pkgs.sdl3.lib}/lib/libSDL3.so)"
            exec "${pkg}/bin/game"
            '';
        });

      devShells = eachSystem ({ pkgs, haskellPkgs, system }: {
        default = haskellPkgs.shellFor {
          withHoogle = true;
          packages = p: [ self.packages.${system}.default ];
          buildInputs = builtins.attrValues {
            inherit (pkgs) cabal-install cabal2nix renderdoc;
            inherit (haskellPkgs) ghc haskell-language-server lambdabot hlint;
          };
        };});

      formatter = eachSystem ({ pkgs, ... }: pkgs.nixfmt);
    };
}
