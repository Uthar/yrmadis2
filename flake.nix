{

  description = "yrmadis2 3d simulation";

  inputs.nixpkgs.url = "nixpkgs/master";

  inputs.nix-cl.url = "github:uthar/nix-cl";

  outputs = { self, nixpkgs, nix-cl }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      sbcl = nix-cl.packages.x86_64-linux.sbcl;
      sbcl' = sbcl.withPackages (ps: with ps; [
        alexandria
        cffi
      ]);
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = [
          sbcl'
        ];
        shellHook = ''
          export LD_LIBRARY_PATH=${pkgs.SDL2}/lib
        '';
      };
    };
    
}
