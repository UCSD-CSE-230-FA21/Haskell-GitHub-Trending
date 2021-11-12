{ sources ? import ./nix/sources.nix }:     # import the sources
with
{ overlay = _: pkgs:
    {
      niv = (import sources.niv {}).niv;
    };
};
let

  pkgs = (import sources.nixpkgs) {
    overlays = [ overlay ];
    config.allowUnfree = true;
    config.allowBroken = true;
  };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    bash
    vim

    ghc
    haskellPackages.stack
    # Deps
    cabal2nix
   
    nixStable

    niv
  ];

  shellHook = ''
    # Before run command
    echo 'CSE230 - Project Env'
  '';
}