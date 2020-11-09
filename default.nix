{ config ? { /*allowBroken = true;*/ }, ... }:
let
  gpipe-glfw-src = (import <nixpkgs> { }).fetchFromGitHub {
    owner = "plredmond";
    repo = "GPipe-GLFW";
    rev = "b7ec81d";
    sha256 = "1nydxnldh6g7xlad4j5s0qp834aq6330087lsi1268np266rlhq9";
  };
  # extract pinned nixpkgs and haskellPackages
  elsewhere = import gpipe-glfw-src { inherit config; mkEnv = false; };
  nixpkgs = elsewhere.nixpkgs;
  # helper function to ignore gitignored files in source dirs
  git-ignore = path: nixpkgs.nix-gitignore.gitignoreSource [ ] path;
  # additively override haskellPackages to include gltut-framework
  haskellPackages = elsewhere.haskellPackages.override
    (old: {
      overrides = self: super: with nixpkgs.haskell.lib;
        (old.overrides self super) // {
          gltut-framework = self.callCabal2nix "gltut-framework" (git-ignore ./framework/hs) { };
        };
    });
  # define the derivation and the environment
  projectPackages = with haskellPackages; [
    (callCabal2nix "gltut-tut01" (git-ignore ./Tut_01/hs) { })
    (callCabal2nix "gltut-tut02" (git-ignore ./Tut_02/hs) { })
    (callCabal2nix "gltut-tut03" (git-ignore ./Tut_03/hs) { })
    (callCabal2nix "gltut-tut04" (git-ignore ./Tut_04/hs) { })
  ];
  drv = nixpkgs.buildEnv { name = "gltut-project"; paths = projectPackages; };
  env = ((builtins.head projectPackages).envFunc { withHoogle = true; }).overrideAttrs
    (old: { nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.ghcid ]; });
in
if nixpkgs.lib.inNixShell then env else drv
