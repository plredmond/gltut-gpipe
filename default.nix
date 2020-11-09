{ config ? { /*allowBroken = true;*/ }, ... }:
let
  gpipe-glfw-src = (import <nixpkgs> { }).fetchFromGitHub {
    owner = "plredmond";
    repo = "GPipe-GLFW";
    rev = "d9a43ec";
    sha256 = "0xj6ny0n9yw35m80495rjln22fn5szzj7w69911fsm4b0n0k05fk";
  };
  # extract pinned nixpkgs and haskellPackages
  elsewhere = import gpipe-glfw-src { inherit config; mkEnv = false; };
  nixpkgs = elsewhere.nixpkgs;
  haskellPackages = elsewhere.haskellPackages;
  # define the derivation and the environment
  projectPackages = with haskellPackages; [
    (callCabal2nix "tut01" ./Tut_01/hs { })
    (callCabal2nix "tut02" ./Tut_02/hs { })
    (callCabal2nix "tut03" ./Tut_03/hs { })
    (callCabal2nix "framework" ./framework/hs { })
  ];
  drv = nixpkgs.buildEnv { name = "gltut-project"; paths = projectPackages; };
  env = ((builtins.head projectPackages).envFunc { withHoogle = true; }).overrideAttrs
    (old: { nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.ghcid ]; });
in
if nixpkgs.lib.inNixShell then env else drv
