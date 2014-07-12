#!/usr/bin/env bash

mkdir gltut
cd gltut

git clone https://github.com/tobbebex/GPipe.git
pushd GPipe
  git checkout 673aad415e3e3fbf228a302ca14b5f0614d90d6e # GPipe 1.4.3
  cabal sandbox init
  cabal install
popd

git clone https://github.com/plredmond/gltut_haskell-gpipe.git
pushd gltut_haskell-gpipe
  pushd framework/hs
    cabal sandbox init
    cabal sandbox add-source ../../../GPipe
    cabal install
  popd
  for N in 01 02 03 04 05 06; do
    pushd Tut_${N?}*/hs
      cabal sandbox init
      cabal sandbox add-source ../../../GPipe
      cabal sandbox add-source ../../framework/hs
      cabal install
    popd
  done
popd
