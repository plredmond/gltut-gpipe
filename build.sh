#!/usr/bin/env bash

mkdir gltut
cd gltut

git clone https://github.com/tobbebex/GPipe.git
git clone https://github.com/plredmond/gltut_haskell-gpipe.git

cabal sandbox init --sandbox pkg
cabal sandbox add-source GPipe
cabal sandbox add-source gltut_haskell-gpipe/framework/hs

for N in 01 02 03 04 05 06; do
  pushd gltut_haskell-gpipe/Tut_${N?}*/hs
    cabal sandbox init --sandbox ../../../pkg
    cabal install
  popd
done
