# Haskell/GPipe examples for

## Learning Modern 3D Graphics Programming

These are ports of the examples from *Jason L. McKesson*'s wonderful online book [Learning Modern 3D Graphics Programming](http://www.arcsynthesis.org/gltut/) from **C++** and **GLSL** into **Haskell**. The ports use *Tobias Bexelius*' OpenGL framework [**GPipe** [1]](https://github.com/tobbebex/GPipe) [[2]](http://hackage.haskell.org/package/GPipe) to express OpenGL buffers, uniforms, vertex shaders, and fragment shaders in normal Haskell code.

### Usage

It is recommended to use a diff tool to compare each code example with the next. The ports have been written to minimize changes between tutorials, therefore diffing them will highlight many of the essential changes required by each lesson.

### Installation

The tutorials are set up to build with `cabal`. A few of the tutorials need to read data files at runtime, which requires that you `cabal install` install them.

#### 1. Prep your haskell environment

```sh
brew install haskell-platform # stable 2013.2.0.0

cabal install cabal-install-1.18.0.4 # b/c the 1.2x.x.x versions seem borked

# set your $PATH properly to use ~/.cabal/bin
```

#### 2. Build (copypasta to your shell)

```sh
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
  # install individual tutorials like the following block
  pushd Tut_01_Hello_Triangle/hs # <- just change this line as appropriate
    cabal sandbox init
    cabal sandbox add-source ../../../GPipe
    cabal sandbox add-source ../../framework/hs
    cabal install
  popd
popd
```

Please note that this approach to building compiles each package to its own local sandbox so there's no single shared `/bin` directory. You can run tutorials from the `gltut_haskell-gpipe` directory with something like:

```sh
$ Tut_01_Hello_Triangle/hs/dist/dist-sandbox-*/build/tut1/tut1
```

### Completion

* Tutorials 1-6 are complete and resemble Jason's examples.
* Tutorials 7-17 are forthcoming.

-- [PLR](http://f06mote.com)

---

Written using *GHC 7.6.3*, *haskell-platform 2013.2.0.0*, and *GPipe 1.4.3*

Licensed under [http://creativecommons.org/licenses/by/3.0/us/].  
Original work by Jason L. McKesson. Adaptation by Patrick Redmond.
