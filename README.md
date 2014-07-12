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

Read `build.sh` or do:

```sh
wget https://raw.githubusercontent.com/plredmond/gltut_haskell-gpipe/master/build.sh
sh build.sh
```

Please note that this approach to building compiles each package to its own local sandbox so there's no single shared `/bin` directory. You can run demos from the `gltut_haskell-gpipe` directory with something like:

```sh
$ Tut_01_Hello_Triangle/hs/dist/dist-sandbox-*/build/tut1/tut1
```

There are multiple demos in most of the tutorials.

### Completion

* Tutorials 1-6 are complete and resemble Jason's examples.
* Tutorials 7-17 are forthcoming.

-- [PLR](http://f06mote.com)

---

Written using *GHC 7.6.3*, *haskell-platform 2013.2.0.0*, and *GPipe 1.4.3*

Licensed under [http://creativecommons.org/licenses/by/3.0/us/].  
Original work by Jason L. McKesson. Adaptation by Patrick Redmond.
