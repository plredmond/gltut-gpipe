# Haskell/GPipe examples for

## Learning Modern 3D Graphics Programming

These are ports of the examples from *Jason L. McKesson*'s wonderful online book [Learning Modern 3D Graphics Programming](http://www.arcsynthesis.org/gltut/) from **C++** and **GLSL** into **Haskell**. The ports use *Tobias Bexelius*' OpenGL framework [**GPipe** [1]](https://github.com/tobbebex/GPipe) [[2]](http://hackage.haskell.org/package/GPipe) to express OpenGL buffers, uniforms, vertex shaders, and fragment shaders in normal Haskell code.

### Usage

It is recommended to use a diff tool to compare each code example with the next. The ports have been written to minimize changes between tutorials, therefore diffing them will highlight many of the essential changes required by each lesson.

### Installation

The tutorials are set up to build with `cabal`. A few of the tutorials need to read data files at runtime, which requires that you `cabal install` install them. I recommend at least `haskell-platform 2013.2.0.0` and that you install into a shared sandbox.

```bash
# 1) get the code
git clone [..clone-url..]
REPO_DIR=[..repo-dir..]

# 2) make a place for the cabal sandbox
mkdir $REPO_DIR/sandbox

# 3) install tutorial dependencies into the sandbox (gpipe & deps, gltut-framework)
cd $REPO_DIR/framework/hs
cabal sandbox init --sandbox $REPO_DIR/sandbox # join the sandbox
cabal install --dry-run -v # review plan
cabal install

# 4) build and/or install a tutorial
cd $REPO_DIR/Tut_01_Hello_Triangle/hs
cabal sandbox init --sandbox $REPO_DIR/sandbox # join the sandbox
cabal build
./dist/build/tut1/tut1 # eyecandy
```

There have been some changes to GPipe that aren't on Hackage yet, so there's an extra installation step between 2 and 3.

```bash
# 2.5) install gpipe 1.4.3 into the sandbox
cd $REPO_DIR
git clone https://github.com/tobbebex/GPipe.git
cd GPipe
git checkout 673aad415e3e3fbf228a302ca14b5f0614d90d6e # GPipe 1.4.3
cabal sandbox init --sandbox $REPO_DIR/sandbox # join the sandbox
cabal install --dry-run -v # review plan
cabal install
```

### Completion

* Tutorials 1-6 are complete and resemble Jason's examples.
* Tutorials 7-17 are forthcoming.

-- [PLR](http://f06mote.com)

---

Written using *GHC 7.6.3*, *haskell-platform 2013.2.0.0*, and *GPipe 1.4.3*

Licensed under [http://creativecommons.org/licenses/by/3.0/us/].  
Original work by Jason L. McKesson. Adaptation by Patrick Redmond.
