## Learning Modern 3D Graphics Programming
_Ported to Haskell & GPipe_

This is a port of a series of tutorials written by _Jason L. McKesson_ and originally published at ~~arcsynthesis.org/gltut~~.
That site has been offline for several years now,
and the original code at ~~bitbucket.org/alfonse/gltut/wiki/Home~~ is also no longer available.
Thankfully the [gltut tutorials](http://paroj.github.io/gltut/) are still online
because an updated [fork on github](http://github.com/paroj/gltut) is being maintained.

Here the original tutorials for C++ and GLSL are ported to Haskell
using Tobias Bexelius' OpenGL framework [GPipe](https://github.com/tobbebex/GPipe-Core)
to express OpenGL buffers, uniforms, vertex shaders, and fragment shaders in normal Haskell code.

It is recommended to use a diff tool to compare each code example with the next.
The ports have been written to minimize changes between subsequent tutorials,
therefore diffing them will highlight many of the essential changes required by each lesson.

### Cloning

This repo is structured as overlay on the tutorials repo.
The code here should run independently, but should you wish to view both projects overlaid, you can do so:

```
git clone https://github.com/paroj/gltut.git
mv gltut/.git gltut/paroj.git
git clone https://github.com/plredmond/gltut_haskell-gpipe.git
cp -r -n gltut_haskell-gpipe/ gltut/
```

### Building

The repo is currently set up to be built with nix.

* `nix-build` will build all the tutorial executables.
* `nix-shell` will drop you into an environment suitable for building the
  tutorial executables at `./Tut*/hs/` using `runhaskell Setup.hs configure && runhaskell Setup.hs build` style commands.
* `nix-shell --arg framework-target true` will drop you into an environment
  suitable for building the framework at `./framework/hs/`.

### Status

* Tutorials 1-4 are mostly ported to GPipe2.
* Tutorials 4-7 are mostly ported to GPipe1 (outdated, and probably won't build).
* Tutorials 8-17 are forthcoming.
