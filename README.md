# Haskell/GPipe examples for

## Learning Modern 3D Graphics Programming

This is a port of a series of tutorials written by Jason McKesson and originally published at ~~arcsynthesis.org/gltut~~.
That site has been offline for several years now,
and the [original code on bitbucket](http://bitbucket.org/alfonse/gltut/wiki/Home) has been inactive for just as long.
Thankfully there is an updated [fork on github](http://github.com/paroj/gltut) receiving love from the community and
the tutorials have been republished at <http://paroj.github.io/gltut/>.

Here the original tutorials for C++ and GLSL are ported to Haskell
using Tobias Bexelius' OpenGL framework [GPipe](https://github.com/tobbebex/GPipe)
to express OpenGL buffers, uniforms, vertex shaders, and fragment shaders in normal Haskell code.

It is recommended to use a diff tool to compare each code example with the next.
The ports have been written to minimize changes between tutorials,
therefore diffing them will highlight many of the essential changes required by each lesson.

### Cloning

This repo is an overlay on the community fork of the tutorials.
Several of the examples here use data files from that repository.

```
git clone https://github.com/paroj/gltut.git
mv gltut/.git gltut/paroj.git
git clone https://github.com/plredmond/gltut_haskell-gpipe.git
cp -r -n gltut_haskell-gpipe/ gltut/
```

### Building

To build these tutorials use haskell [stack](https://docs.haskellstack.org/en/stable/README/).
Stack is in homebrew (for mac users) and can typically be found in the package manager of linux distributions.

### Status

* Tutorials 1-6 are complete and resemble Jason's examples.
* Tutorials 7-17 are forthcoming.
