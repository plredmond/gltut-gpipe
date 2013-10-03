# Haskell/GPipe examples for

## Learning Modern 3D Graphics Programming

Here are ports of the examples from *Jason L. McKesson*'s wonderful online book [Learning Modern 3D Graphics Programming](http://www.arcsynthesis.org/gltut/) from **C++** and **GLSL** into **Haskell**. The ports use *Tobias Bexelius*' OpenGL framework [**GPipe** [1]](https://github.com/tobbebex/GPipe) [[2]](http://hackage.haskell.org/package/GPipe) to express OpenGL buffers, uniforms, vertex shaders, and fragment shaders in normal Haskell code.

The resulting code is free of effects and very concise. I hope this helps others to learn OpenGL and/or Haskell!

-- [PLR](http://f06mote.com)

**Note:** The initial commit contains working code for tutorials 1-6 with many embellishments. These may prove enlightening or confusing! The current version tries to stick as close to Jason's examples as possible.

---

Written using *GHC 7.6.3* and *GPipe 1.4.1*
