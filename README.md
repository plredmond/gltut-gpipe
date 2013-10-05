# Haskell/GPipe examples for

## Learning Modern 3D Graphics Programming

These are ports of the examples from *Jason L. McKesson*'s wonderful online book [Learning Modern 3D Graphics Programming](http://www.arcsynthesis.org/gltut/) from **C++** and **GLSL** into **Haskell**. The ports use *Tobias Bexelius*' OpenGL framework [**GPipe** [1]](https://github.com/tobbebex/GPipe) [[2]](http://hackage.haskell.org/package/GPipe) to express OpenGL buffers, uniforms, vertex shaders, and fragment shaders in normal Haskell code.

It is recommended to use a diff tool to compare each code example with the next. The ports have been written to minimize changes between tutorials, therefore diffing them will highlight many of the essential changes required by each lesson.

### There may be dragons..

The initial commit contains code for tutorials 1-6 that include many embellishments. These may prove enlightening or confusing. I'm working to make a second pass over these tutorials to make them resemble Jason's examples more closely.

* Tutorials 1-4 are clean and resemble Jason's examples.
* Tutorials 5-6 are working but have embellishments not present in Jason's examples.
* Tutorials 7-17 are forthcoming.

-- [PLR](http://f06mote.com)

---

Written using *GHC 7.6.3* and *GPipe 1.4.1*
