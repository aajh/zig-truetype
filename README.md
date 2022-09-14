# zig-truetype
A GPU TrueType font renderer implemented using Zig and OpenGL.

The program works by reading the quadratic Bézier curves from the TrueType file and uploading them to
the GPU. On the GPU, the glyphs are rasterized using signed distance functions (SDF). In particular,
an approximation of signed distance to the Bézier curve is used. The original GLSL code can be found
at https://www.shadertoy.com/view/MlKcDD, which is turn is adapted from https://hhoppe.com/ravg.pdf.

Since this particular SDF only provides the distance without sign, a winding number calculation is
used to determine the sign of the distance, or whatever the pixel is inside or outside the glyph.
The GLSL implementation is by Elijah Stone and can be found in Git repository https://git.outerproduct.net/dconf2021-ff.git/
and a Youtube video describing it at https://www.youtube.com/watch?v=16-gpcjeN6c.

Text shaping support is provided by Harfbuzz, which is included in this repository.

## Usage
When the program is started, the normal view can be changed into a perspective one with the `P` key.
Make sure that the provided font file is in the working directory (the repository root when using
`zig build run` or the same directory as the executable).
Other font files and more complex glyphs might not work, since the TrueType file parser implements only
a subset of the file format.


## Building
Building this program requires Zig compiler version 0.9.1 and SDL2.

### macOS
First, install SDL2 with Homebrew:
```
brew install sdl2
```
Then run
```
zig build run
```
in the repository root.

### Windows
There are build binaries for Windows available on Github.
To build from source install SDL2 so that the header files are in `C:\include\SDL2`
and the libraries in `C:\lib`. These directories can be changed in `build.zig`.

After installing SDL2, run
```
zig build
```
and then copy `SDL2.dll` to `zig-out\bin` directory, where the binary was created.
Then in the repository root run
```
zig build run
```

### Linux
The build process hasn't been tested on Linux. First, SDL2 needs to be installed, so that
the header files are found in `/usr/local/include`, which can be changed in `build.zig`.
When that is done run
```
zig build run
```
