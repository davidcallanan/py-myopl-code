# MYOPL++
David's language with some improvements.
That's it.

Additions so far:
 - Multi-line comments. `#* multi-line comment *#`
 - Import statements. `IMPORT "./path/to/library/some_library.myopl"`
 - Optional parameters. `FUN a(x, y=1, z=2) -> x+y+z; a(1); a(1, 2); a(1, 2, 3)`
 - Closures: In the original, `FUN wrapper(x) -> FUN haxxor() -> x; FUN defineX(x, f) -> f(); PRINT(defineX("hacked", wrapper("closure working")))` would print `hacked` instead of the expected result of `closure working`. This functionality is now implemented properly.
 - DO expression. `DO [stuff] END`. Full behavior in `do.myopl`

## -- Original README starts below --

# py-myopl-code

All code for my "Make your own programming language in Python" tutorial series on YouTube: https://www.youtube.com/playlist?list=PLZQftyCk7_SdoVexSmwy_tBgs7P0b97yD

This code is an interpreter for a BASIC-like language written in Python 3.

The latest code is in the folder which is for the latest episode (`ep14`).

I'm now finished with the development of this project, and the tutorials are complete on YouTube.

## Forks

Improvements have been made by others so I'll link to them here:

 - [AdrianGjerstad](https://github.com/AdrianGjerstad/py-myopl-code) - started on standard library and added string indexing
