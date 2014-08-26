MLPicLib
--------

A simple image combinator library for ML, especially made for teaching purposes.

Inspiration: Structure and Interpretation of Computer Programs, afsnit 2.2.4
             http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4

Bitmap read/write
-----------------
Basic read/write functions for Bitmap-files are available here:
   http://www.soc.napier.ac.uk/~cs66/course-notes/sml/bmp.htm

They work, but should be improved for readability and the helper
functions should be abstracted away. Maybe wrap them in a
Bitmap-structure?

The website also contains misc. image transformations on bitmaps
(fisheye, whirl etc.)

Basic functionality
-------------------
  type color = int * int * int
  type image

  recolor : (color -> color) -> image -> image
  transform : (real*real -> real*real) -> image -> image
  scale : real -> image -> image
  beside : image -> image -> image
  clockwise : image -> image
  torben : image (* https://www2.adm.ku.dk/selv/pls/prt_www40.hentindhold_cms?p_personid=162114 *)
