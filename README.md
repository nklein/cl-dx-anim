# CL-DX-ANIM Library

The CL-DX-ANIM Library is a Common Lisp library that reads Direct X
animation files written by Blender.  The library currently only parses
the Direct X file.  Eventually, it will provide functions that allow
one to query the state of the armature and mesh at any given point in
the animation.

Each Direct X animation file can only contain one animation.  As such,
this library will eventually provide functions one can use to mix and
match compatible armatures, meshes, and animations from across
multiple files.

The main function so far in the library is `READ-DX` which reads a
Direct X animation file from a stream:

    (DEFUN READ-DX (&OPTIONAL (*STANDARD-INPUT* *STANDARD-INPUT*)) ...)

The `READ-DX` function returns a `DX` structure instance.

## The DX Structure

The `DX` structure has slots for `VERSION`, `FLOAT-SIZE`, `FRAME`,
`ANIM-RATE`, and `ANIM-SET`.

The `VERSION` field of a `DX` structure is a `DX-VERSION` structure
instance.  The `DX-VERSION` structure has fields for the `MAJOR`
version number and `MINOR` version number of the Direct X file.  This
library only supports major version 3 with minor version 1, 2, or 3.

The `FLOAT-SIZE` field of a `DX` structure tells the size (in bits) of
the floating point numbers used to generate the Direct X file.  This
field is provided for informational purposes only.  All floating point
numbers in the library are `single-float` values.

The `FRAME` field of a `DX` structure is a `DX-FRAME` structure
instance.  It is a frame of reference which contains both the object
armature and the object mesh.

The `ANIM-RATE` field of a `DX` structure is a positive number telling
the number of animation frames per second in the animation.

The `ANIM-SET` field of a `DX` structure is a `DX-ANIM-SET` structure
instance.  This structure contains information about the scale,
position, and orientation of bones in the armature at various key
frames.
