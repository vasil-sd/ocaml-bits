bits — Library for manipulating of bit vectors
-------------------------------------------------------------------------------
%%VERSION%%

bits is TODO

bits is distributed under the ISC license.

Homepage: https://github.com/vasil-sd/bits  

## Installation

bits can be installed with `opam`:

    opam install bits

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
bits`.

[doc]: https://vasil-sd.github.io/bits/doc

## Sample programs

If you installed bits with `opam` sample programs are located in
the directory `opam var bits:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
