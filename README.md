# PicoBlaze Assembler (pbasm) [![Build Status](https://travis-ci.org/nullobject/pbasm.png?branch=master)](https://travis-ci.org/nullobject/pbasm)

This project implements an assembler for the PicoBlaze (KCPSM6) microcontroller in the Haskell programming language.

## Building

    > cabal install --enable-tests --only-dependencies
    > cabal build && cabal test

## References

* [Xilinx PicoBlaze](http://www.xilinx.com/products/intellectual-property/picoblaze.htm)
* [The Monad.Reader Issue 6](http://www.haskell.org/wikiupload/1/14/TMR-Issue6.pdf)
