# Fay programming language

See [the Fay web site for documentation](http://fay-lang.org/).

## Introduction

A proper subset of Haskell that compiles to JavaScript, Fay is a small
programming language which has the following properties:

* A proper syntactic and semantic subset of Haskell
* Statically typed
* Lazy
* Pure by default
* Compiles to JavaScript
* Has fundamental data types (Double, String, etc.) based upon what JS can support
* Outputs minifier-aware code for small compressed size
* Has a trivial foreign function interface to JavaScript

## Install and run

To install:

    $ cabal install

To run tests (from within this directory):

    $ fay-tests

To generate documentation (from within this directory):

    $ fay-docs

Try it out:

    $ fay --autorun examples/console.hs
    $ node examples/console.js
    Hello, World!

Cabal-dev also works:

    $ cabal-dev install
    $ cabal-dev/bin/fay-tests
    $ cabal-dev/bin/fay-docs

    $ cabal-dev/bin/fay --autorun examples/console.hs
    $ node examples/console.js
    Hello, World!
