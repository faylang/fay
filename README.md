# Fay programming language

[![Build Status](https://secure.travis-ci.org/faylang/fay.png?branch=master)](http://travis-ci.org/faylang/fay)

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

    $ cabal install fay fay-base

Or download and unpack:

    $ cabal unpack fay
    $ cd fay
    $ cabal install

To run tests from within this directory (you need nodejs installed):

    $ fay-tests

To generate documentation (from within this directory):

    $ fay-docs

Try it out (make sure you have fay-base package installed, or you'll
get an error about missing fay-base):

    $ fay examples/console.hs
    $ node examples/console.js
    Hello, World!

Cabal-dev also works:

    $ cabal-dev install

Given that cabal-dev is installed in a specific dir, you'll wanna
install fay-base in the same dir:

    $ cabal-dev install fay-base

Then run the normal stuff:

    $ cabal-dev/bin/fay-tests
    $ cabal-dev/bin/fay-docs

    $ cabal-dev/bin/fay --no-ghc examples/console.hs
    $ node examples/console.js
    Hello, World!

If you only installed with cabal-dev then you will probably get a 'no
package fay' error from GHC, so you can tell it where to get the
package from with an environment variable:

    HASKELL_PACKAGE_SANDBOX=cabal-dev/packages-7.4.1.conf cabal-dev/bin/fay examples/alert.hs

## The FFI

Here are the rules for serialization. All serialization is based on
the type you specify at the ffi declaration, e.g.:

    foo :: Int -> String
    foo = ffi "JSON.stringify(%1)"

The rules are simple:

1. Concrete type, e.g. `String`, `Maybe Int`: yes, will be de/serialized.
2. Polymorphic, e.g. `a`, `Maybe a`: will not be touched.

There are two helpers for turning on/off serialization for the above:

1. `Ptr Foo`: will not be touched.
2. `Automatic a`: will attempt to automatically serialize the `a` at runtime.

There are two utility types for interfacing with JS APIs:

* `Nullable a`: Will be serialized to `null`.
* `Defined a`: Will be serialized to `undefined`, and will be ommitted
   from serialized objects e.g. `Foo Undefined` → `{"instance":"Foo"}`.

Usually you want to access some JavaScript global when working with
the FFI so it's a good idea to always use global access so that a
local fay binding won't interfere:

    alert :: String -> Fay ()
    alert :: ffi "window.alert(%1)"

If you want to access `window` (browser) or `global` (nodejs) you can do:

    log :: String -> Fay ()
    log = ffi "(function () { this.console.log(%1); }).call()"

For Google Closure's advanced optimizations you need to use string access to properties:

    jQuery :: String -> Fay JQuery
    jQuery = ffi "window['jQuery']"


## Contributing

If you intend on submitting a pull request, whichever branch you
choose to submit a pull request on, please ensure that it is properly
rebased against master. This will ensure that the merge is clean and
that you have checked that your contribution still works against the
recent master. A typical workflow might be:

Have a remote setup that we can pull proper changes from:

    $ git remote add fay git://github.com/faylang/fay.git

Start our topic branch:

    $ git branch my-topic-branch

Hack hack hack! Once changes committed, run git pull on master and try
to rebase onto it to check whether your work is out of date.

    $ git commit -a -m "My topic done."
    $ git fetch fay
    $ git rebase fay/master -i

Install with `cabal install` and run `fay-tests`. If there are
no warnings and all tests pass, move on.

If there are any conflicts, resolve them. Install and run `fay-tests`
again. Push changes to your Github fork:

    $ git push origin my-topic-branch

Make a pull request on Github for my-topic-branch. Pull request gets
merged in. Pull from the official Github remote:

    $ git pull haskell master

Delete your topic branch:

    $ git branch -D my-topic-branch

I'm all done!
