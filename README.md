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

    $ cabal install

To run tests from within this directory (you need nodejs installed):

    $ cabal install
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

    $ cabal-dev/bin/fay --no-ghc examples/console.hs
    $ node examples/console.js
    Hello, World!

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
