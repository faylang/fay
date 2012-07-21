To install:

    $ cabal install

To run tests (from within this directory):

    $ fay-tests

To generate documentation (from within this directory):

    $ fay-docs

Try it out:

    $ fay -autorun examples/console.hs
    $ node examples/console.js
    Hello, World!

Cabal-dev also works:

    $ cabal-dev install
    $ cabal-dev/bin/fay-tests
    $ cabal-dev/bin/fay-docs

    $ cabal-dev/bin/fay -autorun examples/console.hs
    $ node examples/console.js
    Hello, World!
