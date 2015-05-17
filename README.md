Logix -- Propositional Proof Assistant in Haskell
===
## Introduction
This package is trying to build a propositional logic PA with Haskell.

## Related Work
Edwards Yang had made a "logitext" before, which used Coq as backend, Haskell as primary langauge and Ur/Web as web development tool.

And ???? has written a book called "Haskell way to Mathematic, Logic and ???", which is suppoed to be a nice book although I have never read it yet!

## Get Started
First you need to init a sandbox:

    cabal sandbox init

You can specify a path for this sandbox, like what Xcode does for every project you have created:

    cabal sandbox init --sandbox $PATH$

Then, install all dependencies:

    cabal install --only-dependencies

Start coding!

To build your product, simply type `cabal build`. While debugging, you may prefer `cabal run`. No more makefile or similar stuffs.