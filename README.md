
cabal dependencies:

* haskeline

# Get Started
First you need to init a sandbox:

```
cabal sandbox init
```

You can specify a path for this sandbox, like what Xcode does for every project you have created:

```
cabal sandbox init --sandbox $PATH$
```

Then, install all dependencies:

```
cabal install --only-dependencies
```

Start coding!

To build your product, simply type ``cabal build``. While deubing, you may prefer ``cabal run``. No more makefile or similar stuffs.
