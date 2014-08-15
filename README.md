clone-all
=========

Clone all the github repositories of a particular user.

This package is available on [hackage](http://hackage.haskell.org/package/clone-all)
so can be installed with

````
cabal install clone-all
````

Alternatively, you can clone this repo and install with cabal

````
git clone https://github.com/silky/clone-all.git
cd clone-all
cabal install
````

Example usage:

    mkdir temp
    clone-all temp -u silky

This will clone all the repositories I have (note: It's a lot.)


````
Usage: clone-all [DIRECTORY] (-u|--user ARG)

Available options:
  -h,--help                Show this help text
  DIRECTORY                Directory to clone everything into
  -u,--user ARG            Name of the user to clone the repos of.
````
