HaskRay
=======

Path tracing ray tracer implemented in Haskell.

Suggested Method of Testing
---------------------------

* Install Haskell Platform
* Execute `cabal update`
* Install cabal-dev: `cabal install cabal-dev`
* Compile and install sandboxed package: `cabal-dev install --cabal-install-arg=--flags=-glview`
* Run with `./cabal-dev/bin/HaskRay -w WIDTH -h HEIGHT -s SAMPLES -i INPUT_FILE -o OUTPUT_FILE [-r RANDOM_SEED] [-g GLOBAL_ILLUMINATION_DEPTH]`

Prereqs
-------

* [Awesomium 1.6.x](http://awesomium.com/) - Awesomium.framework required in working directory for OS X

Install Flags
-------------

* buildfrontend
* glview
