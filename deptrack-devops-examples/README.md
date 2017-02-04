DepTrack for Devops: some examples
==================================

This repository shows examples of how to use DepTrack for Devops.

This repository contains examples (currently one) which are binaries built from
the `app/` directory and which pull a default main library. This default main
contains a primitive command line parser as well as some boilerplate wiring;
this way, one can take a declarative approach when describing some piece of
infrastructure. A real application would organize descriptions in library
modules and provide some better user frontend (e.g., a command line with
optparse-applicative or a web frontend).

# Prerequisites
- you need a linux system (tested on Ubuntu16.04)
- you need git to clone this repository
- you need to have installed The Haskell Tool Stack

# Installation and usage of examples

One can build all examples from the deptrack-devops-examples repository `stack
install`.  If everything goes well, you'll get something like `Copied
executables to /home/devop/.local/bin:` ; add this directory to your `PATH` and
you'll get binaries using the default main.

Binaries using the default main accept exactly one argument:
- list: shows tuples of (node-ID, node-description) in increasing node-ID
  order; node-ID are hashes of the description
- print: prints a tree-based representation of the dependencies
- dot: output a graphviz-language graph representation of the dependencies,
  roots represent highest-level goals
- check-dot: like dot but first checks every statuses to pinpoint broken nodes
- up: turns up every node in the graph, starting from the leaves and towards
  the roots, maximizing concurrency when multiple branches are available
- down: turns down every node in the graph, starting from the roots and towards
  the leaves
- upkeep: runs continuously trying to keep up every node in the graph (i.e.,
  periodically check them)

Running such binaries with other set of arguments will show you a summary:
```
deptrack-devops default main:
  Available arguments:
    up, down, upkeep, print, dot, check-dot, list
```

# deptrack-devops-example-devtools

This target is a simple first example. I use a similar system when starting
a new VM where I'll be developping, I need some packages, my VIM configuration
etc. The code has a `main` function which passes the command-line arguments to
the defaul main; the default optimization is to batch all debian packages
together (i.e., this way we apt-get install them all at once, we do not fear
locking when we concurrently operate the graph).

The output of `dot -Tpng -o deptrack-devops-devtools.png <( deptrack-devops-example-devtools dot )`
is as follows:
![graph of deptrack-devops-example-devtools dependencies](deptrack-devops-example-devtools.png)

We observe a number of things:
- some nodes map one-to-one with the description in the code (e.g., dotfiles generation)
- some nodes were not expressed in the graph: the library code in
  deptrack-devops-recipes does this work underneath (e.g., adding an apt
  repository to install the Haskell Stack tool)
- all Debian packages have been lumped into a single node (as per the optimization)

# deptrack-devops-example-postgrest

This target is a more involved example. We want to play with PostGrest, and
excellent REST-api provider for any PostGreSQL database. We want to get the
dev-branch for PostGrest hence we need to build it from its Git source.  We
also need to install PostGre and configure a user and a database.

The output of `dot -Tpng -o deptrack-devops-postgrest.png <( deptrack-devops-example-postgrest dot )`
is as follows:
![graph of deptrack-devops-example-postgrest dependencies](deptrack-devops-example-postgrest.png)

This graph is more complicated than the previous one. We can see the many
operations required to administrate a simple database. And a lot is missing!
No backups are defined, there are no table creations (and hence Postgrest will
have nothing to serve). Though, you get the feeling of how to improve this
example; creating a few tables and views could be a good addition to this
example (I'm currently leaving this for future work).
