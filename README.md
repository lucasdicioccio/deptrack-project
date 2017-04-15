DepTrack project
================

Umbrella repository for the various DepTrack projects.

# Goal of DepTrack.

DepTrack is an approach more than a tool. The key finding of the DepTrack
approach is that programs _logs are more useful when shaped like a tree_ than
when shaped like a sequence. Using such an approach, rather than manually
connecting nodes in a graph, one can focus on computing a value which describes
a high-level goal and _records_ a tree of dependencies used during the
computation.

# Using DepTrack

TODO: write a mini tutorial, and link to it;
for now the gist is:
- clone the repo
- stack install the repo
- take example on deptrack-devops-examples/ to build your binaries

This can be done as follows
```shell
git clone https://github.com/lucasdicioccio/deptrack-project.git
cd deptrack-project
stack setup
cd deptrack-devops-examples
stack install
```

# Sub-projects

## deptrack-core

The lowest-level brick, a monad transformer to describe trees and
directed-acyclic graphs. This brick allows you to write DSL-like libraries that
record directed-acyclic graphs with a node type that best describe your
problem. This approach let you decouple how you _operate_ on (i.e., what you do
with) the graph from _express_ (i.e., which library builds) the graph. This
way, the day you feel like DepTrack is no longer suitable to express your
dependency graph you can still keep your functions to operate on the graph.

## deptrack-devops

This library uses deptrack-core to perform Devops tasks.  Devops is a job
title, and Devops engineers need to do two main things: (a) describe an
infrastructure and, (b) evaluate the description (e.g., to make the world
converge towards their intention, or to diagnose which bits of the system
diverged too much). In particular, (a) encodes a dependency graph between
various subsystems of the infrastructure: faults and outages follow
dependencies. Hence, the Devops space is a good playground for the DepTrack
approach of building dependency graphs.

The author was dissatisfied with existing Devops tools: some existing tools
claim to use a declarative syntax (generally YAML) but actually use
templated-YAML. As a result, the actual description of an infrastructure (a) is
a mix of a template language and a description language. The language used in
the evaluator (b) often is the same scripting language as the one used in the
description, however they do not interact well: a main program provided by the
solution is in charge of generating the description and evaluating it. Such
main programs impose structure (which can be useful at times) but leave little
room for creativity.
Last, the fact that scripting languages dominate the Devops tools space is
annoying because applications written with scripting languages are ironically
hard to deploy and have poor performance (and abismal multicore scalability).

Existing tools have a number of drawbacks:
- using YAML as a description language prevents taking advantage of libraries
  (e.g., running a database query when generating the description, or verifying
  some configs with a testing framework)
- it's hard to manipulate the resulting graph by hand if the evaluation
  language varies from the infrastructure description language or if the
  evaluator is a monolithic program
- the devop configuring description does not build an understanding of the
  internals of the interpreter (I believe such opacity encourages copy/pasting
  to work around problems)
- the split between computation/declaration forces a code/configuration split
  as well. Such split can become un-natural and imposes extra jargon because
  the actual infrastructure description is the encouter of the computations
  with the static/declarative parts.

Our industry needs to empower Devops to describe infrastructure and operate on
the description creatively (e.g., generating statistics/simulations from such
descriptions). DepTrack is an attempt to move the state of the art forward.
Once you have a graph and manipulate it, you can estimate resource needs, you
can budget your monitoring on the most critical bits etc.

This library is written in Haskell and requires Devops to also describe
infrastructure in Haskell.  Using Haskell for DepTrack has the following
advantages:
- Devops are in charge of the dependency graph evaluation. This repository
  provides the basics way of building dependency graphs and let you write your
  main program as you wish. This approach let you read, expose, and manipulate
  configurations as your organization needs it rather than trying to retrofit a
  Devops tool on your organization. It's not more work because it needs fewer
  "ugly hacks".
- One can modify the dependency graph before evaluating it (e.g., to batch
  actions that can be done together)
- The type system ensures that the graph generation is deterministic (i.e.,
  a same input gives the same dependency graph)
- The type system allows you to speak about high level concepts such as "remote
  services providing Foo", "local service consuming Foo", and making sure you
  bind these two things correctly together
- Haskell usually compiles to an easy-to deploy binary.
- Haskell comes with mature libraries, best-in-class testing frameworks, and
  profiling tools. You can leverage all of this using DepTrack for DevOps.

One drawback of using Haskell is that there's more upfront learning than
learning YAML+scripting languages. However, longer term, the author believes
you can reach further with DepTrack than existing Devops tools.

## deptrack-devops-recipes

This module provides some recipes to configure machines. It contains things I
found useful to configure my personal servers; mostly Debian-like.
Organization of this module is likely to change. See the README in this
directory to have an up-to date description.

# Contributing to DepTrack

Contributions to DepTrack projects are welcome. Please use GitHub issues for
discussions. Please add yourself to the Contributors list in your pull
requests.

The bulleted-list of the DepTrack contributor is as follows:
- be humble
- be fair
- be kind
- be patient
- all contributions matter (even one-char typos fixes in documentation)

# Contributors & Thanks

Sorted by first name:

- Arnaud Bailly (https://github.com/abailly)
  - plenty of intellectually-challenging discussions
  - base implementation for reference nodes that are resolved at turnup
- Lucas DiCioccio (https://github.com/lucasdicioccio)
  - inventor & maintainer
