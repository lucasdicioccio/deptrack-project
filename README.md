DepTrack project
================

Umbrella repository for the various DepTrack projects.

# Goal of DepTrack.

Please read individual subprojects descriptions below until I find time to
write some abstract here.

TODO: write a mini intro

# Using DepTrack

TODO: write a mini intro, a tutorial, and link to it;
for now the gist is:
- clone the repo
- stack install the repo
- take example on deptrack-devops-examples/ to build your binaries

# Sub-projects

## deptrack-core

The lowest-level brick, a monad transformer to describe trees and
directed-acyclic graphs. Don't be afraid of the jargon, it only means that the
key finding of the DepTrack approach is that your program can log information
and that this log of information is way more useful when it's shaped like a
tree than when it's shaped like a linear sequence.

## deptrack-devops

Devops is a job title, and Devops engineers need to do two main things: (a)
describe an infrastructure and, (b) evaluate the description (e.g., to make the
world converge towards their intention, or to diagnose which bits of the system
diverged too much). In particular, (a) encodes a dependency graph between
various subsystems of the infrastructure: faults and outages follow
dependencies. Hence, the Devops space is a good playground for the DepTrack
approach of building dependency graph.

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
hard to deploy and have poor performance, abismal multicore scalability.

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
  provides the basics. You can build your Main program as you wish, read
  parameters as your organization needs it rather than trying to retrofit a
  Devops tool on your organization. It's not more work because it needs fewer
  "ugly hacks".
- one can modify the dependency graph before evaluating it (e.g., to batch
  actions that can be done together)
- the type system ensures that the graph generation is deterministic (i.e.,
  a same input gives the same dependency graph)
- the type system allows you to speak about high level concepts such as "remote
  services providing Foo", "local service consuming Foo", and making sure you
  bind these two things correctly together
- Haskell usually compiles to a fast, multithreaded, easy-to-deploy binary

One drawback of using Haskell is that there's more upfront learning than
learning YAML+scripting languages. However, longer term, the author believes
you can reach further with DepTrack than existing Devops tools.

## deptrack-devops-recipes

This module provides some recipes to configure machines. It contains things I
found useful to configure my personal servers; mostly Debian-like.
Organization of this module is likely to change.
