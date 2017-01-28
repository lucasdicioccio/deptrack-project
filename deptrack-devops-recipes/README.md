
DepTrack for DevOps: Recipes
============================

This library provides a nodes to configure small-scale servers using the
node-type defined in "DepTrack for DevOps".

These nodes are Debian/Ubuntu centric because it's what the author use at home.

Particularly interesting are nodes for running services withing virtual machines:
- setting up Qemu virtual machines (and their network)
- bootstrap a Qemu image from scratch
- configure some SSH-CA-based primitive authentication
- serializing and sending closures onto remote machines (i.e., to configure a
  remote node, such as a Qemu virtual machine)

In particular, the author is proud of the fact that by defining types for
remote machines and network services, we can define remote services and proxy
these services. Then we can ensure at typecheck time that firewalls/NATs will
be properly wired.
