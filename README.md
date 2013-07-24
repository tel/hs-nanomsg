
Nanomsg for Haskell
-------------------

Please don't use this. It's an alpha client library for an alpha core
library. Please explore it, though. I'm extremely interested in
creating a simple Haskell interface to a simple messaging abstraction
like Nanomsg. I'm planning on taking a lot of inspiration from
[zeromq3-haskell](http://hackage.haskell.org/package/zeromq3-haskell).

Building
========

This library, `nanomsg`, is intended to be built and installed by
Cabal. When it reaches the `1.0.0` release I will make it available on
Hackage.

The library currently assumes you have `libnanomsg` and its headers
installed at a reasonable place on your system. It makes no attempt to
install that for you.

Contributing
============

#### Commits

Please commit isolated features. If you want to make major changes,
open the pull request early and let's talk about it.

73 character max on the first line of any commit.

If you commit, I will add you to the AUTHORS file unless you ask not
to be added.
