
nnHs --- Nanomsg for Haskell
----------------------------

This is a very early stage client for [Nanomsg](http://nanomsg.org/)
in Haskell.  Please explore it and ask questions or submit suggestions
and code. I'm extremely interested in creating a simple Haskell
interface to a simple messaging abstraction like Nanomsg and would
like input from anyone who has experience building such a thing.

[See the wiki for more information.](https://github.com/tel/hs-nanomsg/wiki)

Building
========

This library, `nanomsg`, is intended to be built and installed by
Cabal. When it reaches the `1.0.0` release I will make it available on
Hackage.

The library currently assumes you have `libnanomsg` and its headers
installed at a reasonable place on your system. It makes no attempt to
install that for you.

Is it any good?
===============

[Not yet.](https://news.ycombinator.com/item?id=3067434)

Contributing
============

#### Commits

Please commit isolated features. If you want to make major changes,
open the pull request early and let's talk about it.

73 character max on the first line of any commit.

If you commit, I will add you to the AUTHORS file unless you ask not
to be added.
