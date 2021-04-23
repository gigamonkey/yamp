Yet Another Markup Parser
=========================

Inspired by the concision of the Markup parser I wrote in Haskell
using the Parsec library, I decided to see if I could do better back
in Common Lisp, the first language I implemented Markup in. While I
had to implement a somewhat fancy macro to do it, the grammar here is
even more streamlined than the Haskell version.

This is the markup system I used for writing all three of my books: _[Practical Common Lisp](http://www.gigamonkeys.com/book/)_,
_[Coders at Work](http://www.codersatwork.com/)_, and _[The Grid](https://www.amazon.com/gp/product/B072K1JM33/ref=as_li_tl?ie=UTF8&tag=gigamonkeys-20&camp=1789&creative=9325&linkCode=as2&creativeASIN=B072K1JM33&linkId=a276d7ed7eda8c1d56059b8e07273dca)_.

See also:

* [Haskell implementation using Parsec](https://github.com/gigamonkey/haskell-markup)

* [A Ruby implementation I never used for anything](https://github.com/gigamonkey/markup)

* [Original Common Lisp implementation](https://github.com/gigamonkey/monkeylib-markup)
