text-utf7
=========

This module provides UTF-7 encoding/decoding for `Data.Text` values in Haskell.
It is based very closely on the equivalent UTF-8 functions in `Data.Text`
itself.

Currently it only supports IMAP's modified UTF-7 specification, but I hope to
add support for standard UTF-7 in the future.

Contributing
------------

One of the biggest contributions you could make is just by making use of the
library and filing bug reports on the github issues page!  When filing bug
reports, please provide either:

- A new (failing) test case as a pull request (you get special magic brownie
  points if you do this and I'll probably try and fix it quicker)
- or, just a sample string which will break it in the body of the message.

If you'd like to fix the bug yourself, even better!  Please include a test case
with your pull request, and provide a good commit message so that I can review
your change easily.

Thanks!

Licence
-------

Released under the BSD3 licence; see LICENSE for details.

Copyright (c) Daniel P. Wright 2014.
