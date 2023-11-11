# pb-haskell

A small bitcoin library.

## Security (or the absolute lack of)

This library is a study artifact from the book [Programming Bitcoin](https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/), by Jimmy Song.
It is by any means intended to be a secure implementation. 

**Never use this code in production!!!**

You better learn how to use [bitcoin core sources](https://github.com/bitcoin) if you are developing code for dealing with bitcoin.

## Execute

* Run `stack exec -- pb-haskell` to see "We're inside the application!"
* With `stack exec -- pb-haskell --verbose` you will see the same message, with more logging.

## Run tests

`stack test`
