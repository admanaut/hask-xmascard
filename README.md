# hask-xmascard

A rewrite of https://github.com/admanaut/asyncxmascard in Haskell.

## Summary

I want a channel where a single producer "Santa" can put Xmas cards on and
multiple consumers "Elves" pick them up, concurrently, and deliver them.

## Solution

I have decided to use *stm-chans*. Even though it requires a bit of boilerplate with
forking new threads and waiting for all threads to finish, I think it exemplifies what
it takes to get concurrent channels in Haskell.

Alternatives are:
  http://hackage.haskell.org/package/stm-conduit
  http://hackage.haskell.org/package/async
