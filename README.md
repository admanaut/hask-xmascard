# hask-xmascard

A rewrite of https://github.com/admanaut/asyncxmascard in Haskell.

## Description

We want a channel where a single producer "Santa" can put Xmas cards and
multiple consumers "Elves" pick them up, concurrently, and deliver them.

## Solution

TBC - but I'm thinking of using *stm-conduit* and *stm-chans*
