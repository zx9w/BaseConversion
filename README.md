# Reasoning

It's a pretty common requirement for CTFs and other things to play around with base encoded strings. In one challenge I did we had figure out that some "chinese" darknet site was a data dump in Base65536 and there is also a Base131072 in the making (but unicode only has around 107000 characters as of now).

For this reason I figure I may as well write some sort of small library that allows me to load data like this into haskell so I can play around with it in GHC and then spit it out in whatever format I want.
