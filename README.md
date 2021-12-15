# Advent of Code 2021: Raehik Edition
yay!

## Building
tl;dr point to a GHC 9.2 fixed generic-lens ver, `hpack && cabal repl`

I wanted to use GHC 9.2 but also generic-optics (which doesn't support GHC 9.2
yet, [link](https://github.com/kcsongor/generic-lens/issues/138)), so this is a
pain to build currently. You need some fixed generic-lens libraries and then to
tell Cabal where to find them (see `cabal.project`). Then run via `hpack &&
cabal repl` etc., because `package.yaml` files are much easier than Cabal files.
(even though they have their own problems...)

## License
Provided under the MIT license. Please see `LICENSE` for the full license text.
