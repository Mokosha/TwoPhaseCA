Two-Phase Cellular Automata
======================

This program is a small implementation of Conway's Game of Life with a twist:
At first, the red cells are set to on or off, and they only affect the green cells.
Then, vice versa, ad infinitum.

## Building and running
This library depends on the `Lambency` framework, which can be [found here](https://github.com/Mokosha/Lambency). Once
that repository is cloned, the rest of the steps are straightforward:

    $ cd path/to/this/repo
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal install path/to/Lambency/repo
    $ cabal configure
    $ cabal run TwoPhaseCA

## Controls

  - `R`: Toggle automatic simulation
  - `Space`: Step once in simulation
  - `Q`: Quit