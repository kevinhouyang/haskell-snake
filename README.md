# haskell-snake
This is Snake implemented in Haskell.

![Alt text](screenshots/screenshot2.png?raw=true)

We mainly built this as an exercise in learning functional programming. It uses the Gloss package for its graphics and game interfaces.

Our snake wraps around the screen, so you do not need to worry about running into walls. The score is incremented everytime an apple is eaten. The game is over whenever the snake tries to eat itself.

Building this game, we scratched the surface of a bunch of interesting topics:
- Haskell's strong type system, namely figuring out type conversion between Floats and Integrals
- Pseudo-random number generation, how that's possible when functions are deterministic
- Maintaining game states while still employing functional programming
- IO monads in Haskell
- Destructuring tuples and lists
- Lot's of recursion

We would love feedback or pull requests on anything that could be rewritten to be more concise or readable!
