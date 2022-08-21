# Trying to Learn Haskell

Requirements:

    sudo apt install haskell-platform
    cabal update

To run the programs run one of:

    cabal run learn-haskell
    cabal run makeCharts
    cabal run web

For `web`, then go to http://localhost:8081/isalive or
http://localhost:8081/chebyshev1st/5.234/10

## Cheatsheet

| Symbol         | Description                      |
| -------------- | -------------------------------- |
| `$`            | Use in place of parenthesis for function calls/applicatives/etc. `a (b c) = a $ b c` |
| `.`            | Function composition, so use in place of `$` when it's just simple function calls |
| `liftIO`       | When you have something that returns an IO monad `f`, and you want to put it where some other (but containing IO) monad should be, then you can do `liftIO f`. It converts an IO monad into the required monad if compatible. |
| `<$>`          | Shorthand for `fmap`; maps a function taking plain old types onto data wrapped in a functor, applicative, or monad, returning wrapped contents |
| `<*>`          | Similar to `<$>`, but also the function is wrapped in an applicative, just like the arguments and result |
