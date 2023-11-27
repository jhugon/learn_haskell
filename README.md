# Trying to Learn Haskell

Requirements:

Install stack somehow?

    sudo apt install libcairo2-dev

To run the programs run one of:

    stack run learn-haskell
    stack run makeCharts
    stack run web

For `web`, then go to http://localhost:8081/isalive or
http://localhost:8081/chebyshev1st/5.234/10

## Tests

To run:

    stack test

## Cheatsheet

| Symbol         | Description                      |
| -------------- | -------------------------------- |
| `$`            | Use in place of parenthesis for function calls/applicatives/etc. `a (b c) = a $ b c` |
| `.`            | Function composition, so use in place of `$` when it's just simple function calls |
| `liftIO`       | When you have something that returns an IO monad `f`, and you want to put it where some other (but containing IO) monad should be, then you can do `liftIO f`. It converts an IO monad into the required monad if compatible. |
| `<$>`          | Shorthand for `fmap`; maps a function taking plain old types onto data wrapped in a functor, applicative, or monad, returning wrapped contents |
| `<*>`          | Similar to `<$>`, but also the function is wrapped in an applicative, just like the arguments and result |

So you can do 

```haskell
(*) <$> (Just 3) <*> (Just 4)
```

and just add another `<*> (Just 5)` if the initial function takes 3 arguments,
and so on.
