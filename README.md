# Learning Haskell

Requirements:

Install stack somehow?

    sudo apt install libcairo2-dev

To run the programs run one of:

    stack run learn-haskell
    stack run makeCharts
    stack run web

For `web`, then go to http://localhost:8081/isalive or
http://localhost:8081/chebyshev1st/5.234/10

For TerminalPlotter, run `ghci src/TerminalPlotter.hs` and then

```haskell
printLinesOfList $ scatter 50 10 [((0),0),(1,1),(2,2),(3,3),(4,4),(0,4),(4,0)]
```

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

## Applicatives and Monads

After Haskell made Monad a subclass of Applicative, you can always use the new
(applicative or functor) name for all of these:

* `>>` is an old name for `*>`.
* `return` is an old name for `pure`.
* `map` and `liftM` are old names for `fmap`.
* `liftM2`, `liftM3`, etc. are old names for `liftA2`, `liftA3`, etc.
* `ap` is an old name for the `<*>` operator.
* `msum` is an old name for `asum`.
* `sequence` and `sequence_` are old names for `sequenceA` and `sequenceA_`.
* `mapM` and `mapM_` are old names for `traverse` and `traverse_`.
* `forM` and `forM_` are old names for `for` and `for_`.

All copied from
https://entropicthoughts.com/haskell-procedural-programming#things-you-never-need-to-care-about
by Christoffer Stjernlöf
