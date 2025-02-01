# Learning Haskell

Requirements:

Install stack somehow?

    sudo apt install libcairo2-dev

To run the programs run one of:

    stack run helloworld
    stack run makeCharts
    stack run web
    stack run hsplot

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
| `sequenceA`    | flips around applicatives e.g. turns a list of Maybe into Just a list or Nothing |
| `traverse`     | `Applicative f => (a -> f b) -> t a -> f (t b)` maps a function reurning an applicative and then flips the applicative outside of the e.g. list |

With applicatives, you can do 

```haskell
(*) <$> (Just 3) <*> (Just 4)
```

and just add another `<*> (Just 5)` if the initial function takes 3 arguments,
and so on.

### Examples with lists

```haskell
> let x = [1,2,3]
> let y = Just <$> x
> y
[Just 1,Just 2,Just 3]
> let z = Nothing : y
> z
[Nothing,Just 1,Just 2,Just 3]
> sequenceA y
Just [1,2,3]
> sequenceA z
Nothing
> traverse Just x
Just [1,2,3]
```

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

On the other hand, using these most general functions can make error messages more confusing when using lists. Concatenation with `++` instead of `<|>` (from Alternative) and mapping with `map` instead of `fmap` or `<$>` (from Functor) can be more readable and simpler when working with a list of monads/applicatives/etc.

## Do Notation

In the Monad do notation,

```
do 
    a <- as
    bs a
```
Translates to `as >>= bs`.

-  `x <- func`: The return value of func must be the monad the do expects. The x is unpacked out of the monad for use in later lines.
- `func`: The return value of func must be the monad the do expects. The contents of the return value are discarded (this is just for the side-effects)
- `let x = func`: use this if you want to capture a return value that’s not an instance of the monad e.g. if it’s a pure function. You could instead substitute with:`x <- return $ func`
- `return x` wraps a value in the monad if needed. pure and return are equivalent.
