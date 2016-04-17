# Whenever

This is an interpreter for the [Whenever](http://www.dangermouse.net/esoteric/whenever.html) esoteric language written in Haskell.

This project was a great opportunity for me to learn a bit more about the Parsec library (even though I ended up with a relatively naïve top-down parsing approach).

## How to build and run
### Cabal

Build with cabal (you may want to `cabal sandbox init` first):

```bash
    cabal configure
    cabal install --only-dependencies
    cabal build
```

And then run:

```bash
    ./whenever [--ast] path/to/file.we
```

### Stack

Or build with stack:

```bash
    stack build
```

And then run:

```bash
    stack exec -- Whenever [--ast] path/to/file.we
```

## Examples

You can find a few examples in the `examples` directory. Most of them have been written by the author of the language and can be found with the [description of the language](http://www.dangermouse.net/esoteric/whenever.html). The `rot13.we` example is a modified version of the example coming from [megahallon's javascript implementation of the interpreter](https://github.com/megahallon/whenever).

## License

This project is licensed under the MIT License.

Copyright (c) 2016 Basile Henry
