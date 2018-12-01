# do

[![Build Status](https://travis-ci.com/guaraqe/do.svg?branch=master)](https://travis-ci.com/guaraqe/do)

Programming has many tasks that are manually done by developers, like building, testing and profiling.
In polyglot codebases, these tasks can follow different conventions, and one needs aditional commands for integrating the different components of the project.
These can be hard to memorize, or use in the correct order.

One possibility is to use `make`. However, it is not able to pass parameters to scripts, and the use of a build system for commanding other build systems is not always appropriate.

`do` is a tool for defining commands line interfaces in folders.
It aims to:

- allow each folder to have its specific commands;
- allow parent folders to have access to the commands of subfolders, if desired;
- avoid code repetition as much as possible.

In `do`, the commands, flags and scripts are declared using a `do.nix` file.
Since the Nix language is used, we get imports and functions for free, which allows to easily reuse and generate command line interfaces.

## Installing

Installation can be done using `cabal`:

```
$ cabal install
```

Or (recommended) it can be done using Nix:

```
$ nix-build release.nix -A do
```

## Example

Write the following `do.nix` file in your folder:

```nix
{
  echo = {
    help = "Repeats the word";
    vars = [ "word" ];
    script = ./echo.sh;
  };

  ls = {
    help = "Shows the folder contents";
    script = "ls";
  };

  subcommand = {
    ls = {
      help = "Shows the folder contents";
      script = "ls";
    };

  };
}
```

That will give you the following commands:

```
$ do # Shows the possible commands
$ do echo --word Hi # Executes ./echo.sh "Hi"
$ do ls
$ do subcommand ls
```

## How it works

Scripts can be either files or inline scripts.
Each case behaves differently:

- when the script is a file, variables are passed to the script as positional arguments, in the order in which the variables are defined in the `do.nix` file;
- when the script is inline, variables are passed as environment variables.

This was decided so that Shell scripts can also be used manually if wanted, but to allow inline scripts to be more clear about their variables.

For the moment, variables must always passed to `do` as explicit flags.

## Limitations

The evaluation of Nix expressions is done using [`hnix`](https://github.com/haskell-nix/hnix), so we are limited by its current evaluation capacity.

## TO DO

- Allow positional arguments?
