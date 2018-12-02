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

[Nix](https://nixos.org/nix/) must be installed for it to work completely.

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
    script = "${./echo.sh} $word";
  };

  ls = {
    help = "Shows the folder contents";
    script = "ls";
  };

  subcommand = {
    ls = {
      help = "Shows the folder contents";
      vars = [ "folder" ];
      script = "ls $folder";
    };

  };
}
```

That will give you the following commands:

```
$ do # Shows the possible commands
$ do echo --word Hi # Executes ./echo.sh "Hi"
$ do ls
$ do subcommand ls --folder app
```

Each command has a `script` attribute that is an inline script.
Variables are passed to scripts as environment variables.
For the moment, variables must always be passed to `do` as explicit flags.

## Use case: subprojects

Suppose you have subprojects `project1` and `project2` in your repository, living in subfolders with the same name.
Each project can have its own `do.nix` file in its respective folder, and one can create a parent `do.nix` with the following contents:

```nix
{
  project1 = import ./project1/do.nix;
  project2 = import ./project2/do.nix;
}
```

which will give access to the subprojects commands from the top level.

For this to work, one has to guarantee that the commands to be executed take into account the good folder, which can be done by giving the folder as an explicit parameter with Nix.

## Use case: multiple projects with the same language

Suppose you have subprojects `project1` and `project2` in your repository, living in subfolders with the same name, written in the same language.
Both will have commands that are the same, like `build` and `test`, therefore that can be factored out.

One can create a `do-lang.nix` file with the good commands at the top level, and import it at the subproject level:

```nix
import ../do-lang.nix //

{
  some-command = {
    script = "echo hi";
  };
}
```

## Limitations

The evaluation of Nix expressions is done using [`hnix`](https://github.com/haskell-nix/hnix), so we are limited by its current evaluation capacity.
In particular, evaluating (most?) derivations from `nixpkgs` is not possible.
However, this is not a big problem since we can delegate the task of giving executables by either calling `nix-shell` in the script, or by executing the `do` commands inside a `nix-shell`.

## TO DO

- Allow positional arguments?
- Allow optional parameters?
