# do

[![Build Status](https://travis-ci.com/guaraqe/do.svg?branch=master)](https://travis-ci.com/guaraqe/do)

The @do@ tool defines commands line interfaces in folders using a @do.nix@ file.
Since the Nix language is used, we get imports and functions for free, which allow to reuse and generate command line interfaces.

## Installing

You can either use:

- `cabal install`
- `nix-build release.nix -A do`

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

That will allow you to use the following commands:

```
$ do echo --word Hi
$ do ls
$ do subcommand ls
```

## How it works

Scripts can be either files or inline scripts.
Each case behaves differently:

- when the script is a file, variables are passed to the script as positional arguments, in the order in which the variables are defined in the file;
- when the script is inline, variables are passed as environment variables.

Variables are always passed as explicit flags.
