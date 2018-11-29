{
  file-with-variable = {
    help = "Repeats the word";
    vars = [ "word" ];
    script = ./echo.sh;
  };

  file-no-variable = {
    help = "Shows the folder contents";
    script = ./ls.sh;
  };

  text-with-variable = {
    help = "Repeats the word";
    vars = [ "word" ];
    script = "echo $word";
  };

  text-no-variables = {
    help = "Shows the folder contents";
    script = "ls";
  };

  import =
    import ./import.nix;

  import-subfolder =
    import ./folder/import.nix;

  with-defaults = {
    script = "echo with-defaults";
  };

  sub = import ./subdo.nix;

}
