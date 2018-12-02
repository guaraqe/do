{
  variable = {
    help = "Repeats the word";
    vars = [ "word" ];
    script = "./echo.sh $word";
  };

  no-variables = {
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
