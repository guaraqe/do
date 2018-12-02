{
  with-variable = {
    help = "Repeats the word";
    vars = [ "word" ];
    script = "${./echo.sh} $word";
  };

  no-variable = {
    help = "Shows the folder contents";
    script = "${./ls.sh}";
  };

}
