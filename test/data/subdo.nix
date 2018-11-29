{
  with-variable = {
    help = "Repeats the word";
    vars = [ "word" ];
    script = ./echo.sh;
  };

  no-variable = {
    help = "Shows the folder contents";
    script = ./ls.sh;
  };

}
