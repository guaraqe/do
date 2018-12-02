{
  help = "Repeats the word";
  vars = [ "word" ];
  script = "${./echo-subfolder.sh} $word";
}
