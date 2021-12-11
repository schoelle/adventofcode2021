BEGIN{RS=","}
{ printf("%c", $0); }
