echo "@prefix id: <http://aristo.allenai.org/id#> ."
echo "@prefix token: <http://nlp.stanford.edu/token/> ."
echo "@prefix basic: <http://nlp.stanford.edu/basic/> ."
echo "@prefix dep: <http://nlp.stanford.edu/dep/> ."
echo

sed -En '
s/^([^"][^ ]*) ([0-9]+) ([^ "\r]+)\r?$/token:begin \2 .\ntoken:text "\1" .\ntoken:pos "\3" ./p; # token info
s/^([^(]+)\((.+)-([0-9]+), (.+)-([0-9]+)\)\r?$/:\3 basic:\1 :\5 .\n\:\3 dep:\1 :\5 ./p # dependency info (duplicate as basic: and dep:)
' | \
awk '
/^token:begin 0 .$/{s++;t=0} # new sentence
/^token:begin /{t++} # new token
m=/^token:/{print ":" s+0 "." t " " $0} # add id to token info
!m{gsub(/(^:| :)/, "&" s+0 "."); print} # expand ids in non-token info
' | \
sed 's/\^//'
