while read line; do curl http://www.halotestframework.net:8003/decompose?question=$line; done | \
sed 's/Is it true that //;s/?"/."/' | \
jq '.[] | "extract(\"\(.question)\", \"\(.focus)\")."' | \
sed -E "s/'/\\\'/g;s/^\"//;s/\"$//;s/[\\]\"/'/g"
