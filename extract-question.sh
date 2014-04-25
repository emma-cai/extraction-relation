while read line
  do echo "QUESTION: $line"
    question=$(
      echo $line | \
      sed -E "s/ /%20/g;s/[(]/%28/g;s/[)]/%29/g;s/[?]/%3F/g;s/'/%27/g"
    )
    cmd=$(
      curl http://www.halotestframework.net:8003/decompose?question=$question | \
      sed 's/Is it true that //;s/?"/."/' | \
      jq '.[] | "swipl -q -l extract.pl -g \"extract(\"\(.question)\", \"\(.focus)\"),halt.\""' | \
      sed -E "s/'/\\\'/g;s/^\"//;s/\"$//;s/[\\]\"/'/g;
              s/'extract[(]/\"extract(/g;s/halt.'/halt.\";/g"
    )
  eval $cmd
done
