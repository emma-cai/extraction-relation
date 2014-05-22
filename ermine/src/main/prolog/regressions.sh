swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('sample/sample.ttl'),findall(_,(relation(I,J),write(I),write(J),nl,nl),_),halt" > sample/sample.out

for f in sample/*.out
  do grep -E '^[;%]' $f > $f.txt
     grep -E '^(english\(|rule[0-9]+:: )' $f | gsed -E "s/^(english[(])?rule/\1sample.rule/" > $f.inf
     sed -E 's/(, )?isa\([^)]+\)//g;s/(\(|, )[EA][^-]+-/\1/g;s/:: ( -> )?(, )?/:: /;s/ -> , / -> /;s/ -> \././' $f.inf > $f.inf.simple
     grep '^{"class":' $f > $f.json
     head $f | grep '^@' > $f.ttl
     grep -E '^[<	]' $f >> $f.ttl
done


swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/ie-target.txt.rnn.ttl'),findall(_,(relation(I,J),write(I),write(J),nl,nl),_),halt" > regressions/ie-target.txt.rnn.out

swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/june2014.txt.ttl'),findall(_,(relation(I,J),write(I),write(J),nl,nl),_),halt" > regressions/june2014.txt.out

swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/barrons.txt.rnn.ttl'),findall(_,(relation(I,J),write(I),write(J),nl,nl),_),halt" > regressions/barrons.txt.rnn.out

./extract-question.sh < regressions/june2014questions.txt > regressions/june2014questions.out

for f in regressions/*.out
  do grep -E '^[;%]' $f > $f.txt
     grep -E '^(english\(|rule[0-9]+:: )' $f | gsed -E "s/^(english[(])?rule/\1`basename $f`.rule/;s/\.txt(\.rnn)?\.out//" > $f.inf
     sed -E 's/(, )?isa\([^)]+\)//g;s/(\(|, )[EA][^-]+-/\1/g;s/:: ( -> )?(, )?/:: /;s/ -> , / -> /;s/ -> \././' $f.inf > $f.inf.simple
     grep '^{"class":' $f > $f.json
     head $f | grep '^@' > $f.ttl
     grep -E '^[<	]' $f >> $f.ttl
done

