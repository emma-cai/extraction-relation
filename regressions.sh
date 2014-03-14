swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/barrons.txt.rnn.ttl'),findall(_,relation,_),halt" > regressions/barrons.txt.rnn.new
diff -qs regressions/barrons.txt.rnn.out regressions/barrons.txt.rnn.new

swipl -q -l relation.pl -g "consult('patterns-sapir.pl'),rdf_load('regressions/barrons.txt.sapir.ttl'),findall(_,relation,_),halt" > regressions/barrons.txt.sapir.new
diff -qs regressions/barrons.txt.sapir.out regressions/barrons.txt.sapir.new

#swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/ie-target.txt.rnn.ttl'),findall(_,relation,_),halt" > regressions/barrons.txt.rnn.new
#diff -qs regressions/ie-target.txt.rnn.out regressions/ie-target.txt.rnn.new

