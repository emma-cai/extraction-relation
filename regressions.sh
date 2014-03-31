swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/ie-target.txt.rnn.ttl'),findall(_,relation,_),halt" > regressions/ie-target.txt.rnn.inf.new
diff -qs regressions/ie-target.txt.rnn.inf regressions/ie-target.txt.rnn.inf.new

swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/ie-target.txt.rnn.ttl'),findall(_,(relation(J),write(J),nl,nl),_),halt" > regressions/ie-target.txt.rnn.inf.new
diff -qs regressions/ie-target.txt.rnn.json regressions/ie-target.txt.rnn.json.new


swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('regressions/barrons.txt.rnn.ttl'),findall(_,relation,_),halt" > regressions/barrons.txt.rnn.inf.new
diff -qs regressions/barrons.txt.rnn.inf regressions/barrons.txt.rnn.inf.new

swipl -q -l relation.pl -g "consult('patterns-sapir.pl'),rdf_load('regressions/barrons.txt.sapir.ttl'),findall(_,relation,_),halt" > regressions/barrons.txt.sapir.new
diff -qs regressions/barrons.txt.sapir.out regressions/barrons.txt.sapir.new

