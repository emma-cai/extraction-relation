sed -Ef barrons.sent_ids.sed barrons.txt.rnn.out.inf | grep -Eo '^[^:]+::' | sed -E 's|([^.]+\.)([^.]+\.)([^:]+)::|s/\1\3([:,])/\1\2\3\\1/|' > barrons.rule_ids.sed
sed -Ef barrons.rule_ids.sed barrons.txt.rnn.out.inf > barrons.txt.rnn.out.inf.ids
rm barrons.rule_ids.sed
