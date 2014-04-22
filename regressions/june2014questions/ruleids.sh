for f in june2014.q*.out; do gsed "s/rule/$f.rule/" $f; done | gsed 's/\.txt\.xml\.ttl\.out//' | grep -E '^(%|english|june2014)'
