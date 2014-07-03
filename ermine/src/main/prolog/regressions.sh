# Parsed TTL files.
# Should be generated from the Ermine pipeline regression_inputs.conf.
INFILES=(sample/sample.ttl regressions/ie-target.txt.rnn.ttl
  regressions/june2014.txt.ttl regressions/barrons.txt.rnn.ttl)

for INFILE in ${INFILES[@]}; do
  RAW_OUT="${INFILE%.*}.out"
  swipl -q -l relation.pl -g "consult('patterns-stanford.pl'),rdf_load('$INFILE'),findall(_,(relation(I),write(I),nl,nl),_),halt" > $RAW_OUT
  # Textual representation of extraction tuples. Lines start with %.
  grep -E '^[;%]' $RAW_OUT > $RAW_OUT.txt

  INFERENCE="$RAW_OUT.inf"
  # Inference format. Lines start with "english(", "pretty(", or a rule name.
  grep -E '^(english\(|pretty\(|[^ ]*[.]rule[0-9]+:: )' $RAW_OUT > $INFERENCE
  # "Simple" inference format. Unsure what this is.
  sed -E 's/(, )?isa\([^)]+\)//g;s/(\(|, )[EA][^-]+-/\1/g;s/:: ( -> )?(, )?/:: /;s/ -> , / -> /;s/ -> \././' $INFERENCE | grep -v '^pretty(' > $INFERENCE.simple

  # Turtle file containing the extraction graph.
  TURTLE="$RAW_OUT.ttl"
  # First, any @prefix defs at the start of the file.
  head $RAW_OUT | grep '^@' > $TURTLE
  # Turtle lines start with a URI literal beginning with "<", or a continuation
  # line (blank space).
  grep -E '^[<	]' $RAW_OUT >> $TURTLE
done

# TODO: Add question regression back in?
