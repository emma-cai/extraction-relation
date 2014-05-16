# Noun Definition Extraction Patterns

This directory contains the rules enocding patterns to perform information extraction from definitions of noun terms from targeted definition sources using OpenRegex.

## Files

Each of the 'taggers' files contains rules for a phrase with a particular part of speech as head. For e.g., 'noun.taggers' contains patterns to parse noun-phrases. 
The cascade file, defn.cascade defines the cascade FST that chains the outputs from the list of taggers in the specified order.
Sometimes there are multiple taggers files per phrase type, like noun2.taggers and noun3.taggers appearing in the chain after the initial noun.taggers, because we need to define patterns that reference rules defined in other intermediate taggers files. For.e.g, noun2.taggers references rules defined in prep.taggers, which appears in the cascade after noun.taggers because it references rules (describing simpler noun phrase patterns) in noun.taggers. In this manner, the sequence of taggers specified in the cascade is crucial.
rel.taggers identifies some common relations like modals and 'to' relations, while others are more-or-less self-explanatory based on their names.

sent.taggers is the topmost level taggers file that defines the high-level patterns for a given sentence in the supported definition formats.

The highest level rules (in sent.taggers) cover the most commonly occuring patterns in noun definitions, namely-

Definition1_1 - Definition1_3:

These differ only in the type of verb phrase following the relative (that/which) clause, namely, simple/compund (with antecedent-consequent)/composite.
They cover patterns of the form-
<Term> is/are/refers to <NP> that/, which {<VP1>, <VP2>,… : implied subject}
<Term> : <NP> that/, which {<VP1>, <VP2>,… : implied subject}

E.g.:
The ear is the organ that detects sound.
Ear: The organ that detects sound.


Definition2_2_1 - Definition2_2_2:

These differ only in the type of sentence following the relative clause, namely, simple/compound sentence.
They cover patterns of the form-
<Term> is/are/refers to <NP> <PP> which <sentence>
<Term> : <NP> <PP> which <sentence>

E.g.:
Freezing is a phase change in which a liquid turns into a solid when its temperature is lowered below its freezing point.
Freezing : A phase change in which a liquid turns into a solid when its temperature is lowered below its freezing point


Definition2_1_1 - Definition2_1_2:

These differ only in the type of sentence following the relative clause, namely, simple/compound sentence.
They cover patterns of the form-
<Term> is/are/refers to <NP>  where/whereby <sentence>
<Term> : <NP> where/whereby <sentence>

E.g.:
A drought is an extended period of months or years when a region notes a deficiency in its water supply whether surface or underground water.


Definition3_1 - Definition3_3:

These differ only in the type of verb phrase following the relative (that/which) clause, namely, simple/compund (with antecedent-consequent)/composite.
They cover patterns of the form-
<Term> is/are/refers to <NP>  {<VP1>, VP2>,… : implied subject}
<Term> : <NP>  {<VP1>, <VP2>,… : implied subject}

E.g.:
Chlorophyll is a green pigment found in almost all plants, algae, and cyanobacteria.


Definition4_1 - Definition4_2:

These cover patterns of the form-
<Term> is/are/refers to {<NP1>, <NP2>,…}

E.g.:
Earth is the third planet from the Sun, and the densest and fifth-largest of the eight planets in the Solar System.


Definition5_1 - Definition5_2:

These cover patterns of the form-

<Term> is when/where <sentence>

E.g.:
An 'agreement' is where everybody has the same feelings about something.



