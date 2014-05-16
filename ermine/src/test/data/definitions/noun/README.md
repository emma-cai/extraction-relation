# Noun Definition Extraction Patterns

This directory contains the rules enocding patterns to perform information extraction from definitions of noun terms from targeted definition sources using OpenRegex.

## Files

Each of the 'taggers' files contains rules for a phrase with a particular part of speech as head. For e.g., 'noun.taggers' contains patterns to parse noun-phrases. 
The cascade file, defn.cascade defines the cascade FST that chains the outputs from the list of taggers in the specified order.
Sometimes there are multiple taggers files per phrase type, like noun2.taggers and noun3.taggers appearing in the chain after the initial noun.taggers, because we need to define patterns that reference rules defined in other intermediate taggers files. For.e.g, noun2.taggers references rules defined in prep.taggers, which appears in the cascade after noun.taggers because it references rules (describing simpler noun phrase patterns) in noun.taggers. In this manner, the sequence of taggers specified in the cascade is crucial.
rel.taggers identifies some common relations like modals and 'to' relations, while others are more-or-less self-explanatory based on their names.
sent.taggers is the topmost level taggers file that defines the high-level patterns for a given sentence in the supported definition formats.
