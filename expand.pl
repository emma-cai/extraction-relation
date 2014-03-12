:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).

:- rdf_register_prefix(token, 'http://nlp.stanford.edu/token/').
:- rdf_register_prefix(ne, 'http://nlp.stanford.edu/ne/').
:- rdf_register_prefix(basic, 'http://nlp.stanford.edu/basic/').
:- rdf_register_prefix(dep, 'http://nlp.stanford.edu/dep/').
:- rdf_register_prefix(coref, 'http://nlp.stanford.edu/coref/').
:- rdf_register_prefix(id, 'http://halo.vulcan.com/id#').


expand :-
	rdf(_Sentence,basic:root,Root),
	write_string(Root).

write_string(Root) :-
	tokens(Root,Tokens),
	write_tokens(Tokens),
	write('.\r\n').

tokens(Node,Sorted) :-
	tokens(Node,Sorted,[]).
tokens(Node,Sorted,Exclude) :-
	findall(Constit,constit(Node,Constit,Exclude),Constits),
	predsort(compare_offsets,[Node|Constits],Sorted).

constit(Node,Constit) :-
	constit(Node,Constit,[]).
constit(Node,Constit,Exclude) :-
	rdf(Node,Dep,C),
	atom_concat('http://nlp.stanford.edu/basic/',DepType,Dep),
	\+ member(DepType, Exclude),
	\+ token_id(C,0), % Sapir: exclude that1-0
	( Constit=C
	; constit(C,Constit) ). % filter top-level only

token_id(E1,N1) :-
	atom_concat('http://halo.vulcan.com/id#', S1, E1),
	sub_atom(S1,B1,_,_,'.'), BS1 is B1 + 1, sub_atom(S1,BS1,_,0,T1),
	atom_number(T1,N1).

compare_offsets(Delta,E1,E2) :-
	offset(E1,N1),
	offset(E2,N2),
	compare(Delta,N1,N2).

offset(E1,N1) :-
	rdf(E1,token:begin,literal(type(xsd:integer,S1))),
	atom_number(S1,N1).

coref(E,C) :-
	rdf(E,token:pos,literal(Pos)),
	member(Pos, ['PRP', 'PRP$']),
	rdf(A,coref:ref,E),
	( C = A
	; rdf(A,coref:ref,C) ),
	C \= E,
	compare_offsets(<,C,E),
	!.

write_text(E) :-
	rdf(E,token:text,literal(T)),
	write(T), !.
write_text(T) :-
	write(T).

write_token(T) :-
	coref(T,Coref),
	tokens(Coref,CorefT),
	write_text(T),
	write('['), write_tokens0(CorefT), write(']'),
	!.
write_token(T) :-
	write_text(T).

% expand coref
write_tokens([]).
write_tokens([T]) :-
	write_token(T),
	!.
write_tokens([T|Rest]) :-
	write_token(T), write(' '),
	write_tokens(Rest).

% don't expand
write_tokens0([]).
write_tokens0([T]) :-
	write_text(T),
	!.
write_tokens0([T|Rest]) :-
	write_text(T), write(' '),
	write_tokens0(Rest).


write_lemmas([]).
write_lemmas([T]) :-
	rdf(T,token:lemma,literal(L)),
	write(L),
	!.
write_lemmas([T|Rest]) :-
	write_lemmas([T]),
	write(' '),
	write_lemmas(Rest).


