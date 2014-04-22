:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).

:- rdf_register_prefix(token, 'http://nlp.stanford.edu/token/').
:- rdf_register_prefix(ne, 'http://nlp.stanford.edu/ne/').
:- rdf_register_prefix(basic, 'http://nlp.stanford.edu/basic/').
:- rdf_register_prefix(dep, 'http://nlp.stanford.edu/dep/').
:- rdf_register_prefix(coref, 'http://nlp.stanford.edu/coref/').
:- rdf_register_prefix(id, 'http://aristo.allenai.org/id#').


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
	\+ token_number(C,0), % Sapir: exclude that1-0
	( Constit=C
	; constit(C,Constit) ). % filter top-level only

token_number(E1,N1) :-
	atom_concat('http://aristo.allenai.org/id#', S1, E1),
	sub_atom(S1,B1,_,_,'.'), BS1 is B1 + 1, sub_atom(S1,BS1,_,0,T1),
	atom_number(T1,N1).

token_id(E1,N1) :-
	atom_concat('http://aristo.allenai.org/id#', S1, E1),
	sub_atom(S1,B1,_,_,'.'), BS1 is B1 + 1,
	sub_atom(S1,BS1,_,0,TokenId),
	sub_atom(S1,0,B1,_,SentId),
	atomic_list_concat([TokenId,'S',SentId],N1).

compare_offsets(Delta,E1,E2) :-
	offset(E1,N1),
	offset(E2,N2),
	compare(Delta,N1,N2).

offset(E1,N1) :-
	rdf(E1,token:begin,literal(type(xsd:integer,S1))),
	atom_number(S1,N1).

coref(E,C) :-
	rdf(E,token:pos,literal(Pos)),
%	member(Pos, ['PRP', 'PRP$']),
	rdf(A,coref:ref,E),
	( C = A
	; rdf(A,coref:ref,C) ),
	C \= E,
	compare_offsets(<,C,E),
	!.

% expand coref
token_text([],[]) :- !.
token_text(_Token-Text,Text) :- !.
token_text(Token,Text) :-
	coref(Token,Coref),
	token_text0(Token,TokenText),
	tokens(Coref,CorefTokens),
	tokens_text0(CorefTokens,CorefText),
	format(atom(Text), '~w[~w]', [TokenText, CorefText]),
	!.
token_text(T,Text) :-
	token_text0(T,Text).

tokens_text_list([],[]).
tokens_text_list([Token],[Text]) :-
	token_text(Token,Text),
	!.
tokens_text_list([Token|Rest],[TokenText,' '|RestText]) :-
	token_text(Token,TokenText),
	tokens_text_list(Rest,RestText).

tokens_text(Tokens,Text) :-
	tokens_text_list(Tokens,List),
	atomic_list_concat(List,Text).
tokens_text_quoted(Tokens,QuotedText) :-
	tokens_text(Tokens,Text),
	format(atom(QuotedText), '"~w"', [Text]).

write_tokens(Tokens) :-
	tokens_text(Tokens,Text),
	write(Text).

% don't expand
token_text0(Token,Text) :-
	rdf(Token,token:text,literal(Text)),
	!.
token_text0(Text,Text).

tokens_text0_list([],[]).
tokens_text0_list([Token],[Text]) :-
	token_text0(Token,Text),
	!.
tokens_text0_list([Token|Rest],[TokenText,' '|RestText]) :-
	token_text0(Token,TokenText),
	tokens_text0_list(Rest,RestText).

tokens_text0(Tokens,Text) :-
	tokens_text0_list(Tokens,List),
	atomic_list_concat(List,Text).

write_tokens0(Tokens) :-
	tokens_text0(Tokens,Text),
	write(Text).

% lemmas if available
lemma_text(_T-L,L) :- !.
lemma_text(T,L) :-
	rdf(T,token:lemma,literal(L)),
	!.
lemma_text(T,L) :-
	rdf(T,token:text,literal(L)). % allow for missing lemmas in Sapir

lemmas_text_list([],[]).
lemmas_text_list([T],[L]) :-
	lemma_text(T,L),
	!.
lemmas_text_list([T],[T]) :- !. % not a token
lemmas_text_list([T|Rest],[Text,' '|RestText]) :-
	lemma_text(T,Text),
	lemmas_text_list(Rest,RestText).

lemmas_text(Tokens,Text) :-
	lemmas_text_list(Tokens,List),
	atomic_list_concat(List,Text).
lemmas_text_quoted(Tokens,QuotedText) :-
	lemmas_text(Tokens,Text),
	format(atom(QuotedText), '"~w"', [Text]).

write_lemmas(Lemmas) :-
	lemmas_text(Lemmas,Text),
	write(Text).
