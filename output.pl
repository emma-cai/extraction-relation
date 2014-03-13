
write_relation(Action,Rel,Purpose) :-
	write_tuple(Action),
	write_rel(Rel),
	write_tuple(Purpose),
	nl.

write_entity_relation(Entity1,Rel,Entity2) :-
	( (atom(Entity1),
	   write_entity(Entity1))
	; write_tuple(Entity1) ),
	write_rel(Rel),
	( (atom(Entity2),
	   write_entity(Entity2))
	; write_tuple(Entity2) ),
	!, nl.

write_rel([Rel|Tokens]) :-
	write('\t"'),
	write_tokens(Tokens),
	write('"/'),
	write(Rel),
	write('\t').

write_simple_tuple(Node) :-
	tuple(Node,Tuple),
	write_tuple(Tuple),
	nl.
write_simple_tuple(_).

% normalize to verb if possible
write_tuple(Ent) :-
	atom(Ent),
	rdf(Ent,token:lemma,literal(Lemma)),
	( (wn-denom(Lemma,Verb), !)
	; (rdf(Ent,token:pos,literal('VBG')), Verb = Lemma) ),
	argument(Ent,dep:prep_of,Obj),
	argument(Ent,dep:prep_by,Subj),
	write('('),
	write_arg(Subj),
	write(' '),
	write(Verb),
	write(' '),
	write_arg(Obj),
	write(')'),
	!.
write_tuple(Ent) :-
	atom(Ent), !,
	write_arg(Ent).

write_tuple([S,V]) :-
	write_tuple([S,V,[]]).
write_tuple([S,V,Arg|Mods]) :-
	write('('),
	write_arg(S),
	write(' '),
	write_verb(V),
	write(' '),
	( (rdf(_,basic:cop,V),
	   write_verb(Arg)) % copula
	; write_arg(Arg) ), % dobj
	( Mods = []
	; (write(' [ '),
	   write_mods(Mods),
	   write(' ] ')) ),
	write(')'), !.

write_arg([]) :- !,
	write('""').
write_arg(Arg-Var) :- !,
	write_arg(Arg),
	write('/?'),
	write(Var).
% special case for prep to apply exclusion list to pobj
write_arg(Arg) :-
	rdf(_,basic:prep,Arg),
	( rdf(Arg,basic:pobj,Obj)
	; rdf(Arg,basic:pcomp,Obj)), !,
	write('"'),
	write_token(Arg), write(' '),
	write_arg(Obj),
	write('"').
write_arg(Arg) :-
	tokens(Arg,Tokens,[conj,cc,appos,dep,xcomp,infmod,rcmod,partmod,advmod,cop,nsubj,aux,ref]),
	write('"'),
	write_tokens(Tokens),
	write('"').

write_entity(Arg) :-
	% remove 'such as' PP
	rdf(Arg,dep:prep_such_as,Pobj),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Prep,basic:mwe,As), !,
	tokens(Pobj,PobjTokens,[]),
	tokens(Arg,ArgTokens,[conj,cc,appos,xcomp,advmod,rcmod,partmod,cop,nsubj,aux]),
	subtract(ArgTokens,[Prep,As|PobjTokens],Tokens),
	write('"'),
	write_tokens(Tokens),
	write('"').
write_entity(Arg) :-
	tokens(Arg,Tokens,[conj,cc,appos,xcomp,advmod,rcmod,partmod,cop,nsubj,aux]),
	write('"'),
	write_tokens(Tokens),
	write('"').

write_mod([]) :- !.
% special case for prep to apply exclusion list to pobj
write_mod(Mod) :-
	rdf(_,basic:prep,Mod),
	( rdf(Mod,basic:pobj,Obj)
	; rdf(Mod,basic:pcomp,Obj)), !,
	write_token(Mod), write(' '),
	write_mod(Obj).
write_mod(Mod) :-
	tokens(Mod,Tokens,[conj,cc,appos,xcomp,infmod,rcmod]),
	write_tokens(Tokens).

write_verb([]) :- !.
write_verb(Arg) :-
	rdf(Arg,token:lemma,literal(Lemma)),
	wn-denom(Lemma,Verb),
	write('"'),
	write(Verb),
	write('"'),
	!.
write_verb(Arg) :-
	tokens(Arg,Tokens,[aux,auxpass,nsubj,nsubjpass,csubj,csubjpass,dobj,iobj,xcomp,prep,conj,cc,mark,advcl,advmod,npadvmod,tmod,acomp,dep,ccomp,cop,expl,attr,xsubj,purpcl]),
	write('"'),
	write_lemmas(Tokens),
	write('"').

write_mods([]).
write_mods([Arg]) :- !,
	write('"'),
	write_mod(Arg),
	write('"').
write_mods([Arg|Rest]) :-
	write_mods([Arg]),
	write(', '),
	write_mods(Rest).


write_sentence(Root) :-	
	tokens(Root,Tokens),
	write(';;; '),
	write_tokens(Tokens),
	write('.\n').
