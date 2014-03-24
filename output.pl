:- use_module(library(http/http_json)).

write_relation(Action,Rel,Purpose) :-
	nl, write(';;; '),
	write_relation(Action,Rel,Purpose,Json),
	json:json_write(user_output,Json), nl.

write_relation(Action,Rel,Purpose,json([class='ExtractionRule',antecedents=[ActionJson],relation=RelJson,consequents=[PurposeJson]])) :-
	write_tuple(Action,ActionJson),
	write_rel(Rel,RelJson),
	write_tuple(Purpose,PurposeJson), nl.


write_entity_relation(Action,Rel,Purpose) :-
	nl, write(';;; '),
	write_entity_relation(Action,Rel,Purpose,Json),
	json:json_write(user_output,Json), nl.

write_entity_relation(Entity1,Rel,Entity2,json([class='ExtractionRule',antecedents=[Json1],relation=RelJson,consequents=[Json2]])) :-
	( (atom(Entity1),
	   write_entity(Entity1,Json1))
	; write_tuple(Entity1,Json1) ),
	write_rel(Rel,RelJson),
	( (atom(Entity2),
	   write_entity(Entity2,Json2))
	; write_tuple(Entity2,Json2) ),
	!, nl.

write_rel([Rel-_Id|Tokens],Json) :- !,
	write_rel([Rel|Tokens],Json).
% write rule Ids (comment out previous clause)
write_rel([Rel-Id|Tokens],json([class='Relation',string=Tokens,normalizedRelation=Rel])) :- !,
	write('\tRULE-'),
	write(Id),
	write(':"'),
	write_tokens(Tokens),
	write('"/'),
	write(Rel),
	write('\t').
write_rel([Rel|Tokens],json([class='Relation',string=Tokens,normalizedRelation=Rel])) :-
	write('\t"'),
	write_tokens(Tokens),
	write('"/'),
	write(Rel),
	write('\t').

write_simple_tuple(Node) :-
	tuple(Node,Tuple),
	nl, write(';;; '),
	write_tuple(Tuple,Json), nl,
	json:json_write(user_output,Json), nl.
write_simple_tuple(_).


% normalize to verb if possible
write_tuple(Ent,json([class='ExtractionTuple',subject=SubjJson,verbPhrase=Verb,directObject=ObjJson])) :-
	atom(Ent),
	rdf(Ent,token:lemma,literal(Lemma)),
	( (wn-denom(Lemma,Verb), !)
	; (rdf(Ent,token:pos,literal('VBG')), Verb = Lemma) ),
	argument(Ent,dep:prep_of,Obj),
	argument(Ent,dep:prep_by,Subj),
	write('('),
	write_arg(Subj,SubjJson),
	write(' '),
	write(Verb),
	write(' '),
	write_arg(Obj,ObjJson),
	write(')'),
	!.
write_tuple(Ent,Json) :-
	atom(Ent), !,
	write_arg(Ent,Json).

write_tuple([S,V],Json) :-
	write_tuple([S,V,[]],Json).
write_tuple([S,V,Arg|Mods],json([class='ExtractionTuple',subject=SubjJson,verbPhrase=VerbTokens,directObject=ObjJson|ExtraPhrases])) :-
	write('('),
	write_arg(S,SubjJson),
	write(' '),
	write_verb(V,VerbTokens),
	write(' '),
	( (rdf(_,basic:cop,V),
	   write_verb(Arg,ArgTokens), % copula
	   ObjJson=json([class='Other',string=ArgTokens]))
	; write_arg(Arg,ObjJson) ), % dobj
	( (Mods = [],
	   ExtraPhrases = [])
	; (write(' [ '),
	   write_mods(Mods,ModsTokens),
	   ExtraPhrases = [extraPhrases=ModsTokens],
	   write(' ] ')) ),
	write(')'), !.

write_arg([],json([])) :- !,
	write('""').
write_arg(Arg-Var,json([class=Class,string=String,coreferences=Var])) :- !,
	write_arg(Arg,json([class=Class,string=String])),
	write('/?'),
	write(Var).
% special case for prep to apply exclusion list to pobj
write_arg(Arg,Json) :-
	rdf(_,basic:prep,Arg),
	( rdf(Arg,basic:pobj,Obj)
	; rdf(Arg,basic:pcomp,Obj)), !,
	write('"'),
	write_token(Arg), write(' '),
	write_arg(Obj,Json),
	write('"').
write_arg(Arg,json([class='NounPhrase',string=Tokens])) :-
	tokens(Arg,Tokens,[conj,cc,appos,dep,xcomp,infmod,rcmod,partmod,advmod,cop,nsubj,aux,ref]),
	write('"'),
	write_tokens(Tokens),
	write('"').

write_entity(Arg,json([class='NounPhrase',string=Tokens])) :-
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
write_entity(Arg,json([class='NounPhrase',string=Tokens])) :-
	tokens(Arg,Tokens,[conj,cc,appos,xcomp,advmod,rcmod,partmod,cop,nsubj,aux]),
	write('"'),
	write_tokens(Tokens),
	write('"').

write_mod([],[]) :- !.
% special case for prep to apply exclusion list to pobj
write_mod(Mod,[Mod|ObjTokens]) :-
	rdf(_,basic:prep,Mod),
	( rdf(Mod,basic:pobj,Obj)
	; rdf(Mod,basic:pcomp,Obj)), !,
	write_token(Mod), write(' '),
	write_mod(Obj,ObjTokens).
write_mod(Mod,Tokens) :-
	tokens(Mod,Tokens,[conj,cc,appos,xcomp,infmod,rcmod]),
	write_tokens(Tokens).

write_verb([],[]) :- !.
write_verb(Arg,Verb) :-
	rdf(Arg,token:lemma,literal(Lemma)),
	wn-denom(Lemma,Verb),
	write('"'),
	write(Verb),
	write('"'),
	!.
write_verb(Arg,Tokens) :-
	tokens(Arg,Tokens,[aux,auxpass,nsubj,nsubjpass,csubj,csubjpass,dobj,iobj,xcomp,prep,conj,cc,mark,advcl,advmod,npadvmod,tmod,acomp,dep,ccomp,cop,expl,attr,xsubj,purpcl]),
	write('"'),
	write_lemmas(Tokens),
	write('"').

write_mods([],[]).
write_mods([Arg],[Tokens]) :- !,
	write('"'),
	write_mod(Arg,Tokens),
	write('"').
write_mods([Arg|Rest],[Tokens|RestTokens]) :-
	write_mods([Arg],[Tokens]),
	write(', '),
	write_mods(Rest,RestTokens).


write_sentence(Root) :-	
	tokens(Root,Tokens),
	write(';;; '),
	write_tokens(Tokens),
	write('.\n').
