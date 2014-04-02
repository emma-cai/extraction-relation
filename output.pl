:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_meta write_rdf(r,r,-,-).

:- rdf_register_prefix(pred, 'http://aristo.allenai.org/pred/').


write_relation(Action,Rel,Purpose,Json) :-
	text_relation(Action,Rel,Purpose,Text),
	format(current_output, '% ~w\t~w\t~w', Text), nl,
	write_inf_relation(Action,Rel,Purpose), nl,
	json_relation(Action,Rel,Purpose,Json).
%	json:json_write(current_output,Json), nl.

text_relation(Action,Rel,Purpose,[ActionText,RelText,PurposeText]) :-
	text_tuple(Action,ActionText),
	text_rel(Rel,RelText),
	text_tuple(Purpose,PurposeText).

json_relation(Action,Rel,Purpose,json([class='ExtractionRule',antecedents=[ActionJson],relation=RelJson,consequents=[PurposeJson]])) :-
	json_tuple(Action,ActionJson),
	json_rel(Rel,RelJson),
	json_tuple(Purpose,PurposeJson).

write_entity_relation(Action,Rel,Purpose,Json) :-
	text_entity_relation(Action,Rel,Purpose,Text),
	format(current_output, '% ~w\t~w\t~w', Text), nl,
	write_inf_relation(Action,Rel,Purpose), nl,
	json_entity_relation(Action,Rel,Purpose,Json).
%	json:json_write(current_output,Json), nl.

text_entity_relation(Entity1,Rel,Entity2,[Entity1Text,RelText,Entity2Text]) :-
	( (atom(Entity1),
	   text_entity(Entity1,Entity1Text))
	; text_tuple(Entity1,Entity1Text) ),
	text_rel(Rel,RelText),
	( (atom(Entity2),
	   text_entity(Entity2,Entity2Text))
	; text_tuple(Entity2,Entity2Text) ),
	!.

json_entity_relation(Entity1,Rel,Entity2,json([class='ExtractionRule',antecedents=[Json1],relation=RelJson,consequents=[Json2]])) :-
	( (atom(Entity1),
	   json_entity(Entity1,Json1))
	; json_tuple(Entity1,Json1) ),
	json_rel(Rel,RelJson),
	( (atom(Entity2),
	   json_entity(Entity2,Json2))
	; json_tuple(Entity2,Json2) ),
	!.

% comment to write rule ids
text_rel([Rel-_Id|Tokens],Text) :- !,
	text_rel([Rel|Tokens],Text).
text_rel([Rel|Tokens],Text) :-
	subtract(Tokens,[[]],Tokens2),
	tokens_text_quoted(Tokens2,TokensText),
	format(atom(Text), '~w/~w', [TokensText,Rel]).

json_rel([Rel-_Id|Tokens],Json) :- !,
	json_rel([Rel|Tokens],Json).
json_rel([Rel|Tokens],json([class='Relation',string=TokenIds,normalizedRelation=Rel])) :-
	prefixed_ids(Tokens,TokenIds).

write_simple_tuple(Node,Json) :-
	tuple(Node,Tuple),
	text_tuple(Tuple,Text),
	write('% '), write(Text), nl,
	write_inf_simple_tuple(Tuple),
	json_tuple(Tuple,Json).
%	json:json_write(current_output,Json), nl.


write_inf_relation(Action,[Rel-_|_],Purpose) :-
	inf_tuple(Action,ActionId),
	inf_tuple(Purpose,PurposeId),
	stripped_ids([ActionId, PurposeId], Ids),
	downcase_atom(Rel,LRel),
	% left to right
	write_inf_relation0(ActionId,[LRel|Ids],PurposeId),
	% right to left
	write_inf_relation0(PurposeId,[LRel|Ids],ActionId),
	rdf_unload_graph(ActionId),
	rdf_unload_graph(PurposeId).

write_inf_relation0(Left,Rel,Right) :-
	write_inf_tuple(Left,[],LeftTriples),
	format(' -> ~w(~w, ~w), ', Rel),
	write_inf_tuple(Right,LeftTriples,_),
	write('.'), nl.


%write_inf_tuple(GraphId,Remove,Out) :-
%	rdf_save_turtle(stream(current_output),[graph(GraphId),silent(true)]),
%	fail.
write_inf_tuple(GraphId,Remove,Triples) :-
	findall([S,P,O], rdf(S,P,O,GraphId), Ts),
	subtract(Ts,Remove,Triples),
	write_rdf_list(Triples,GraphId).

write_inf_simple_tuple(Tuple) :-
	inf_tuple(Tuple,Id),
	write_inf_simple_tuple0(Id),
	rdf_unload_graph(Id).


%write_inf_simple_tuple0(GraphId) :-
%	rdf_save_turtle(stream(current_output),[graph(GraphId),silent(true)]),
%	fail.
write_inf_simple_tuple0(GraphId) :-
	rdf(S,pred:isa,O,GraphId),
	% write first
	write_rdf(S,pred:isa,O,GraphId),
	write(' -> '),
	% write rest
	write_inf_tuple(GraphId,[[S,_,O]],_),
	write('.'), nl,
	fail.
write_inf_simple_tuple0(_) :-
	nl.

% comma separated list
write_rdf_list([],_) :- !.
write_rdf_list([[S,P,O]],GraphId) :-
	write_rdf(S,P,O,GraphId),
	!.
write_rdf_list([[S,P,O]|Rest],GraphId) :-
	write_rdf(S,P,O,GraphId),
	write(', '),
	write_rdf_list(Rest,GraphId).

write_rdf(S,P,O,GraphId) :-
	rdf(S,P,O,GraphId),
	(O = literal(Val); Val = O),
	stripped_ids([P,S,Val],Ids),
	format('~w(~w, ~w)',Ids), !.


inf_tuple(Ent,Ent) :-
	atom(Ent), !,
	text_arg(Ent,Text),
	rdf_assert(Ent,pred:isa,Text,Ent).
inf_tuple([S,V,O-_|Rest],Id) :- !, % ignore vars
	inf_tuple([S,V,O|Rest],Id).
inf_tuple([S-_,V|Rest],Id) :- !, % ignore vars
	inf_tuple([S,V|Rest],Id).
inf_tuple([S,V],Id) :-
	inf_tuple([S,V,[]],Id).
inf_tuple([S,Verb,Arg|Mods],V) :-
	text_arg(S,SubjText),
	text_verb(Verb,VerbText,V),
	( (rdf(_,basic:cop,V),
	   text_verb(Arg,ObjText,_)) % copula
	; text_arg(Arg,ObjText) ), % dobj
	rdf_assert(V,pred:isa,literal(VerbText),V),
	(S = [] ; rdf_assert(S,pred:isa,literal(SubjText),V)),
	(Arg = [] ; rdf_assert(Arg,pred:isa,literal(ObjText),V)),
	(S = [] ; rdf_assert(V,pred:agent,S,V)),
	(Arg = [] ; rdf_assert(V,pred:object,Arg,V)),
	inf_mods(V,Mods),
	!.

inf_mods(_V, []) :- !.
inf_mods(V, [Prep|Rest]) :-
	prep(Mod,Prep),
	rdf(V,PrepRel,Mod),
	atom_concat('http://nlp.stanford.edu/dep/prep_',P,PrepRel),
	atom_concat('http://aristo.allenai.org/pred/',P,NewPrepRel),
	!,
	text_mod(Mod, Text),
	rdf_assert(V,NewPrepRel,Text,V),
	inf_mods(V, Rest).
inf_mods(V, [Mod|Rest]) :-
	text_mod(Mod, Text),
	rdf_assert(V,pred:arg,Text,V),
	inf_mods(V, Rest).

text_tuple(Ent,Text) :-
	atom(Ent), !,
	text_arg(Ent,Text).
text_tuple([S,V],Text) :-
	text_tuple([S,V,[]],Text).
text_tuple([S,Verb,Arg|Mods],Text) :-
	text_arg(S,SubjText),
	text_verb(Verb,VerbText,V),
	( (rdf(_,basic:cop,V),
	   text_verb(Arg,ObjText,_)) % copula
	; text_arg(Arg,ObjText) ), % dobj
	( (Mods = [],
	   format(atom(Text), '(~w ~w ~w)', [SubjText, VerbText, ObjText]))
	; (text_mods(Mods,ModsList),
	   atomic_list_concat(ModsList,ModsText),
	   format(atom(Text), '(~w ~w ~w [ ~w ] )', [SubjText, VerbText, ObjText, ModsText])) ),
	!.


json_tuple(Ent,json([class='ExtractionTuple',confidence='1.0',subject=Json])) :-
	atom(Ent), !,
	json_arg(Ent,Json).
json_tuple([S,V],Json) :-
	json_tuple([S,V,[]],Json).
json_tuple([S,Verb,Arg|Mods],json([class='ExtractionTuple',confidence='1.0',subject=SubjJson,verbPhrase=VerbTokenIds|RestJson])) :-
	json_arg(S,SubjJson),
	json_verb(Verb,VerbTokenIds,V),
	( (rdf(_,basic:cop,V),
	   json_verb(Arg,ArgTokenIds,_), % copula
	   ObjJson=json([class='Other',string=ArgTokenIds]))
	; json_arg(Arg,ObjJson) ), % dobj
	json_mods(Mods,ModsTokens),
	ExtraPhrases = [extraPhrases=ModsTokens],
	( (ObjJson = json([]),
	   RestJson = ExtraPhrases)
	; (RestJson = [directObject=ObjJson|ExtraPhrases]) ),
	!.


text_arg([],'""') :- !.
text_arg(Arg-Var-true,Text) :- !,
	text_arg(Arg-Var,Text).
text_arg(Arg-Var,Text) :- !,
	text_arg(Arg,ArgText),
	format(atom(Text), '~w/?~w', [ArgText, Var]).
text_arg(Arg,Text) :-
	arg_tokens(Arg,Tokens),
	tokens_text_quoted(Tokens,Text).

json_arg([],json([])) :- !.
json_arg(Arg-Var-true,json([class=Class,string=[],isInferred= @(true),
			    coreferences=[json([class='Coreference',label=Var,sourceTokens=TokenIds])]])) :- !,
	json_arg(Arg,json([class=Class,string=TokenIds|_])).
json_arg(Arg-Var,json([class=Class,string=TokenIds,isInferred= @(false),
		       coreferences=[json([class='Coreference',label=Var])]])) :- !,
	json_arg(Arg,json([class=Class,string=TokenIds|_])).
json_arg(Arg,json([class='NounPhrase',string=TokenIds,isInferred= @(false),
		   coreferences=[]])) :-
	arg_tokens(Arg,Tokens),
	prefixed_ids(Tokens,TokenIds).

% special case for prep to apply exclusion list to pobj
arg_tokens(Arg,[Arg|Tokens]) :-
	rdf(_,basic:prep,Arg),
	( rdf(Arg,basic:pobj,Obj)
	; rdf(Arg,basic:pcomp,Obj)), !,
	arg_tokens(Obj,Tokens).
arg_tokens(Arg,Tokens) :-
	tokens(Arg,Tokens,[conj,cc,appos,dep,xcomp,infmod,rcmod,partmod,advmod,cop,nsubj,aux,ref]).


text_entity(Arg,Text) :-
	entity_tokens(Arg,Tokens),
	tokens_text_quoted(Tokens,Text).

json_entity(Arg,json([class='ExtractionTuple',confidence='1.0',subject=json([class='NounPhrase',string=TokenIds]),verbPhrase=[]])) :-
	entity_tokens(Arg,Tokens),
	prefixed_ids(Tokens,TokenIds).

entity_tokens(Arg,Tokens) :-
	% remove 'such as' PP
	rdf(Arg,dep:prep_such_as,Pobj),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Prep,basic:mwe,As), !,
	tokens(Pobj,PobjTokens,[]),
	tokens(Arg,ArgTokens,[conj,cc,appos,xcomp,advmod,rcmod,partmod,cop,nsubj,aux]),
	subtract(ArgTokens,[Prep,As|PobjTokens],Tokens).
entity_tokens(Arg,Tokens) :-
	tokens(Arg,Tokens,[conj,cc,appos,xcomp,advmod,rcmod,partmod,cop,nsubj,aux]).


text_mods([],[]).
text_mods([Arg],[Text]) :- !,
	text_mod(Arg,Text).
text_mods([Arg|Rest],[ArgText,', '|RestText]) :-
	text_mod(Arg,ArgText),
	text_mods(Rest,RestText).

text_mod(Mod,Text) :-
	mod_tokens(Mod,Tokens),
	tokens_text_quoted(Tokens,Text).


json_mods([],[]).
json_mods([Arg|Rest],[TokenIds|RestTokenIds]) :-
	json_mod(Arg,TokenIds),
	json_mods(Rest,RestTokenIds).

json_mod(Mod,TokenIds) :-
	mod_tokens(Mod,Tokens),
	prefixed_ids(Tokens,TokenIds).


mod_tokens([],[]) :- !.
% special case for prep to apply exclusion list to pobj
mod_tokens(Mod,[Mod|ObjTokens]) :-
	rdf(_,basic:prep,Mod),
	( rdf(Mod,basic:pobj,Obj)
	; rdf(Mod,basic:pcomp,Obj)), !,
	mod_tokens(Obj,ObjTokens).
mod_tokens(Mod,Tokens) :-
	tokens(Mod,Tokens,[conj,cc,appos,xcomp,infmod,rcmod]).


text_verb(Arg-Norm,Text,Arg) :- !, % denominalized
	format(atom(Text), '"~w"', [Norm]).
text_verb(Arg,Text,Arg) :-
	verb_tokens(Arg,Tokens),
	lemmas_text_quoted(Tokens,Text).

json_verb(Arg-Norm,Norm,Arg) :- !. % denominalized
json_verb(Arg,TokenIds,Arg) :-
	verb_tokens(Arg,Tokens),
	prefixed_ids(Tokens,TokenIds).

verb_tokens([],[]) :- !.
verb_tokens(Arg,Verb) :-
	rdf(Arg,token:lemma,literal(Lemma)),
	wn-denom(Lemma,Verb),
	!.
verb_tokens(Arg,Tokens) :-
	tokens(Arg,Tokens,[aux,auxpass,nsubj,nsubjpass,csubj,csubjpass,dobj,iobj,xcomp,prep,conj,cc,mark,advcl,advmod,npadvmod,tmod,acomp,dep,ccomp,cop,expl,attr,xsubj,purpcl]).


write_sentence(Root) :-	
	tokens(Root,Tokens),
	write(';;; '),
	write_tokens(Tokens),
	write('.\n').


prefixed_ids(Tokens,TokenIds) :-
	maplist(prefixed_id,Tokens,TokenIds), !.
prefixed_id(Token,TokenId) :-
	rdf_global_id(id:Val,Token),
	atomic_list_concat([id,':',Val],TokenId).
prefixed_id(Token,TokenId) :-
	rdf_global_id(Term,Token),
	term_to_atom(Term,TokenId).

stripped_ids(Tokens,TokenIds) :-
	maplist(stripped_id,Tokens,TokenIds),
	!.
stripped_id(Token,TokenId) :-
	rdf_global_id(id:_,Token),
	token_id(Token,Id),
	atomic_list_concat(['E',Id],TokenId).
stripped_id(Token,TokenId) :-
	rdf_global_id(_:TokenId,Token).
stripped_id(Token,Token). % literal
