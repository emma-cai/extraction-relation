:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_meta write_rdf(r,r,-,-).

:- rdf_register_prefix(pred, 'http://aristo.allenai.org/pred/').


write_relation(Action,Rel,Purpose) :-
	text_relation(Action,Rel,Purpose,Text),
	format(user_output, '~w\t~w\t~w', Text), nl,
	write_inf_relation(Action,Rel,Purpose).
%	write_json_relation(Action,Rel,Purpose).

write_json_relation(Action,Rel,Purpose) :-
	json_relation(Action,Rel,Purpose,Json),
	json:json_write(user_output,Json), nl.

text_relation(Action,Rel,Purpose,[ActionText,RelText,PurposeText]) :-
	text_tuple(Action,ActionText),
	text_rel(Rel,RelText),
	text_tuple(Purpose,PurposeText).

json_relation(Action,Rel,Purpose,json([class='ExtractionRule',antecedents=[ActionJson],relation=RelJson,consequents=[PurposeJson]])) :-
	json_tuple(Action,ActionJson),
	json_rel(Rel,RelJson),
	json_tuple(Purpose,PurposeJson).

write_entity_relation(Action,Rel,Purpose) :-
	text_entity_relation(Action,Rel,Purpose,Text),
	format(user_output, '~w\t~w\t~w', Text), nl,
	write_inf_relation(Action,Rel,Purpose).
%	write_json_entity_relation(Action,Rel,Purpose).

write_json_entity_relation(Action,Rel,Purpose) :-
	json_entity_relation(Action,Rel,Purpose,Json),
	json:json_write(user_output,Json), nl.

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

write_simple_tuple(Node) :-
	tuple(Node,Tuple),
	text_tuple(Tuple,Text),
	write(Text), nl,
	write_inf_simple_tuple(Node).
write_simple_tuple(_).

write_json_simple_tuple(Node) :-
	tuple(Node,Tuple),
	json_tuple(Tuple,Json),
	json:json_write(user_output,Json), nl.
write_json_simple_tuple(_).


write_inf_relation(Action,[Rel-_|_],Purpose) :-
	inf_tuple(Action,ActionId),
	inf_tuple(Purpose,PurposeId),
	stripped_ids([ActionId, PurposeId], Ids),
	% left to right
	write_inf_tuple(ActionId),
	format('-> ~w(~w, ~w) ', [Rel|Ids]),
	write_inf_tuple(PurposeId),
	nl,
	% right to left
	write_inf_tuple(PurposeId),
	format('-> ~w(~w, ~w) ', [Rel|Ids]),
	write_inf_tuple(ActionId),
	nl,
	rdf_unload_graph(ActionId),
	rdf_unload_graph(PurposeId).


%write_inf_tuple(GraphId) :-
%	rdf_save_turtle(stream(user_output),[graph(GraphId),silent(true)]),
%	fail.
write_inf_tuple(GraphId) :-
	rdf(S,P,O,GraphId),
	write_rdf(S,P,O,GraphId),
	write(' '),
	fail.
write_inf_tuple(_).


write_inf_simple_tuple(Node) :-
	tuple(Node,Tuple),
	inf_tuple(Tuple,Id),
	write_inf_simple_tuple0(Id),
	rdf_unload_graph(Id).
write_inf_simple_tuple(_).

%write_inf_simple_tuple(GraphId) :-
%	rdf_save_turtle(stream(user_output),[graph(GraphId),silent(true)]),
%	fail.
write_inf_simple_tuple0(GraphId) :-
	rdf(S,pred:isa,O,GraphId),
	% write first
	nl,
	write_rdf(S,pred:isa,O,GraphId),
	write(' ->'),
	% rest excluding first
	rdf(S2,P2,O2,GraphId),
	\+ (S2 = S, O2 = O),
	write(' '),
	write_rdf(S2,P2,O2,GraphId),
	fail.
write_inf_simple_tuple0(_).


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
inf_tuple([S,V,Arg|Mods],V) :-
	text_arg(S,SubjText),
	text_verb(V,VerbText),
	( (rdf(_,basic:cop,V),
	   text_verb(Arg,ObjText)) % copula
	; text_arg(Arg,ObjText) ), % dobj
	( (Mods = [],
	   ModsText = [])
	; (text_mods(Mods,ModsList),
	   atomic_list_concat(ModsList,ModsText)) ),
	rdf_assert(V,pred:isa,literal(VerbText),V),
	(S = [] ; rdf_assert(S,pred:isa,literal(SubjText),V)),
	(Arg = [] ; rdf_assert(Arg,pred:isa,literal(ObjText),V)),
	(S = [] ; rdf_assert(V,pred:agent,S,V)),
	(Arg = [] ; rdf_assert(V,pred:object,Arg,V)),
	!.


text_tuple(Ent,Text) :-
	atom(Ent), !,
	text_arg(Ent,Text).
text_tuple([S,V],Text) :-
	text_tuple([S,V,[]],Text).
text_tuple([S,V,Arg|Mods],Text) :-
	text_arg(S,SubjText),
	text_verb(V,VerbText),
	( (rdf(_,basic:cop,V),
	   text_verb(Arg,ObjText)) % copula
	; text_arg(Arg,ObjText) ), % dobj
	( (Mods = [],
	   format(atom(Text), '(~w ~w ~w)', [SubjText, VerbText, ObjText]))
	; (text_mods(Mods,ModsList),
	   atomic_list_concat(ModsList,ModsText),
	   format(atom(Text), '(~w ~w ~w [ ~w ] )', [SubjText, VerbText, ObjText, ModsText])) ),
	!.


json_tuple(Ent,Json) :-
	atom(Ent), !,
	json_arg(Ent,Json).
json_tuple([S,V],Json) :-
	json_tuple([S,V,[]],Json).
json_tuple([S,V,Arg|Mods],json([class='ExtractionTuple',subject=SubjJson,verbPhrase=VerbTokenIds,directObject=ObjJson|ExtraPhrases])) :-
	json_arg(S,SubjJson),
	json_verb(V,VerbTokenIds),
	( (rdf(_,basic:cop,V),
	   json_verb(Arg,ArgTokenIds), % copula
	   ObjJson=json([class='Other',string=ArgTokenIds]))
	; json_arg(Arg,ObjJson) ), % dobj
	( (Mods = [],
	   ExtraPhrases = [])
	; (json_mods(Mods,ModsTokens),
	   ExtraPhrases = [extraPhrases=ModsTokens]) ),
	!.


text_arg([],'""') :- !.
text_arg(Arg-Var,Text) :- !,
	text_arg(Arg,ArgText),
	format(atom(Text), '~w/?~w', [ArgText, Var]).
text_arg(Arg,Text) :-
	arg_tokens(Arg,Tokens),
	tokens_text_quoted(Tokens,Text).

json_arg([],json([])) :- !.
json_arg(Arg-Var,json([class=Class,string=TokenIds,coreferences=Var])) :- !,
	json_arg(Arg,json([class=Class,string=TokenIds])).
json_arg(Arg,json([class='NounPhrase',string=TokenIds])) :-
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

json_entity(Arg,json([class='NounPhrase',string=TokenIds])) :-
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


text_verb(Arg,Text) :-
	verb_tokens(Arg,Tokens),
	lemmas_text_quoted(Tokens,Text).

json_verb(Arg,TokenIds) :-
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
	maplist(prefixed_id,Tokens,TokenIds).
prefixed_id(Token,TokenId) :-
	rdf_global_id(Term,Token),
	term_to_atom(Term,TokenId).

stripped_ids(Tokens,TokenIds) :-
	maplist(stripped_id,Tokens,TokenIds),
	!.
stripped_id(Token,TokenId) :-
	rdf_global_id(id:Id,Token),
	atomic_list_concat(['E',Id],TokenId).
stripped_id(Token,TokenId) :-
	rdf_global_id(_:TokenId,Token).
stripped_id(Token,Token). % literal
