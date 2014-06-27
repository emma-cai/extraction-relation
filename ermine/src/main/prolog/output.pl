:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_meta write_rdf(r,r,-,-,-).

:- rdf_register_prefix(pred, 'http://aristo.allenai.org/pred/').
:- rdf_register_prefix(rel, 'http://aristo.allenai.org/rel/').


write_relation(Top,Antecedent,Rel,Consequent,Inf,Json) :-
	text_relation(Antecedent,Rel,Consequent,Text),
	format(current_output, '% ~w\t~w\t~w', Text), nl,
	write_inf_relation(Top,Antecedent,Rel,Consequent,Inf),
	json_relation(Antecedent,Rel,Consequent,Json).
%	json:json_write(current_output,Json), nl.

text_relation(Entity1,Rel,Entity2,[Entity1Text,RelText,Entity2Text]) :-
	( (atom(Entity1),
	   text_entity(Entity1,Entity1Text))
	; text_tuple(Entity1,Entity1Text) ),
	text_rel(Rel,RelText),
	( (atom(Entity2),
	   text_entity(Entity2,Entity2Text))
	; text_tuple(Entity2,Entity2Text) ),
	!.

json_relation(Entity1,Rel,Entity2,json([class='ExtractionRule',antecedents=[Json1],relation=RelJson,consequents=[Json2],confidence=1.0])) :-
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
	json_ids(Tokens,TokenIds).

write_simple_tuple(Node,Inf,json([class='ExtractionRule',antecedents=[Json],consequents=[],confidence=1.0])) :-
	tuple(Node,Tuple),
	text_tuple(Tuple,Text),
	write('% '), write(Text), nl,
	write_inf_simple_tuple(Node,Tuple,Inf),
	json_tuple(Tuple,Json).
%	json:json_write(current_output,Json), nl.

entity_id(Entity,Id) :-
	(Id-_-_ = Entity ; Id-_ = Entity ; Id = Entity),
	!.

% move focus statements out of antecedent
question_focus(Ent-_) :- !,
	question_focus(Ent).
question_focus([_,Event-_,_]) :- !, % denominalized form
	question_focus(Event).
question_focus(Ent) :-
	atom(Ent), !,
	current_question_focus(Ent),
	question_focus_consequent_arg(Ent).
question_focus([_,Event|_]) :-
	current_question_focus(Event),
	question_focus_consequent_event(Event).
question_focus([Subj,Event|_]) :-
	entity_id(Subj,SubjId),
	entity_id(Event,EventId),
	current_question_focus(SubjId),
	( dependency(SubjId,dep:partmod,EventId)
	; dependency(SubjId,dep:rcmod,EventId) ),
	question_focus_consequent_event(EventId).
question_focus([Subj,_Event,Obj|Args]) :-
	member(Arg,[Subj,Obj|Args]), Arg \= [],
	question_focus(Arg), !.
question_focus([Subj,_Event,Obj|Args]) :-
	current_question_focus(Focus),
	member(Arg,[Subj,Obj|Args]), Arg \= [],
	entity_id(Arg,Id),
	constit(Id,Focus,[rcmod,partmod]), % substring case
	question_focus_consequent_arg(Id).
question_focus([_,Event|_]) :- % conjunction case
	current_question_focus(Focus),
	entity_id(Event,EventId),
	( rdf(EventId,basic:conj,Focus)
	; rdf(Focus,basic:conj,EventId) ),
	question_focus_consequent_event(EventId).

question_focus_consequent_event(Event) :-
	rdf(Event,Pred,Arg,antecedent),
	rdf_global_id(pred:_,Pred),
%	rdf_update(Event,Pred,Arg,antecedent,graph(consequent)),
	rdf_assert(Event,Pred,Arg,consequent),
	% move Arg to consequent if not referenced elsewhere in antecedent
	\+ (rdf(Event2,Pred2,Arg,antecedent),
	    Event2 \= Event,
	    rdf_global_id(pred:_,Pred2)),
	question_focus_consequent_arg(Arg),
	fail.
question_focus_consequent_event(_).

question_focus_consequent_arg(Id) :-
	rdf(Id,pred:isa,Type,antecedent),
%	rdf_update(Id,pred:isa,Type,antecedent,graph(consequent)),
	rdf_assert(Id,pred:isa,Type,consequent),
	fail.
question_focus_consequent_arg(_).


write_inf_relation(Top,Ent-_,Rel,Tuple,Out) :- !,
	write_inf_relation(Top,Ent,Rel,Tuple,Out).
write_inf_relation(_Top,Entity,[Rel-_|_],[Subj,Verb|Rest],Out) :- % question-specific
	current_question_focus(_),
	atom(Entity),
	entity_id(Subj,E),
	% isa relative clause
	rdf(E,dep:nsubj,Entity),
	rdf(E,dep:rcmod,Verb), !,
	inf_tuple(Entity,AntecedentId,antecedent),
	inf_tuple([Subj,Verb|Rest],_,antecedent),
	rdf_assert(Entity,pred:isa,Subj,antecedent),
	downcase_atom(Rel,LRel),
	rdf_global_id(rel:LRel,RelId),
	rdf_assert(AntecedentId,RelId,Subj,consequent),
	write_question_relation0(Entity,[Subj,Verb|Rest],Out).
write_inf_relation(_Top,Antecedent,[Rel-_|_],Consequent,Out) :- % question-specific
	current_question_focus(_), !,
	inf_tuple(Antecedent,AntecedentId,antecedent),
	inf_tuple(Consequent,ConsequentId,antecedent),
	downcase_atom(Rel,LRel),
	rdf_global_id(rel:LRel,RelId),
	rdf_assert(AntecedentId,RelId,ConsequentId,consequent),
	write_question_relation0(Antecedent,Consequent,Out).
write_inf_relation(Top,Entity,[Rel-_|_],[Subj,Verb|Rest],Out) :-
	atom(Entity),
	entity_id(Subj,E),
	% isa relative clause
	rdf(E,dep:nsubj,Entity),
	rdf(E,dep:rcmod,Verb), !,
	inf_tuple(Entity,_,AntecedentId),
	once(stripped_id(AntecedentId,StrippedAntecedentId)),
	inf_tuple([Subj,Verb|Rest],_,ConsequentId),
	once(stripped_id(Subj,StrippedSubjId)),
	downcase_atom(Rel,LRel),
	% left to right
	rdf_assert(Entity,pred:isa,Subj,ConsequentId), % RHS
	write_inf_relation0(Top,AntecedentId,[LRel,StrippedAntecedentId,StrippedSubjId],ConsequentId,Out1),
	rdf_retractall(Entity,pred:isa,Subj,ConsequentId), % RHS
	% right to left
	rdf_assert(Entity,pred:isa,Subj,AntecedentId), %LHS
	write_inf_relation0(Top,ConsequentId,[LRel,StrippedAntecedentId,StrippedSubjId],AntecedentId,Out2),
	atomic_list_concat([Out1,Out2],Out),
	% write turtle
	rdf_global_id(rel:LRel,RelId),
	rdf_assert(AntecedentId,RelId,Subj,AntecedentId), %LHS
	write_turtle_relation(AntecedentId,ConsequentId),
	rdf_unload_graph(AntecedentId),
	rdf_unload_graph(ConsequentId).
write_inf_relation(Top,Antecedent,[Rel-_|_],Consequent,Out) :-
	inf_tuple(Antecedent,_,AntecedentId),
	once(stripped_id(AntecedentId,StrippedAntecedentId)),
	inf_tuple(Consequent,_,ConsequentId),
	once(stripped_id(ConsequentId,StrippedConsequentId)),
	downcase_atom(Rel,LRel),
	% left to right
	write_inf_relation0(Top,AntecedentId,[LRel,StrippedAntecedentId,StrippedConsequentId],ConsequentId,Out1),
	% right to left
	write_inf_relation0(Top,ConsequentId,[LRel,StrippedAntecedentId,StrippedConsequentId],AntecedentId,Out2),
	atomic_list_concat([Out1,Out2],Out),
	% write turtle
	rdf_global_id(rel:LRel,RelId),
	rdf_assert(AntecedentId,RelId,ConsequentId,AntecedentId), %LHS
	write_turtle_relation(AntecedentId,ConsequentId),
	rdf_unload_graph(AntecedentId),
	rdf_unload_graph(ConsequentId).

write_turtle_relation(AntecedentId,ConsequentId) :-
	nl,
	rdf_save_turtle(stream(current_output),[graph(AntecedentId),silent(true)]),
	nl,
	rdf_save_turtle(stream(current_output),[graph(ConsequentId),silent(true)]).

write_inf_relation0(Top,Left,Rel,Right,Out) :-
	gensym(rule,Id),
	write_inf_rule_text(Top,Id,Text),
	write_inf_tuple(Left,[],LeftTriples,true,LHS),
	format(atom(Relation), '~w(~w, ~w)', Rel),
	write_inf_tuple(Right,LeftTriples,_,false,RHS),
	write_simplified_inf_relation(Left,Right,Rel,Id,Pretty),
	format(atom(Out), '~w.~n~w.~n~w:: ~w -> ~w~w.~n', [Text,Pretty,Id,LHS,Relation,RHS]).


write_question_relation0(Left,Right,Out) :-
	( question_focus(Left) ; question_focus(Right) ),
	gensym(rule,Id),
	write_inf_rule_text(_Top,Id,Text), % all input
	% collect RHS to exclude from LHS
	%%% TODO: track down rdf_update failures
	findall([S,P,O],
		(rdf(S,P,O,consequent),
		 \+ rdf_global_id(rdf:type,P)),
		Consequents),
	write_inf_tuple(antecedent,Consequents,LeftTriples,true,LHS),
	write_inf_tuple(consequent,LeftTriples,RightTriples,true,RHS),
	write_simplified_inf_question(LeftTriples,RightTriples,Id,Pretty),
	format(atom(Out), '~w.~n~w.~n~w:: ~w -> ~w.~n', [Text,Pretty,Id,LHS,RHS]),
	write_turtle_relation(antecedent,consequent),
	!.
write_question_relation0(_,_,''). % setup only, don't write


write_inf_tuple(GraphId,Remove,Triples,First,Out) :-
	findall([S,P,O],
		(rdf(S,P,O,GraphId),
		 \+ rdf_global_id(rdf:type,P)),
		Ts),
	subtract(Ts,Remove,Triples),
	write_rdf_list(Triples,GraphId,TriplesOut),
	( ( (Triples = [] ; First = true),
	    Out = TriplesOut )
	; format(atom(Out), ', ~w', [TriplesOut]) ),
	!.

write_inf_simple_tuple(_,Tuple,Out) :- % question-specific
	current_question_focus(_), !,
	inf_tuple(Tuple,_,antecedent),
	write_question_relation0(Tuple,[],Out).
write_inf_simple_tuple(Root,Tuple,Inf) :-
	inf_tuple(Tuple,_,Id),
	findall(Out,write_inf_simple_tuple0(Root,Id,Out),Outs),
	atomic_list_concat(Outs,Inf),
	rdf_unload_graph(Id).

write_inf_simple_tuple0(Root,GraphId,Out) :-
	rdf(S,pred:isa,O,GraphId),
	gensym(rule,Id),
	write_inf_rule_text(Root,Id,Text),
	% write first
	write_rdf(S,pred:isa,O,GraphId,First),
	% write rest
	write_inf_tuple(GraphId,[[S,_,O]],_,true,Rest),
	write_simplified_inf_simple_tuple(S,Root,Id,Pretty),
	format(atom(Out), '~w.~n~w.~n~w:: ~w -> ~w.~n', [Text,Pretty,Id,First,Rest]).
write_inf_simple_tuple0(_,GraphId,'') :-
	nl,
	rdf_save_turtle(stream(current_output),[graph(GraphId),silent(true)]),
	nl.

write_inf_rule_text(Root,Id,Out) :-
	tokens(Root,Tokens),
	tokens_text(Tokens,Text),
	format(atom(Out), 'english(~w, "~w")', [Id,Text]),
	!.

% comma separated list
write_rdf_list([],_,'') :- !.
write_rdf_list([[S,P,O]],GraphId,Out) :-
	write_rdf(S,P,O,GraphId,Out),
	!.
write_rdf_list([[S,P,O]|Rest],GraphId,AllOut) :-
	write_rdf(S,P,O,GraphId,Out),
	write_rdf_list(Rest,GraphId,RestOut),
	format(atom(AllOut), '~w, ~w', [Out,RestOut]).

write_rdf(S,P,O,GraphId,Out) :-
	rdf(S,P,O,GraphId),
	(O = literal(Val); Val = O),
	stripped_ids([P,S,Val],Ids),
	format(atom(Out), '~w(~w, ~w)',Ids), !.

inf_tuple(Ent-_,EntId,GraphId) :- !,
	inf_tuple(Ent,EntId,GraphId).
inf_tuple(Ent,Ent,GraphId) :-
	atom(Ent), !,
	( nonvar(GraphId) ; GraphId = Ent ), !,
	text_arg(Ent,Text),
	rdf_assert(Ent,pred:isa,literal(Text),GraphId),
	rdf_assert(Ent,rdf:type,entity,GraphId).
inf_tuple([S,V,O-_|Rest],TupleId,GraphId) :- !, % ignore vars
	inf_tuple([S,V,O|Rest],TupleId,GraphId).
inf_tuple([S-_,V|Rest],TupleId,GraphId) :- !, % ignore vars
	inf_tuple([S,V|Rest],TupleId,GraphId).
inf_tuple([S,V],TupleId,GraphId) :-
	inf_tuple([S,V,[]],TupleId,GraphId).
inf_tuple([S,Verb,Arg|Mods],V,GraphId) :-
	text_arg(S,SubjText),
	text_verb(Verb,VerbText,V),
	( nonvar(GraphId) ; GraphId = V), !,
	( (rdf(_,basic:cop,V),
	   text_verb(Arg,ObjText,_)) % copula
	; text_arg(Arg,ObjText) ), % dobj
	rdf_assert(V,pred:isa,literal(VerbText),GraphId),
	rdf_assert(V,rdf:type,event,GraphId),
	(S = []
	; (rdf_assert(V,pred:agent,S,GraphId),
	   rdf_assert(S,pred:isa,literal(SubjText),GraphId),
	   rdf_assert(S,rdf:type,entity,GraphId)) ),
	(Arg = []
	; (rdf_assert(V,pred:object,Arg,GraphId),
	   rdf_assert(Arg,pred:isa,literal(ObjText),GraphId),
	   rdf_assert(Arg,rdf:type,entity,GraphId)) ),
	inf_mods(V,Mods,GraphId),
	!.

inf_mods(_V, [], _GraphId) :- !.
inf_mods(V, [Pobj|Rest], GraphId) :-
	prep(Pobj,Prep),
	!,
	lemma(Prep,P),
	atom_concat('http://aristo.allenai.org/pred/',P,NewPrepRel),
	tokens(Pobj,Tokens,[conj,cc,appos,xcomp,infmod,rcmod]),
	tokens_text_quoted(Tokens,Text),
	rdf_assert(V,NewPrepRel,Pobj,GraphId),
	rdf_assert(Pobj,pred:isa,literal(Text),GraphId),
	inf_mods(V,Rest,GraphId).
inf_mods(V, [Prep|Rest], GraphId) :-
	prep(Mod,Prep),
	rdf(V,PrepRel,Mod),
	( (atom_concat('http://nlp.stanford.edu/dep/prep_',P,PrepRel),
	   NewP = P)
	; (atom_concat('http://nlp.stanford.edu/dep/prepc_',PC,PrepRel),
	   NewP = PC) ),
	!,
	atom_concat('http://aristo.allenai.org/pred/',NewP,NewPrepRel),
	text_mod(Mod,Text),
	rdf_assert(V,NewPrepRel,Mod,GraphId),
	rdf_assert(Mod,pred:isa,literal(Text),GraphId),
	inf_mods(V,Rest,GraphId).
inf_mods(V, [Mod|Rest], GraphId) :-
	text_mod(Mod,Text),
	rdf_assert(V,pred:arg,Mod,GraphId),
	rdf_assert(Mod,pred:isa,literal(Text),GraphId),
	inf_mods(V,Rest,GraphId).

text_tuple(Ent,Text) :-
	(atom(Ent) ; Ent = _-_), !,
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


json_tuple(Ent,json([class='ExtractionTuple',subject=Json,verbPhrase=[],extraPhrases=[]])) :-
	(atom(Ent) ; Ent = _-_), !,
	json_arg(Ent,Json).
json_tuple([S,V],Json) :-
	json_tuple([S,V,[]],Json).
json_tuple([S,Verb,Arg|Mods],json([class='ExtractionTuple',subject=SubjJson,verbPhrase=VerbTokenIds|RestJson])) :-
	json_arg(S,SubjJson),
	json_verb(Verb,VerbTokenIds,V),
	( (rdf(_,basic:cop,V),
	   json_verb(Arg,ArgTokenIds,_), % copula
	   ObjJson=json([class='Other',string=ArgTokenIds]))
	; json_arg(Arg,ObjJson) ), % dobj
	json_mods(Mods,ModsTokens),
	ExtraPhrases = [extraPhrases=ModsTokens],
	( (ObjJson = json([string=[]|_]), % empty
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

json_arg([],json([string=[],isInferred= @(false),coreferences=[]])) :- !.
json_arg(Arg-Var-true,json([class=Class,string=[],isInferred= @(true),
			    coreferences=[json([class='Coreference',label=Var,sourceTokens=TokenIds])]])) :- !,
	json_arg(Arg,json([class=Class,string=TokenIds|_])).
json_arg(Arg-Var,json([class=Class,string=TokenIds,isInferred= @(false),
		       coreferences=[json([class='Coreference',label=Var,sourceTokens=TokenIds])]])) :- !,
	json_arg(Arg,json([class=Class,string=TokenIds|_])).
json_arg(Arg,json([class='NounPhrase',string=TokenIds,isInferred= @(false),
		   coreferences=[]])) :-
	arg_tokens(Arg,Tokens),
	json_ids(Tokens,TokenIds).

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

json_entity(Arg,json([class='ExtractionTuple',subject=json([class='NounPhrase',string=TokenIds,isInferred= @(false),coreferences=[]]),verbPhrase=[],extraPhrases=[]])) :-
	entity_tokens(Arg,Tokens),
	json_ids(Tokens,TokenIds).

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
	json_ids(Tokens,TokenIds).


mod_tokens([],[]) :- !.
% special case for prep to apply exclusion list to pobj
mod_tokens(Mod,[Mod|ObjTokens]) :-
	rdf(_,basic:prep,Mod),
	( rdf(Mod,basic:pobj,Obj)
	; rdf(Mod,basic:pcomp,Obj)), !,
	tokens(Obj,ObjTokens,[conj,cc,appos,xcomp,infmod,rcmod]).
% include prep with pobj
mod_tokens(Obj,[Prep|ObjTokens]) :-
	prep(Obj,Prep), !,
	tokens(Obj,ObjTokens,[conj,cc,appos,xcomp,infmod,rcmod]).
mod_tokens(Mod,Tokens) :-
	tokens(Mod,Tokens,[conj,cc,appos,xcomp,infmod,rcmod]).


text_verb(Arg-Norm,Text,Arg) :- !, % denominalized
	format(atom(Text), '"~w"', [Norm]).
text_verb(Arg,Text,Arg) :-
	verb_tokens(Arg,Tokens),
	lemmas_text_quoted(Tokens,Text).

json_verb(Arg-Norm,[Json],Arg) :- !, % denominalized
	json_id(Arg-Norm,Json).
json_verb(Arg,TokenIds,Arg) :-
	verb_tokens(Arg,Tokens),
	json_lemma_ids(Tokens,TokenIds).

verb_tokens([],[]) :- !.
verb_tokens(Arg,[Arg-Verb]) :-
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


json_ids(Tokens,TokenIds) :-
	maplist(json_id,Tokens,TokenIds), !.
json_id(Token-Text,json([token=TokenId,text=Text])) :- !,
	rdf_global_id(id:Val,Token),
	atomic_list_concat([id,':',Val],TokenId).
json_id(Token,json([token=TokenId,text=Text])) :-
	rdf_global_id(id:Val,Token),
	atomic_list_concat([id,':',Val],TokenId),
	token_text(Token,Text).
json_id(Token,json([token=TokenId])) :-
	rdf_global_id(Term,Token),
	term_to_atom(Term,TokenId).

json_lemma_ids(Tokens,TokenIds) :-
	maplist(json_lemma_id,Tokens,TokenIds), !.
json_lemma_id(Token-Lemma,json([token=TokenId,text=Lemma])) :- !,
	rdf_global_id(id:Val,Token),
	atomic_list_concat([id,':',Val],TokenId).
json_lemma_id(Token,json([token=TokenId,text=Text])) :-
	rdf_global_id(id:Val,Token),
	atomic_list_concat([id,':',Val],TokenId),
	lemma_text(Token,Text).
json_lemma_id(Token,json([token=TokenId])) :-
	rdf_global_id(Term,Token),
	term_to_atom(Term,TokenId).

stripped_ids([],[]) :- !.
stripped_ids([Token|Rest],[TokenId|RestIds]) :-
	once(stripped_id(Token,TokenId)),
	stripped_ids(Rest,RestIds).

stripped_id(Token,TokenId) :- % event
	rdf(Token,rdf:type,event),
	rdf_global_id(id:_,Token),
	token_id(Token,Id),
	lemma(Token,Lemma),
	atomic_list_concat(['E',Id,'-',Lemma],TokenId).
stripped_id(Token,TokenId) :- % argument
	rdf_global_id(id:_,Token),
	token_id(Token,Id),
	lemma(Token,Lemma),
	atomic_list_concat(['A',Id,'-',Lemma],TokenId).
stripped_id(Token,TokenId) :-
	rdf_global_id(_:TokenId,Token).
stripped_id(Token,Token). % literal
