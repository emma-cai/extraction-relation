
%%% simplified form of rules

write_simplified_inf_relation(Left,Right,Rel,Id,Pretty) :-
	simplified_inf_pred(Left,LPred,null,''), % LHS
	simplified_inf_rel(Rel,Left,Right,Relation,Prefix),
	simplified_inf_pred(Right,RPred,null,Prefix), % RHS
	format(atom(Pretty), 'pretty(~w, "~w~w~w.")', [Id,LPred,Relation,RPred]),
	!.
write_simplified_inf_relation(_,_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).

simplified_inf_rel([example|_],Left,Right,Relation,', ') :-
	% relc case
	( dependency(Arg,dep:partmod,Left)
	; dependency(Arg,dep:rcmod,Left) ), !,
	simplified_string(Right,null,R),
	simplified_string(Arg,null,A),
	left_relation(example,RRel),
	format(atom(Relation), ' -IMPLIES-> ~w(~w, ~w)', [RRel,R,A]).
simplified_inf_rel([example|_],Left,Right,Relation,', ') :-
	% relc case
	( dependency(Arg,dep:partmod,Right)
	; dependency(Arg,dep:rcmod,Right) ), !,
	simplified_string(Left,null,R),
	simplified_string(Arg,null,A),
	left_relation(example,RRel),
	format(atom(Relation), ' -IMPLIES-> ~w(~w, ~w)', [RRel,R,A]).
simplified_inf_rel([Rel,LId,_],Left,_,Relation,' ') :-
	stripped_id(Left,LId), !,
	left_relation(Rel,LRel),
	format(atom(Relation), ' -~w->', [LRel]).
simplified_inf_rel([Rel,_,RId],Left,_,Relation,' ') :-
	stripped_id(Left,RId), !,
	right_relation(Rel,RRel),
	format(atom(Relation), ' -~w->', [RRel]).
simplified_inf_rel([Rel,_,_],_,_,Relation,' ') :-
	format(atom(Relation), ' -~w->', [Rel]).

left_relation(example,'EXAMPLE_OF').
left_relation(cause,'CAUSES').
left_relation(enable,'ENABLES').
left_relation(when,'WHEN').
left_relation(Rel,Relation) :-
	upcase_atom(Rel,UpRel),
	atomic_list_concat(['HAS_',UpRel], Relation).

right_relation(example,'HAS_EXAMPLE').
right_relation(cause,'CAUSED_BY').
right_relation(enable,'ENABLED_BY').
right_relation(when,'THEN').
right_relation(Rel,Relation) :-
	upcase_atom(Rel,UpRel),
	atomic_list_concat([UpRel,'_OF'], Relation).


simplified_inf_pred(Root-_,Pred,Focus,Prefix) :-
	simplified_inf_pred(Root,Pred,Focus,Prefix).
simplified_inf_pred(Focus,'',Focus,'') :- !, fail. % LHS only
simplified_inf_pred(Root,'',Focus,'') :- % LHS only
	( dependency(Focus,dep:partmod,Root)
	; dependency(Focus,dep:rcmod,Root) ),
	!, fail.
simplified_inf_pred(Root,Pred,Focus,Prefix) :-
	rdf(Root,rdf:type,event),
	simplified_lemma(Root,Focus,Lemma),
	( (rdf(Root,pred:agent,Subj),
	   simplified_string(Subj,Focus,S))
	; S = 'X' ),
	simplified_inf_args(Root,Focus,Args),
	format(atom(Pred), '~w~w(~w~w)', [Prefix,Lemma,S,Args]).
simplified_inf_pred(_,'',_,', ') :- % after relclause
	!.
simplified_inf_pred(Root,String,Focus,Prefix) :-
	simplified_lemma(Root,Focus,Lemma), % entity
	entity_tokens(Root,Tokens),
	tokens_text_single_quoted(Tokens,Text),
	format(atom(String), '~wisa(~w, ~w)', [Prefix,Lemma,Text]).
simplified_inf_pred(Root,String,_Focus,Prefix) :-
%	simplified_lemma(Root,Focus,Lemma), % entity
	entity_tokens(Root,Tokens),
	tokens_text_single_quoted(Tokens,Text),
	format(atom(String), '~w~w', [Prefix,Text]).
simplified_inf_pred(_,'',_,_).

simplified_inf_args(Root,Focus,ArgString) :-
	findall(Arg,simplified_inf_arg(Root,Focus,Arg),Args),
	Args \= [],
	format_simplified_inf_args(Args,ArgString).
simplified_inf_args(_,_,'').

simplified_inf_arg(Root,Focus,Arg) :-
	rdf(Root,pred:object,Obj),
	\+ rdf(Root,pred:agent,Obj), % TODO: track down fly(bat, bat)
	simplified_string(Obj,Focus,Arg).
simplified_inf_arg(Root,Focus,Arg) :-
	rdf(Root,pred:arg,Obj),
	simplified_string(Obj,Focus,Arg).
simplified_inf_arg(Root,Focus,PP) :-
	rdf(Root,Pred,Obj),
	rdf_global_id(pred:Prep,Pred),
	\+ memberchk(Prep,[agent,object,arg,isa]),
	simplified_string(Obj,Focus,Arg),
	atomic_list_concat([Prep,'(',Arg,')'],PP).

format_simplified_inf_args([],'') :- !.
format_simplified_inf_args([Arg|Rest],ArgString) :-
	format_simplified_inf_args(Rest,RestArgString),
	format(atom(ArgString), ', ~w~w', [Arg, RestArgString]).

simplified_lemma(Arg,null,Lemma) :-
	rdf(Arg,rdf:type,event),
	rdf(Arg,pred:isa,literal(String)),
	atomic_list_concat(['',Lemma,''],'"',String),
	!.
simplified_lemma(Arg,null,Lemma) :-
	lemma(Arg,Lemma), !.
simplified_lemma(Arg,_,'Q') :-
	current_question_focus(Arg), !.
simplified_lemma(Arg,_,'Q') :-
	current_question_focus(Focus),
	( dependency(Arg,dep:partmod,Focus)
	; dependency(Arg,dep:rcmod,Focus) ),
	!.
simplified_lemma(Arg,_,Lemma) :-
	lemma(Arg,Lemma).


simplified_string(Arg,null,Lemma) :-
	rdf(Arg,rdf:type,event),
	rdf(Arg,pred:isa,literal(String)),
	atomic_list_concat(['',Lemma,''],'"',String),
	!.
simplified_string(Arg,null,Lemma) :-
	arg_tokens(Arg,Tokens),
	tokens_text_single_quoted(Tokens,Lemma), !.
simplified_string(Arg,_,'Q') :-
	current_question_focus(Arg), !.
simplified_string(Arg,_,'Q') :-
	current_question_focus(Focus),
	( dependency(Arg,dep:partmod,Focus)
	; dependency(Arg,dep:rcmod,Focus) ),
	!.
simplified_string(Arg,_,Lemma) :-
	arg_tokens(Arg,Tokens),
	tokens_text_single_quoted(Tokens,Lemma).


%%% simplified form of basic sentence (no relation)

write_simplified_inf_simple_tuple(Entity,Root,Id,Pretty) :-
	( (rdf(Entity,rdf:type,event),
	   verb_tokens(Entity,Tokens))
	; entity_tokens(Entity,Tokens) ),
	tokens_text_single_quoted(Tokens,Text),
	simplified_lemma(Entity,null,Lemma),
	simplified_inf_pred(Root,Pred,null,''),
	format(atom(Pretty), 'pretty(~w, "isa(~w, ~w) -> ~w.")', [Id,Lemma,Text,Pred]),
	!.
write_simplified_inf_simple_tuple(_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).


%%% simplified form of questions

write_simplified_inf_question(LeftTriples,RightTriples,Id,Pretty) :-
	current_question_focus(Focus),
	write_simplified_triples(LeftTriples,Focus,LeftPred,''),
	write_simplified_question_relation(RightTriples,Focus,'',Relation,Prefix),
	( (Prefix = ', Q=',
	   write_simplified_triples(RightTriples,null,RightPred,Prefix))
	; simplified_inf_pred(Focus,RightPred,null,', Q=') ),
	format(atom(Pretty), 'pretty(~w, "~w~w~w.")', [Id,LeftPred,Relation,RightPred]),
	!.
write_simplified_inf_question(_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).

write_simplified_triples(Triples,Focus,Out,Prefix) :-
	findall(Event,
		(member([Event|_],Triples),
		 Event \= Focus,
		 rdf(Event,rdf:type,event)),
		Events),
	Events \= [],
	list_to_set(Events,EventSet),
	findall(Pred,
	        (member(E,EventSet),
		 once(simplified_inf_pred(E,Pred,Focus,''))),
		Preds),
	atomic_list_concat(Preds,', ',OutPreds),
	format(atom(Out), '~w~w', [Prefix,OutPreds]).
write_simplified_triples([[Entity|_]|Rest],Focus,OutPred,Prefix) :-
	Entity \= Focus,
	\+ (member([Entity,P,_],Rest), rdf_global_id(rel:_,P)),
	\+ (member([_,P,Entity],Rest), rdf_global_id(rel:_,P)),
	simplified_inf_pred(Entity,OutPred,Focus,Prefix).
write_simplified_triples(_,_,'',_).

write_simplified_question_relation(Triples,Focus,_Prev,Relation,Var) :-
	member([S,P,O],Triples),
	rdf_global_id(rel:Rel,P), !,
	focus_lemma(S,Focus,SLemma, '('),
	focus_lemma(O,Focus,OLemma, ', '),
	( ( ( SLemma = '(Q'
	    ; OLemma = ', Q'
	    ; memberchk([_,_,Focus],Triples) ),
	  Var = ', Q=' )
	; Var = ', '), % relation only
	left_relation(Rel,RelName),
	format(atom(Relation), ' -> ~w~w~w)', [RelName,SLemma,OLemma]).
write_simplified_question_relation(_,_,_,' -> ','Q=').

focus_lemma(Focus,Focus,Q,Prefix) :-
	atomic_list_concat([Prefix,'Q'],Q), !.
focus_lemma(Arg,Focus,Q,Prefix) :-
	dependency(Focus,dep:partmod,Arg),
	atomic_list_concat([Prefix,'Q'],Q), !.
focus_lemma(Arg,Focus,Q,Prefix) :-
	dependency(Focus,dep:partmod,Arg),
	atomic_list_concat([Prefix,'Q'],Q), !.
focus_lemma(Arg,_,Out,Prefix) :-
	simplified_lemma(Arg,null,Lemma),
	atomic_list_concat([Prefix,Lemma],Out).
