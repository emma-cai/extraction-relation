
%%% simplified form of rules

write_simplified_inf_relation(Left,Right,[Rel,LId,_RId],Id,Pretty) :-
	stripped_id(Left,LId),
	simplified_inf_pred(Left,LPred,null,'',LArgs), % LHS
	upcase_atom(Rel,Relation),
	simplified_inf_pred(Right,RPred,null,'',RArgs), % RHS
	append([LArgs,RArgs],ArgsList),
	list_to_set(ArgsList,Args),
	arg_definitions(Args,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "~w(~w, ~w)~w.")', [Id,Relation,LPred,RPred,ArgsText]),
	!.
write_simplified_inf_relation(Right,Left,[Rel,LId,_RId],Id,Pretty) :-
	stripped_id(Left,LId),
	simplified_inf_pred(Left,LPred,null,'',LArgs), % LHS
	upcase_atom(Rel,Relation),
	simplified_inf_pred(Right,RPred,null,'',RArgs), % RHS
	append([LArgs,RArgs],ArgsList),
	list_to_set(ArgsList,Args),
	arg_definitions(Args,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "~w(~w, ~w)~w.")', [Id,Relation,LPred,RPred,ArgsText]),
	!.
write_simplified_inf_relation(_,_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).

arg_definitions([],_,'').
arg_definitions([[_Prep,Arg]|Rest],Focus,ArgText) :- !,
	arg_definitions([Arg|Rest],Focus,ArgText).
arg_definitions([Arg],Focus,ArgText) :- !,
	simplified_lemma(Arg,Focus,Lemma),
	simplified_string(Arg,Focus,String),
	atomic_list_concat([', ',Lemma,' = ',String],ArgText).
arg_definitions([Arg|RestArgs],Focus,ArgsText) :-
	arg_definitions([Arg],Focus,ArgText),
	arg_definitions(RestArgs,Focus,RestArgsText),
	atomic_list_concat([ArgText,RestArgsText],ArgsText).

simplified_inf_pred(Root-_,Pred,Focus,Prefix,Args) :-
	simplified_inf_pred(Root,Pred,Focus,Prefix,Args).
simplified_inf_pred(Root,Pred,Focus,Prefix,Args) :-
	rdf(Root,rdf:type,event),
	simplified_lemma(Root,Focus,Lemma),
	simplified_inf_args(Root,Focus,ArgText,Args),
	atomic_list_concat([Prefix,Lemma,ArgText],Pred).
simplified_inf_pred(Root,Lemma,Focus,_Prefix,[Root]) :-
	simplified_lemma(Root,Focus,Lemma).
	
/*
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
simplified_inf_pred(Root,String,Focus,Prefix) :-
	Prefix \= ', Q=', % not focus
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
*/

simplified_inf_args(Root,Focus,ArgStringOut,Args) :-
	findall(Arg,simplified_inf_arg(Root,Arg),Args),
	Args \= [],
	format_simplified_inf_args(Args,Focus,'',ArgString,Prep),
	atomic_list_concat([Prep,'(',ArgString,')'],ArgStringOut).
simplified_inf_args(_,_,'',[]).

simplified_inf_arg(Root,Arg) :-
	rdf(Root,pred:agent,Arg).
simplified_inf_arg(Root,Arg) :-
	rdf(Root,pred:object,Arg),
	\+ rdf(Root,pred:agent,Arg). % TODO: track down fly(bat, bat)
simplified_inf_arg(Root,Arg) :-
	rdf(Root,pred:arg,Arg).
simplified_inf_arg(Root,[Prep,Arg]) :-
	rdf(Root,Pred,Arg),
	rdf_global_id(pred:Prep,Pred),
	\+ memberchk(Prep,[agent,object,arg,isa]).

format_simplified_inf_args([],_,_,'','') :- !.
format_simplified_inf_args([],_,_,'',_Prep) :- !.
format_simplified_inf_args([[Prep,Arg]|Rest],Focus,Prefix,ArgString,PrepOut) :-
	var(PrepOut), % first PP
	atomic_list_concat(['_',Prep],PrepOut),
%	simplified_inf_pred(Arg,ArgText,Focus,'',_SubArgs),
	simplified_lemma(Arg,Focus,ArgText),
	format_simplified_inf_args(Rest,Focus,', ',RestArgString,PrepOut),
	atomic_list_concat([Prefix,ArgText,RestArgString],ArgString).
format_simplified_inf_args([[Prep,Arg]|Rest],Focus,Prefix,ArgString,PrepOut) :-
	nonvar(PrepOut), % non-initial PPs
%	simplified_inf_pred(Arg,ArgText,Focus,'',_SubArgs),
	simplified_lemma(Arg,Focus,ArgText),
	format_simplified_inf_args(Rest,Focus,', ',RestArgString,PrepOut),
	atomic_list_concat([Prefix,Prep,'(',ArgText,')',RestArgString],ArgString).
format_simplified_inf_args([Arg|Rest],Focus,Prefix,ArgString,Prep) :-
%	simplified_inf_pred(Arg,ArgText,Focus,'',_SubArgs),
	simplified_lemma(Arg,Focus,ArgText),
	format_simplified_inf_args(Rest,Focus,', ',RestArgString,Prep),
	atomic_list_concat([Prefix,ArgText,RestArgString],ArgString).

simplified_lemma(Arg,_,Lemma) :- % non-question
	rdf(Arg,rdf:type,event),
	simplified_verb_tokens(Arg,Tokens),
	lemmas_text(Tokens,Lemma),
	!.
simplified_lemma(Arg,_,Lemma) :-
	lemma(Arg,L),
	atom_codes(L,[First|Rest]),
	code_type(Upper,to_upper(First)),
	atom_codes(Lemma,[Upper|Rest]).

% hacky override to allow for NPs parsed as verbs
simplified_verb_tokens([],[]) :- !.
simplified_verb_tokens(Arg,[Arg-Verb]) :-
	rdf(Arg,token:lemma,literal(Lemma)),
	wn-denom(Lemma,Verb),
	!.
simplified_verb_tokens(Arg,Tokens) :-
	tokens(Arg,Tokens,[aux,auxpass,nsubj,nsubjpass,csubj,csubjpass,dobj,iobj,xcomp,prep,conj,cc,mark,advcl,advmod,npadvmod,tmod,acomp,dep,ccomp,cop,expl,attr,xsubj,purpcl,det]).


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
	simplified_inf_pred(Root,Pred,null,'',_),
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
	; simplified_inf_pred(Focus,RightPred,null,', Q=',_) ),
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
		 once(simplified_inf_pred(E,Pred,Focus,'',_))),
		Preds),
	atomic_list_concat(Preds,', ',OutPreds),
	format(atom(Out), '~w~w', [Prefix,OutPreds]).
write_simplified_triples([[Entity|_]|Rest],Focus,OutPred,Prefix) :-
	Entity \= Focus,
	\+ (member([Entity,P,_],Rest), rdf_global_id(rel:_,P)),
	\+ (member([_,P,Entity],Rest), rdf_global_id(rel:_,P)),
	simplified_inf_pred(Entity,OutPred,Focus,Prefix,_).
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
