
%%% simplified form of rules

% relative clause case
write_simplified_inf_relation(Left,Right,[example,LId,_RId],Id,Pretty) :-
	stripped_id(Left,LId),
	( dependency(Subj,dep:partmod,Right)
	; dependency(Subj,dep:rcmod,Right) ),
	rdf(Left,pred:isa,Subj),
	simplified_inf_pred(Left,null,LPred,LArgs), % LHS
	simplified_inf_pred(Subj,null,Pred,Args), % RHS
	simplified_inf_pred(Right,null,RPred,RArgs), % RHS
	append([LArgs,Args,RArgs],ArgsList),
	list_to_set(ArgsList,ArgsSet),
	arg_definitions(ArgsSet,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "EXAMPLE(~w, ~w) & ~w~w.")', [Id,LPred,Pred,RPred,ArgsText]),
	!.
write_simplified_inf_relation(Right,Left,[example,LId,_RId],Id,Pretty) :-
	stripped_id(Left,LId),
	( dependency(Subj,dep:partmod,Right)
	; dependency(Subj,dep:rcmod,Right) ),
	rdf(Left,pred:isa,Subj),
	simplified_inf_pred(Left,null,LPred,LArgs), % LHS
	simplified_inf_pred(Subj,null,Pred,Args), % RHS
	simplified_inf_pred(Right,null,RPred,RArgs), % RHS
	append([LArgs,Args,RArgs],ArgsList),
	list_to_set(ArgsList,ArgsSet),
	arg_definitions(ArgsSet,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "EXAMPLE(~w, ~w) & ~w~w.")', [Id,LPred,Pred,RPred,ArgsText]),
	!.

write_simplified_inf_relation(Left,Right,[Rel,LId,_RId],Id,Pretty) :-
	stripped_id(Left,LId),
	simplified_inf_pred(Left,null,LPred,LArgs), % LHS
	upcase_atom(Rel,Relation),
	simplified_inf_pred(Right,null,RPred,RArgs), % RHS
	append([LArgs,RArgs],ArgsList),
	list_to_set(ArgsList,Args),
	arg_definitions(Args,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "~w(~w, ~w)~w.")', [Id,Relation,LPred,RPred,ArgsText]),
	!.
write_simplified_inf_relation(Right,Left,[Rel,LId,_RId],Id,Pretty) :-
	stripped_id(Left,LId),
	simplified_inf_pred(Left,null,LPred,LArgs), % LHS
	upcase_atom(Rel,Relation),
	simplified_inf_pred(Right,null,RPred,RArgs), % RHS
	append([LArgs,RArgs],ArgsList),
	list_to_set(ArgsList,Args),
	arg_definitions(Args,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "~w(~w, ~w)~w.")', [Id,Relation,LPred,RPred,ArgsText]),
	!.
write_simplified_inf_relation(_,_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).

arg_definitions([],_,'').
arg_definitions([Focus],Focus,'U').
arg_definitions([[_Prep,Arg]|Rest],Focus,ArgText) :- !,
	arg_definitions([Arg|Rest],Focus,ArgText).
arg_definitions([Arg],Focus,ArgText) :- !,
	simplified_lemma(Arg,null,Lemma),
	simplified_string(Arg,Focus,String),
	atomic_list_concat([', ',Lemma,' = ',String],ArgText).
arg_definitions([Arg|RestArgs],Focus,ArgsText) :-
	arg_definitions([Arg],Focus,ArgText),
	arg_definitions(RestArgs,Focus,RestArgsText),
	atomic_list_concat([ArgText,RestArgsText],ArgsText).

simplified_inf_pred(Root-_,Focus,Pred,Args) :-
	simplified_inf_pred(Root,Focus,Pred,Args).
simplified_inf_pred(Root,Focus,Pred,Args) :-
	rdf(Root,rdf:type,event),
	simplified_lemma(Root,Focus,Lemma),
	simplified_inf_args(Root,Focus,ArgText,Args),
	atomic_list_concat([Lemma,ArgText],Pred).
simplified_inf_pred(Root,Focus,Lemma,[Root]) :-
	simplified_lemma(Root,Focus,Lemma).
	
simplified_inf_args(Root,Focus,ArgStringOut,Args) :-
	findall(Arg,simplified_inf_arg(Root,Arg),Args),
	Args \= [],
	format_simplified_inf_args(Args,Focus,'',ArgString,Prep),
	atomic_list_concat([Prep,'(',ArgString,')'],ArgStringOut).
simplified_inf_args(_,_,'',[]).

simplified_inf_arg(Root,Arg) :-
	( dependency(Subj,dep:partmod,Root)
	; dependency(Subj,dep:rcmod,Root) ),
	rdf(Arg,pred:isa,Subj).
simplified_inf_arg(Root,Arg) :-
	rdf(Root,pred:agent,Arg),
	\+ ( ( dependency(Subj,dep:partmod,Root)
	     ; dependency(Subj,dep:rcmod,Root) ),
	     rdf(_,pred:isa,Subj) ).
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
format_simplified_inf_args([[Prep,Arg]|Rest],Focus,Prefix,ArgString,'') :-
%	simplified_inf_pred(Arg,Focus,ArgText,_SubArgs),
	simplified_lemma(Arg,Focus,ArgText),
	format_simplified_inf_args(Rest,Focus,', ',RestArgString,_PrepOut),
	atomic_list_concat([Prefix,Prep,'(',ArgText,')',RestArgString],ArgString).
format_simplified_inf_args([Arg|Rest],Focus,Prefix,ArgString,Prep) :-
%	simplified_inf_pred(Arg,Focus,ArgText,_SubArgs),
	simplified_lemma(Arg,Focus,ArgText),
	format_simplified_inf_args(Rest,Focus,', ',RestArgString,Prep),
	atomic_list_concat([Prefix,ArgText,RestArgString],ArgString).

simplified_lemma(Arg,_,Lemma) :- % non-question
	rdf(Arg,rdf:type,event),
	simplified_verb_tokens(Arg,Tokens),
	lemmas_text(Tokens,Lemma),
	!.
simplified_lemma(Focus,Focus,'U').
simplified_lemma(Arg,nofocus,Lemma) :-
	simplified_string(Arg,nofocus,Lemma).
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


simplified_string(Arg,nofocus,Lemma) :-
	rdf(Arg,rdf:type,event),
	rdf(Arg,pred:isa,literal(String)),
	atomic_list_concat(['',Lemma,''],'"',String),
	!.
simplified_string(Arg,nofocus,Lemma) :-
	arg_tokens(Arg,Tokens),
	tokens_text_single_quoted(Tokens,Lemma), !.
simplified_string(Arg,_,'U') :-
	current_question_focus(Arg), !.
simplified_string(Arg,_,'U') :-
	current_question_focus(Focus),
	( dependency(Arg,dep:partmod,Focus)
	; dependency(Arg,dep:rcmod,Focus) ),
	!.
simplified_string(Arg,_,Lemma) :-
	arg_tokens(Arg,Tokens),
	tokens_text_single_quoted(Tokens,Lemma).


%%% simplified form of basic sentence (no relation)

write_simplified_inf_simple_tuple(_Entity,Root,Id,Pretty) :-
	simplified_inf_pred(Root,null,Pred,ArgsList),
	list_to_set(ArgsList,Args),
	arg_definitions(Args,null,ArgsText),
	format(atom(Pretty), 'pretty(~w, "~w~w.")', [Id,Pred,ArgsText]),
	!.
write_simplified_inf_simple_tuple(_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).


%%% simplified form of questions

write_simplified_inf_question(LeftTriples,RightTriples,Id,Pretty) :-
	current_question_focus(Focus),
	% setup
	write_simplified_triples(LeftTriples,Focus,LeftPred,LeftArgs),
	( (LeftPred = '', Prefix = '')
	; Prefix = ' & '),
	% relation
	write_simplified_question_relation(RightTriples,Focus,Prefix,Relation,RelArgs),
	% focus
	simplified_focus_pred(Focus,FocusText,FocusArgs),
	% variable values
	append([RelArgs,LeftArgs,FocusArgs],EventArgs),
	list_to_set(EventArgs,Args),
	arg_definitions(Args,nofocus,ArgsText),
	format(atom(Pretty), 'pretty(~w, "~w~w, U = ~w~w.")', [Id,LeftPred,Relation,FocusText,ArgsText]),
	!.
write_simplified_inf_question(_,_,Id,Pretty) :- % failed
	format(atom(Pretty), 'pretty(~w, "")', [Id]).

simplified_focus_pred(Focus,FocusText,FocusArgs) :-
	dependency(Focus,dep:partmod,Verb),
	simplified_inf_pred(Verb,null,FocusText,FocusArgs).
simplified_focus_pred(Focus,FocusText,FocusArgs) :-
	dependency(Focus,dep:rcmod,Verb),
	simplified_inf_pred(Verb,null,FocusText,FocusArgs).
simplified_focus_pred(Focus,FocusText,FocusArgs) :-
	rdf(Focus,rdf:type,event),
	simplified_inf_pred(Focus,null,FocusText,FocusArgs).
simplified_focus_pred(Focus,Lemma,[]) :-
	simplified_lemma(Focus,nofocus,Lemma).

write_simplified_question_relation(Triples,Focus,Prefix,Relation,Args) :-
	member([Left,R,Right],Triples),
	rdf_global_id(rel:Rel,R), !,
	focus_lemma(Left,Focus,LeftLemma,LArgs),
	focus_lemma(Right,Focus,RightLemma,RArgs),
	append([LArgs,RArgs],Args),
	upcase_atom(Rel,RelName),
	format(atom(Relation), '~w~w(~w, ~w)', [Prefix,RelName,LeftLemma,RightLemma]).
write_simplified_question_relation(_,_,_,'',[]).

write_simplified_triples(Triples,Focus,EventPreds,EventArgs) :-
	findall(Event,
		(member([Event|_],Triples),
		 Event \= Focus,
		 rdf(Event,rdf:type,event),
		 % not in relation
		 \+ ( (rdf(Event,R,_) ; rdf(_,R,Event) ),
		      rdf_global_id(rel:_,R) )
		 ),
		Events),
	Events \= [],
	list_to_set(Events,EventSet),
	write_simplified_events(EventSet,Focus,EventPreds,EventArgs).
write_simplified_triples(_,_,'',[]).

write_simplified_events([],_Focus,'',[]).
write_simplified_events([Event],Focus,Pred,Args) :- !,
	simplified_inf_pred(Event,Focus,Pred,Args).
write_simplified_events([Event|Rest],Focus,Preds,Args) :-
	simplified_inf_pred(Event,Focus,Pred,EventArgs),
	write_simplified_events(Rest,Focus,RestPreds,RestArgs),
	atomic_list_concat([Pred,' & ',RestPreds],Preds),
	append([EventArgs,RestArgs],Args).

focus_lemma(Focus,Focus,'U',[]) :- !.
focus_lemma(Arg,Focus,'U',[]) :-
	dependency(Focus,dep:partmod,Arg), !.
focus_lemma(Arg,Focus,'U',[]) :-
	dependency(Focus,dep:rcmod,Arg), !.
focus_lemma(Arg,Focus,Lemma,Args) :-
	simplified_inf_pred(Arg,Focus,Lemma,Args).
