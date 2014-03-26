
:- [expand].
:- [tuple].
:- [output].
:- [denominalizations].


relation :-
	% pick a sentence
	rdf(_Sentence,dep:root,Root),
%	write('processing: '), write(Root), nl,
%	write_sentence(Root),
	% descend through every node checking for relations
	( top_relation(Root)
	; constit(Root,Node),
	  relation(Node) ).

% find related tuples
top_relation(Root) :-
	findall(Root,relation(Root),Roots),
	Roots \= [], !.
% else write simple top-level tuple
top_relation(Root) :-
	argument(Root,dep:nsubj,Subj), Subj \= [],
	argument(Root,dep:dobj,Obj), Obj \= [],
	\+ helps(Root),
	write_simple_tuple(Root).


% cause (tuple-NP)
relation(Root) :-
	cause(Root,Entity,Rel),
	tuple(Root,Tuple),
	write_relation(Entity,Rel,Tuple).
% purpose (tuple-NP)
relation(Root) :-
	purpose(Root,Entity,Rel),
	tuple(Root,Tuple),
	\+ filter_tuple(Root,Tuple),
	\+ filter_entity(Entity),
	write_relation(Tuple,Rel,Entity).
% effect (tuple-tuple)
relation(Root) :-
	effect(Root,Comp,Rel),
	tuple(Root,Tuple1),
	\+ filter_tuple(Root,Tuple1),
	tuple(Comp,Tuple2),
	distribute_args(Tuple1,Tuple2,Tuple1Out,Tuple2Out),
	write_relation(Tuple1Out,Rel,Tuple2Out).
% function (NP-tuple)
relation(Root) :-
	function(Root,Comp,Rel),
	\+ dependency(_,dep:prep_for,Comp), % responsible for
	tuple(Comp,Tuple),
	distribute_args([Root],Tuple,_,TupleOut),
	denominalize(Root,RootTuple),
	write_relation(RootTuple,Rel,TupleOut).
% function (NP-NP)
relation(Root) :-
	function(Root,Comp,Rel),
	dependency(_,dep:prep_for,Comp), % responsible for
	denominalize(Root,RootTuple),
	denominalize(Comp,CompTuple),
	write_relation(RootTuple,Rel,CompTuple).
% example (NP-tuple)
relation(Root) :-
	example_NP_Tuple(Root,Comp,Rel),
	tuple(Comp,[Subj|Rest]),
	Subj \= Root, % from rcmod
	write_entity_relation(Root,Rel,[Subj|Rest]).
% example (tuple-NP)
relation(Root) :-
	example_Tuple_NP(Root,Comp,Rel),
	tuple(Comp,Tuple),
	write_entity_relation(Tuple,Rel,Root).
% example (NP-NP)
relation(Root) :-
	example_NP_NP(Root,Entity,Rel),
	write_entity_relation(Root,Rel,Entity).


% synonym sets
helps(Help) :-
	lemma(Help,Lemma),
	memberchk(Lemma, [help,
			  aid,
			  allow,
			  assist,
			  enable
			 ]).
causes(Cause) :-
	lemma(Cause,Lemma),
	memberchk(Lemma, [cause
/* % synonyms from Girju & Moldovan (2002) "Text Mining for Causal Relations"
                          induce,
                          %give rise (to),
                          produce,
                          generate,
                          effect,
                          %bring about,
                          provoke,
                          arouse,
                          elicit,
                          lead,% (to),
                          trigger,
                          derive,% (from),
                          associate,% (with),
                          relate,% (to),
                          link,% (to),
                          stem,% (from),
                          originate,
                          %bring forth,
                          %lead up,
                          %trigger off,
                          %bring on,
                          result% (from)
*/
		      ]).



% exclude cases
filter_tuple(Root,_) :-
	\+ rdf(Root,basic:cop,_),
	\+ rdf(Root,dep:aux,_),
	\+ rdf(Root,dep:dobj,_),
	\+ rdf(_,dep:xcomp,Root),
	pos(Root,Pos),
	atom_concat('NN',_,Pos). % nominal as verb
filter_tuple(Root,_) :-
	rdf(Root,basic:cop,_),
	lemma(Root,Token),
	member(Token,[
		      ability,
		      able,
		      necessary,
		      responsible,
		      similar
		     ]).
filter_tuple(Root,_) :-
	rdf(Root,basic:cop,_),
	rdf(Root,basic:nsubj,Subj),
	lemma(Subj,it), % pleonastic
	lemma(Root,Token),
	member(Token,[
		      necessary,
		      nice,
		      possible
		     ]).
filter_tuple(Root,[[]|_]) :- % empty subject
	rdf(Root,basic:nsubjpass,_), !,
	text(Root,Token),
	member(Token,[
		      called,
		      committed,
		      considered,
		      used
		     ]).
filter_tuple(Root,_) :-
	lemma(Root,Token),
	member(Token,[
		      be,
		      cause
		     ]).
filter_tuple(Root,[S,V]) :- % empty object
	filter_tuple(Root,[S,V,[]]).
filter_tuple(Root,[_,_,[]|_]) :- % empty object
	helps(Root).
filter_tuple(Root,[_,_,[]|_]) :- % empty object
	lemma(Root,Token),
	member(Token,[
		      appear,
		      avoid,
		      begin,
		      change,
		      continue,
		      do,
		      go,
		      have,
		      include,
		      involve,
		      make,
		      need,
		      result,
		      take,
		      try,
		      use,
		      want,
		      wish
		     ]).
filter_tuple(_,[_,_,Obj|_]) :-
	pos(Obj,'WRB'). % 'why'


filter_entity(Entity) :-
	lemma(Entity,Lemma),
	wn-denom(Lemma,_Verb),
	!, fail.
filter_entity(Entity) :-
	rdf(Entity,dep:det,_).
filter_entity(Entity) :-
	rdf(Entity,dep:num,_).
filter_entity(Entity) :-
	rdf(Entity,dep:poss,_).


% don't use subject from parent clause if already object
distribute_args([Subj1|Rest],[[],V2,Obj2|Rest2], [Subj1|Rest],[[],V2,Obj2|Rest2]) :-
	(Subj1 = Obj2
	; (rdf(A,coref:ref,Subj1),
	   rdf(A,coref:ref,Obj2))), !.
% don't use subject from child clause if already object
distribute_args([[],V1,Obj1|Rest],[Subj2|Rest2], [[],V1,Obj1|Rest],[Subj2|Rest2]) :-
	(Subj2 = Obj1
	; (rdf(A,coref:ref,Subj2),
	   rdf(A,coref:ref,Obj1))), !.
% use subject from parent clause
distribute_args([S|Rest],[[],V2|Rest2], [S-x|Rest],[S-x,V2|Rest2]) :-
	\+ rdf(V2,dep:auxpass,_), % not passive
	S \= [], !.
% use subject from child clause
distribute_args([[]|Rest],[S2|Rest2], [S2-x|Rest],[S2-x|Rest2]) :-
	S2 \= [], !.
% find shared values
distribute_args([S1,V1,O1|Rest1],[S1,V2,O1|Rest2], [S1-x,V1,O1-y|Rest1],[S1-x,V2,O1-y|Rest2]) :-
	S1 \= [], O1 \= [], !.
distribute_args([S1,V1,O1|Rest1],[S1,V2,O2|Rest2], [S1-x,V1,O1|Rest1],[S1-x,V2,O2|Rest2]) :-
	S1 \= [], !.
distribute_args([S1,V1,O1|Rest1],[S2,V2,O1|Rest2], [S1,V1,O1-y|Rest1],[S2,V2,O1-y|Rest2]) :-
	O1 \= [], !.
% write unchanged
distribute_args(Action,Purpose, Action,Purpose).


