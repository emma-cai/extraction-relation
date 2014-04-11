
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
	( top_relation(Root,_)
	; constit(Root,Node),
	  relation(Root,Node,_Json) ).

% top level to call from JPL and backtrack for all values
relation(JsonString) :-
	rdf(_Sentence,dep:root,Root),
	( top_relation(Root,Json)
	; constit(Root,Node),
	  relation(Root,Node,Json) ),
	with_output_to(atom(JsonString),
		       json_write(current_output,Json,[width(0)])).

% find top-level related tuples
top_relation(Root,Json) :-
	findall(Json, relation(Root,Root,Json), Jsons),
	Jsons \= [], !,
	member(Json, Jsons).
% else write simple top-level tuple
top_relation(Root,Json) :-
	argument(Root,dep:nsubj,Subj), Subj \= [],
	argument(Root,dep:dobj,Obj), Obj \= [],
	\+ helps(Root),
	write_simple_tuple(Root,Json).


% cause (tuple-NP)
relation(Top,Root,Json) :-
	cause(Root,Entity,Rel),
	tuple(Root,Tuple),
	write_relation(Top,Entity,Rel,Tuple,Json).
% purpose (tuple-NP)
relation(Top,Root,Json) :-
	purpose(Root,Entity,Rel),
	tuple(Root,Tuple),
	\+ filter_tuple(Root,Tuple),
	\+ filter_entity(Entity),
	write_relation(Top,Tuple,Rel,Entity,Json).
% effect (tuple-tuple)
relation(Top,Root,Json) :-
	effect(Root,Comp,Rel),
	tuple(Root,Tuple1),
	\+ filter_tuple(Root,Tuple1),
	tuple(Comp,Tuple2),
	distribute_args(Tuple1,Tuple2,Tuple1Out,Tuple2Out),
	write_relation(Top,Tuple1Out,Rel,Tuple2Out,Json).
% function (NP-tuple)
relation(Top,Root,Json) :-
	function(Root,Comp,Rel),
	\+ dependency(_,dep:prep_for,Comp), % responsible for
	tuple(Comp,Tuple),
	denominalize(Root,RootTuple),
	distribute_args(RootTuple,Tuple,RootTupleOut,TupleOut),
	write_relation(Top,RootTupleOut,Rel,TupleOut,Json).
% function (NP-NP)
relation(Top,Root,Json) :-
	function(Root,Comp,Rel),
	dependency(_,dep:prep_for,Comp), % responsible for
	denominalize(Root,RootTuple),
	denominalize(Comp,CompTuple),
	write_relation(Top,RootTuple,Rel,CompTuple,Json).
% example (NP-tuple)
relation(Top,Root,Json) :-
	example_NP_Tuple(Root,Comp,Rel),
	tuple(Comp,[Subj|Rest]),
	Subj \= Root, % from rcmod
	write_entity_relation(Top,Root,Rel,[Subj|Rest],Json).
% example (tuple-NP)
relation(Top,Root,Json) :-
	example_Tuple_NP(Root,Comp,Rel),
	tuple(Comp,Tuple),
	write_entity_relation(Top,Tuple,Rel,Root,Json).
% example (NP-NP)
relation(Top,Root,Json) :-
	example_NP_NP(Root,Entity,Rel),
	write_entity_relation(Top,Root,Rel,Entity,Json).


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


distribute_args(Entity,Tuple, EntityOut,TupleOut) :-
	atom(Entity), !,
	distribute_args([Entity],Tuple, [EntityOut],TupleOut).
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
distribute_args([S|Rest],[[],V2|Rest2], [S-x|Rest],[S-x-true,V2|Rest2]) :-
	\+ rdf(V2,dep:auxpass,_), % not passive
	S \= [], !.
% use subject from child clause
distribute_args([[]|Rest],[S2|Rest2], [S2-x-true|Rest],[S2-x|Rest2]) :-
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


