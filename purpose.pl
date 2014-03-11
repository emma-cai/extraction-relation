
:- [expand].
:- [denominalizations].
:- [tuple].
:- [output].

:- rdf_register_prefix(mod, 'http://halo.vulcan.com/mod/').


%:- rdf_load('barrons.txt.rnn.ttl').


purpose :-
	% pick a sentence
	rdf(_Sentence,dep:root,Root),
%	write('processing: '), write(Root), nl,
%	purpose(Root).
%	write_sentence(Root),
	% descend through every node checking for relations
	( purpose(Root)
	; constit(Root,Node),
	  purpose(Node) ).


% cause (tuple-NP)
purpose(Root) :-
	cause(Root,Entity,Rel),
	tuple(Root,Action),
	write_relation(Entity,'CAUSE',Action).
% purpose (tuple-NP)
purpose(Root) :-
	purpose(Root,Entity,Rel),
	tuple(Root,Action),
	\+ filter_action(Root,Action),
	\+ rdf(Entity,dep:det,_),
	\+ rdf(Entity,dep:num,_),
	\+ rdf(Entity,dep:poss,_),
	write_relation(Action,'PURPOSE',Entity).
% effect (tuple-tuple)
purpose(Root) :-
	effect(Root,Comp,Rel),
	tuple(Root,Action),
	\+ filter_action(Root,Action),
	tuple(Comp,Purpose),
	distribute_args(Action,Purpose,Action2,Purpose2),
	write_relation(Action2,'EFFECT',Purpose2).
% function (NP-tuple)
purpose(Root) :-
	function(Root,Comp,Rel),
	\+ rdf(_,dep:prep_for,Comp), % responsible for
	tuple(Comp,Purpose),
	distribute_args([Root],Purpose,_,Purpose2),
	write_relation(Root,'FUNCTION',Purpose2).
% function (NP-NP)
purpose(Root) :-
	function(Root,Comp,Rel),
	rdf(_,dep:prep_for,Comp), % responsible for
	write_relation(Root,'FUNCTION',Comp).
% example (NP-tuple)
purpose(Root) :-
	example(Root,Comp,Rel),
	tuple(Comp,Action),
	write_entity(Root),
	write('\tEXAMPLE\t'),
	write_tuple(Action),
	nl.
% example (tuple-NP)
purpose(Root) :-
	example2(Root,Comp,Rel),
	tuple(Comp,Action),
	write_tuple(Action),
	write('\tEXAMPLE\t'),
	write_entity(Root),
	nl.
% example (NP-NP)
purpose(Root) :-
	example3(Root,Entity,Rel),
	write_entity(Root),
	write('\tEXAMPLE\t'),
	write_entity(Entity),
	nl.

write_relation(Action,Rel,Purpose) :-
	write_tuple(Action),
	write('\t'),
	%write_tokens(Rel),
	write(Rel),
	write('\t'),
	write_tuple(Purpose),
	nl.


% EXAMPLE: NP is example (of how) Tuple
example(Entity,Comp,[Example]) :-
	rdf(Entity,dep:cop,_),
	rdf(Entity,dep:nsubj,Example),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:ccomp,Comp).
% EXAMPLE: NP is example of Tuple
example(Entity,Comp,Rel) :-
	example3(Entity,Entity2,Rel),
	( rdf(Entity2,dep:partmod,Comp)
	; rdf(Entity2,dep:rcmod,Comp) ).

% EXAMPLE: Tuple is example of NP
example2(Entity,Comp,[Example]) :-
	rdf(Example,dep:prep_of,Entity),
	rdf(Example,token:lemma,literal(example)),
	rdf(Be,dep:nsubj,Example),
	rdf(Be,token:lemma,literal(be)),
	rdf(Be,dep:advcl,Comp).
example2(Entity,Comp,[Example]) :-
	rdf(Example,dep:prep_of,Entity),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:cop,_),
	rdf(Example,dep:csubj,Comp).
% EXAMPLE: NP example includes Tuple
example2(Entity,Comp,[Example]) :-
	rdf(Example,dep:nn,Entity),
	rdf(Example,token:lemma,literal(example)),
	rdf(Include,dep:nsubj,Example),
	rdf(Include,token:lemma,literal(include)),
	rdf(Include,dep:ccomp,Comp).
example2(Entity,Comp,[Example]) :-
	rdf(Example,dep:nn,Entity),
	rdf(Example,token:lemma,literal(example)),
	rdf(Include,dep:nsubj,Example),
	rdf(Include,token:lemma,literal(include)),
	rdf(Include,dep:conj_and,Comp).

% EXAMPLE: NP is a NP
example3(Entity1,Entity2,[Example]) :-
	rdf(Entity2,dep:cop,_),
	rdf(Entity2,dep:det,_),
	rdf(Entity2,dep:nsubj,Entity1),
	\+ rdf(Entity2,token:lemma,literal(example)).
% EXAMPLE: NP such as NP
example3(Entity1,Entity2,['such as']) :-
	rdf(Entity2,dep:prep_such_as,Entity1).
% EXAMPLE: NP is example of NP
example3(Entity1,Entity2,[Example]) :-
	rdf(Entity1,dep:cop,_),
	rdf(Entity1,dep:nsubj,Example),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:prep_of,Entity2).
example3(Entity1,Entity2,[Example]) :-
	rdf(Example,dep:nsubj,Entity1),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:cop,_),
	rdf(Example,dep:prep_of,Entity2).
example3(Entity1,Entity2,[Example]) :-
	rdf(Ent,dep:conj_and,Entity1),
	rdf(Ent,dep:cop,_),
	rdf(Ent,dep:nsubj,Example),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:prep_of,Entity2).
% failed cop
example3(Entity1,Entity2,[Example]) :-
	rdf(Be,dep:nsubj,Entity1),
	rdf(Be,token:lemma,literal(be)),
	rdf(Be,dep:tmod,Example),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:prep_of,Entity2).
% EXAMPLE: example of NP includes NP
example3(Entity1,Entity2,[Example]) :-
	rdf(Include,dep:dobj,Entity1),
	rdf(Include,token:lemma,literal(include)),
	rdf(Include,dep:nsubj,Example),
	rdf(Example,token:lemma,literal(example)),
	rdf(Example,dep:prep_of,Entity2).


% CAUSE: NP "causes" Tuple
cause(Root,Entity,[Cause]) :-
	rdf(Cause,dep:xcomp,Root),
	causes(Cause),
	( rdf(Entity,dep:rcmod,Cause)
	; rdf(Cause,dep:nsubj,Entity) ), !.
% CAUSE: Tuple "because of" NP
cause(Root,Entity,['because of']) :-
	rdf(Root,dep:prep_because_of,Entity).
/*
% NP "results from" Tuple
cause2(Root,Entity,['results from']) :-
	rdf(Result,dep:prepc_from,Root),
	rdf(Result,token:lemma,literal(result)),
	rdf(Result,dep:nsubj,Entity).
% Tuple "results in" NP
cause2(Root,Entity,['results in']) :-
	rdf(Root,dep:xcomp,Result),
	rdf(Result,token:lemma,literal(result)),
	rdf(Result,dep:prep_in,Entity).
*/	


% PURPOSE: Tuple "is for" NP
purpose(Root,Entity,['for']) :-
	rdf(Root,dep:cop,_),
	rdf(Root,dep:prep_for,Entity).
% PURPOSE: Tuple "for" NP
purpose(Root,Entity,['for']) :- % dobj-PP
	rdf(Root,dep:dobj,Dobj),
	rdf(Dobj,dep:prep_for,Entity).
purpose(Root,Entity,['for']) :- % pobj-PP
	\+ rdf(Root,dep:dobj,_),
	rdf(Root,basic:prep,Prep),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,dep:prep_for,Entity).


% copular
function(Entity,Comp,Rel) :-
	rdf(Root,dep:nsubj,Entity),
	rdf(Root,dep:cop,_),
	function(Root,Comp,Rel).
% FUNCTION: NP "helps" Tuple
function(Entity,Comp,[Help]) :-
	rdf(Help,dep:nsubj,Entity),
	\+ rdf(_,dep:rcmod,Help),
	helps(Help),
	( rdf(Help,dep:ccomp,Comp)
	; rdf(Help,dep:xcomp,Comp) ).
function(Entity,Comp,[Help]) :-
	rdf(Entity,dep:rcmod,Help),
	helps(Help),
	( rdf(Help,dep:ccomp,Comp)
	; rdf(Help,dep:xcomp,Comp) ).
% FUNCTION: NP "helps in" Tuple
function(Entity,Comp,[Help]) :-
	rdf(Help,dep:nsubj,Entity),
	\+ rdf(_,dep:rcmod,Help),
	helps(Help),
	rdf(Help,dep:prepc_in,Comp),
	rdf(Comp,token:pos,literal('VBG')).
% FUNCTION: NP "is used to" Tuple
function(Entity,Comp,[Aux,Rel]) :-
	rdf(Rel,dep:nsubjpass,Entity),
	rdf(Rel,dep:auxpass,Aux),
	rdf(Rel,token:lemma,literal(use)),
	\+ rdf(_,dep:rcmod,Rel),
	rdf(Rel,dep:xcomp,Comp).
function(Entity,Comp,[Aux,Rel]) :-
	rdf(Entity,dep:cop,_),
	rdf(Entity,dep:nsubj,Subj),
	rdf(Subj,dep:infmod,Rel),
	rdf(Rel,token:lemma,literal(use)),
	rdf(Rel,dep:xcomp,Comp),
	aux(Rel,Aux).
function(Entity,Comp,[Rel,Aux]) :-
	rdf(Entity,dep:partmod,Rel),
	rdf(Rel,token:lemma,literal(use)),
	\+ rdf(_,dep:rcmod,Rel),
	rdf(Rel,dep:xcomp,Comp),
	aux(Comp,Aux).
% FUNCTION: NP "to" Tuple
function(Entity,Comp,[]) :-
	rdf(Entity,dep:nsubj,Subj),
	\+ rdf(Entity,dep:cop,_),
	\+ rdf(Entity,token:lemma,literal(be)),
	rdf(Subj,dep:infmod,Comp),
	\+ rdf(Comp,token:lemma,literal(use)).
% FUNCTION: NP "measures" NP
function(Entity,Measure,[]) :-
	rdf(Measure,dep:nsubj,Entity),
	rdf(Measure,token:lemma,literal(measure)).
% FUNCTION: NP "by which" Tuple
function(Entity,Comp,[By,Which]) :-
	rdf(Root,dep:nsubj,Entity),
	rdf(Root,dep:cop,_),
	rdf(Root,dep:rcmod,Comp),
	rdf(Comp,basic:prep,By),
	rdf(By,token:lemma,literal(by)),
	rdf(By,basic:pobj,Which),
	rdf(Which,token:lemma,literal(which)).
% FUNCTION: "function of" NP is Tuple
function(Entity,Comp,[Function]) :-
	rdf(Function,dep:prep_of,Entity),
	rdf(Function,token:lemma,literal(function)),
	rdf(Comp,dep:nsubj,Function),
	\+ rdf(Comp,token:lemma,literal(be)).
function(Entity,Comp,[Function]) :-
	rdf(Function,dep:prep_of,Entity),
	rdf(Function,token:lemma,literal(function)),
	rdf(Be,dep:nsubj,Function),
	rdf(Be,token:lemma,literal(be)),
	rdf(Be,dep:xcomp,Comp).
% FUNCTION: NP "is responsible for" Tuple
function(Entity,Comp,[Responsible]) :-
	rdf(Responsible,dep:nsubj,Entity),
	rdf(Responsible,token:lemma,literal(responsible)),
	rdf(Responsible,dep:cop,_),
	rdf(Responsible,dep:prepc_for,Comp).
% FUNCTION: NP "is responsible for" NP
function(Entity,Comp,[Responsible]) :-
	rdf(Responsible,dep:nsubj,Entity),
	rdf(Responsible,token:lemma,literal(responsible)),
	rdf(Responsible,dep:cop,_),
	rdf(Responsible,dep:prep_for,Comp).
	

%%%effect(Root,Comp,[Aux]) :-
%%%	argument(Root,dep:purpcl,Comp), % Sapir
%%%	Comp \= [],
%%%	aux(Comp,Aux).
%%%effect(Root,Comp,[]) :-
%%%	argument(Root,dep:advcl,Comp), % Sapir
%%%	Comp \= [].


% EFFECT: Tuple "in preparation for" Tuple
effect(Root,Comp,[]) :-
	rdf(Root,dep:prep_in,Comp),
	rdf(Comp,token:lemma,literal(preparation)),
	rdf(Comp,dep:prep_for,_).

% EFFECT: Tuple "helps to" Tuple
effect(Root,Comp,[Aux,Help]) :-
	rdf(Root,dep:xcomp,Help),
	helps(Help), !,
	aux(Help,Aux),
	( rdf(Help,dep:xcomp,Comp)
	; rdf(Help,dep:ccomp,Comp)).
effect(Root,Comp,[Aux,Help]) :- % infmod on dobj
	rdf(Root,dep:dobj,Dobj),
	rdf(Dobj,dep:infmod,Help),
	helps(Help),
	rdf(Help,dep:ccomp,Comp),
	aux(Help,Aux).
effect(Root,Comp,[Aux,Help]) :- % infmod on pobj
	rdf(Root,basic:prep,Prep),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,dep:infmod,Help),
	helps(Help),
	rdf(Help,dep:ccomp,Comp),
	aux(Help,Aux).
% EFFECT: Tuple "in order to" Tuple
effect(Root,Comp,[Prep,Pobj,Aux]) :- % xcomp on pobj
	rdf(Root,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(in)),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,token:lemma,literal(order)),
	rdf(Root,dep:xcomp,Comp),
	\+ rdf(Pobj,dep:xcomp,_), !,
	aux(Comp,Aux).
effect(Root,Comp,[Prep,Pobj,Aux]) :- % infmod on dobj
	rdf(Root,dep:dobj,Dobj),
	rdf(Dobj,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(in)),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,token:lemma,literal(order)),
	rdf(Dobj,dep:infmod,_), !,
	rdf(Dobj,dep:infmod,Comp),
	\+ rdf(Pobj,dep:xcomp,_),
	aux(Comp,Aux).
effect(Root,Comp,[Mark,Dep,Aux]) :- % advcl
	( rdf(Root,dep:advcl,Comp)
	; rdf(Root,dep:dep,Comp)),
	mark(Comp,Mark),
	rdf(Mark,token:lemma,literal(in)),
	rdf(Comp,dep:dep,Dep),
	rdf(Dep,token:lemma,literal(order)),
	aux(Comp,Aux),
	rdf(Aux,token:lemma,literal(to)).
% EFFECT: Tuple "to" Tuple
effect(Root,Comp,[Aux]) :-
	rdf(Root,dep:xcomp,Comp),
	\+ rdf(_,dep:rcmod,Root),
	\+ rdf(Comp,dep:cop,_),
	aux(Comp,Aux).
effect(Root,Comp,[Aux]) :-
	rdf(Root,dep:xcomp,Comp),
	\+ rdf(_,dep:rcmod,Root),
	rdf(Comp,dep:cop,Cop),
	\+ rdf(Cop,token:lemma,literal(be)),
	aux(Comp,Aux).
effect(Root,Comp,[]) :-
	rdf(Root,dep:xcomp,Comp),
	rdf(Comp,token:pos,literal('VBG')),
	\+ rdf(Comp,dep:ccomp,_).
effect(Root,Comp,[Aux]) :- % infmod on dobj
	rdf(Root,dep:dobj,Dobj),
	rdf(Dobj,dep:infmod,Comp),
	\+ rdf(Dobj,token:lemma,literal(ability)),
	\+ helps(Comp),
	aux(Comp,Aux).
effect(Root,Comp,[Prep]) :- % infmod on pobj
	rdf(Root,basic:prep,Prep),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,dep:infmod,Comp),
	\+ helps(Comp).
effect(Root,Comp,[Mod|Aux]) :- % rcmod on dobj
	\+ rdf(Root,dep:cop,_),
	rdf(Root,basic:dobj,Dobj),
	rdf(Dobj,dep:rcmod,Mod),
	rdf(Mod,dep:ccomp,Comp),
	(aux(Comp,Aux) ; Aux = []). % optional aux
effect(Root,Comp,[Mod|Aux]) :- % rcmod on dobj
	\+ rdf(Root,dep:cop,_),
	rdf(Root,basic:dobj,Dobj),
	rdf(Dobj,dep:rcmod,Mod),
	rdf(Mod,dep:xcomp,Comp),
	aux(Comp,Aux). % required aux
effect(Root,Comp,[Mod|Aux]) :- % rcmod on pobj
	\+ rdf(Root,dep:cop,_),
	rdf(Root,basic:prep,Prep),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,dep:rcmod,Mod),
	rdf(Mod,dep:ccomp,Comp),
	(aux(Comp,Aux) ; Aux = []). % optional aux
% EFFECT: Tuple "by" Tuple
effect(Root,Comp,[]) :- % by-VBG
	rdf(Prep,basic:pcomp,Root),
	rdf(Prep,token:lemma,literal(by)),
	rdf(Root,token:pos,literal('VBG')),
	rdf(Comp,basic:prep,Prep).
effect(Root,Comp,[]) :- % passive by-VBG
	rdf(Comp,dep:agent,Root),
	rdf(Comp,token:pos,literal('VBG')),
	rdf(Comp,dep:auxpass,_).
% EFFECT: Tuple "in" Tuple
effect(Root,Comp,[Prep]) :-
	rdf(Root,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(in)),
	rdf(Prep,basic:pcomp,Comp),
	rdf(Comp,token:pos,literal('VBG')).
% EFFECT: Tuple "is for" Tuple
effect(Root,Comp,[Mark]) :-
	rdf(Root,dep:cop,_),
	rdf(Root,dep:advcl,Comp),
	mark(Comp,Mark),
	rdf(Mark,token:lemma,literal(for)), !.
% EFFECT: Tuple "for there" Tuple
effect(Root,Comp,[Mark]) :-
	rdf(Root,dep:advcl,Comp),
	mark(Comp,Mark),
	rdf(Mark,token:lemma,literal(for)),
	rdf(Comp,dep:expl,Expl),
	rdf(Expl,token:lemma,literal(there)).
% EFFECT: Tuple "for" Tuple
effect(Root,Comp,[Aux]) :-
	rdf(Root,dep:advcl,Comp),
	mark(Comp,Mark),
	rdf(Mark,token:lemma,literal(for)),
	\+ rdf(Comp,dep:expl,_),
	rdf(Comp,dep:aux,Aux),
	rdf(Aux,token:lemma,literal(to)).
% EFFECT: Tuple "because" Tuple
effect(Root,Comp,[Mark]) :-
	rdf(Comp,dep:advcl,Root),
	mark(Root,Mark),
	rdf(Mark,token:lemma,literal(because)).
% EFFECT: "As" Tuple Tuple
effect(Root,Comp,[Mark]) :-
	rdf(Comp,dep:advcl,Root),
	mark(Root,Mark),
	rdf(Mark,token:text,literal('As')).
% EFFECT: "If" Tuple Tuple
effect(Root,Comp,[Mark]) :-
	rdf(Comp,dep:advcl,Root),
	mark(Root,Mark),
	rdf(Mark,token:text,literal('If')).
% EFFECT: Tuple "because of" Tuple
effect(Root,Comp,[Mwe,Prep]) :-
	rdf(Pobj,dep:rcmod,Root),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Prep,token:lemma,literal(of)),
	rdf(Prep,basic:mwe,Mwe),
	rdf(Mwe,token:lemma,literal(because)),
	rdf(Comp,basic:prep,Prep).
% EFFECT: Tuple "when" Tuple
effect(Root,Comp,[Adv]) :-
	rdf(Comp,dep:advcl,Root),
	( rdf(Root,dep:advmod,Adv) ; rdf(Root,dep:mark,Adv) ), % for Sapir
	rdf(Adv,token:lemma,literal(when)).
% EFFECT: Tuple "in order for" Tuple
effect(Root,Comp,[Prep,Pobj,Prep2]) :-
	rdf(Root,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(in)),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,token:lemma,literal(order)),
	rdf(Pobj,dep:infmod,Comp),
	( rdf(Pobj,basic:prep,Prep2)
	; rdf(Comp,basic:mark,Prep2) ),
	rdf(Prep2,token:lemma,literal(for)).
effect(Root,Comp,[Prep,Pobj,Prep2]) :- % via xcomp
	rdf(Root2,dep:xcomp,Root),
	rdf(Root2,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(in)),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,token:lemma,literal(order)),
	rdf(Pobj,dep:infmod,Comp),
	( rdf(Pobj,basic:prep,Prep2)
	; mark(Comp,Prep2) ),
	rdf(Prep2,token:lemma,literal(for)).
% EFFECT: Tuple "and helps" Tuple
effect(Root,Comp,[Csubj]) :-
	rdf(Root,dep:conj_and,Comp),
	rdf(Comp,dep:csubj,Csubj),
	helps(Csubj).
effect(Root,Comp,[Help]) :-
	( rdf(Root,dep:conj_and,Help)
	; rdf(Root,dep:ccomp,Help)
	; rdf(Help,dep:csubj,Root) ),
	helps(Help),
	rdf(Help,dep:ccomp,Comp).
% EFFECT: Tuple "to help" Tuple
effect(Root,Comp,[Help]) :-
	rdf(Root,dep:csubj,Help),
	helps(Help),
	rdf(Help,dep:ccomp,Comp).
% EFFECT: Tuple "results in" Tuple
effect(Root,Comp,[Result,Prep]) :-
	rdf(Result,dep:csubj,Root),
	rdf(Result,token:lemma,literal(result)),
	rdf(Result,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(in)),
	rdf(Prep,basic:pcomp,Comp).
% EFFECT: Tuple "so that" Tuple
effect(Root,Comp,[So,Mark]) :-
	rdf(Root,dep:advcl,Adv),
	mark(Adv,Mark),
	rdf(Mark,token:lemma,literal(that)),
	( rdf(Adv,dep:advmod,So) ; rdf(Adv,dep:mark,So)),
	rdf(So,token:lemma,literal(so)), !,
	argument(Root,dep:advcl,Comp). % allow conjuncts
effect(Root,Adv,[So,Mark]) :-
	rdf(Root,dep:dep,Adv), % unknown dep
	mark(Adv,Mark),
	rdf(Mark,token:lemma,literal(that)),
	( rdf(Adv,dep:advmod,So) ; rdf(Adv,dep:mark,So)),
	rdf(So,token:lemma,literal(so)), !.
effect(Root,Comp,[So,Mark]) :-
	rdf(Root,dep:advmod,So),
	rdf(So,token:lemma,literal(so)),
	rdf(Root,dep:ccomp,Ccomp),
	mark(Ccomp,Mark),
	rdf(Mark,token:lemma,literal(that)), !,
	argument(Root,dep:ccomp,Comp). % allow conjuncts
% EFFECT: Tuple "so" Tuple
effect(Root,Comp,[Mark]) :-
	rdf(Root,dep:advcl,Comp),
	rdf(Comp,dep:mark,Mark),
	rdf(Mark,token:lemma,literal(so)).
% EFFECT: Tuple "and by doing so" Tuple
effect(Root,Comp,[Prep,Pcomp,So]) :-
	rdf(Root,dep:conj_and,Comp),
	rdf(Comp,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(by)),
	rdf(Prep,basic:pcomp,Pcomp),
	rdf(Pcomp,token:text,literal(doing)),
	( rdf(Pcomp,dep:advmod,So) ; rdf(Pcomp,dep:mark,So)),
	rdf(So,token:lemma,literal(so)).
% EFFECT: Tuple "is a/one/another way that' Tuple
effect(Root,Comp,[Cop,Way]) :-
	rdf(Way,dep:csubj,Root),
	rdf(Way,dep:cop,Cop),
	rdf(Way,token:lemma,literal(way)),
	rdf(Way,dep:rcmod,Comp), !.
effect(Root,Comp,[Cop,Way]) :-
	rdf(Way,dep:nsubj,Root),
	rdf(Way,dep:cop,Cop),
	rdf(Way,token:lemma,literal(way)),
	rdf(Way,dep:infmod,Comp), !.


helps(Help) :-
	rdf(Help,token:lemma,literal(Lemma)),
	memberchk(Lemma, [help,
			  aid,
			  allow,
			  assist,
			  enable
			 ]).
causes(Cause) :-
	rdf(Cause,token:lemma,literal(Lemma)),
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
filter_action(Root,_) :-
	\+ rdf(Root,basic:cop,_),
	\+ rdf(Root,dep:aux,_),
	\+ rdf(Root,dep:dobj,_),
	\+ rdf(_,dep:xcomp,Root),
	rdf(Root,token:pos,literal(Pos)),
	atom_concat('NN',_,Pos). % nominal as verb
filter_action(Root,_) :-
	rdf(Root,basic:cop,_),
	rdf(Root,token:lemma,literal(Token)),
	member(Token,[
		      ability,
		      able,
		      responsible,
		      similar
		     ]).
filter_action(Root,_) :-
	rdf(Root,basic:cop,_),
	rdf(Root,basic:nsubj,Subj),
	rdf(Subj,token:lemma,literal(it)), % pleonastic
	rdf(Root,token:lemma,literal(Token)),
	member(Token,[
		      necessary,
		      nice,
		      possible
		     ]).
filter_action(Root,[[]|_]) :- % empty subject
	rdf(Root,basic:nsubjpass,_), !,
	rdf(Root,token:text,literal(Token)),
	member(Token,[
		      committed,
		      considered,
		      used
		     ]).
filter_action(Root,_) :-
	rdf(Root,token:lemma,literal(Token)),
	member(Token,[
		      be,
		      cause
		     ]).
filter_action(Root,[S,V]) :- % empty object
	filter_action(Root,[S,V,[]]).
filter_action(Root,[_,_,[]|_]) :- % empty object
	helps(Root).
filter_action(Root,[_,_,[]|_]) :- % empty object
	rdf(Root,token:lemma,literal(Token)),
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
filter_action(_,[_,_,Obj|_]) :-
	rdf(Obj,token:pos,literal('WRB')). % 'why'


% aux from any xcomp at same level
aux(Comp,Aux) :-
	rdf(Comp,dep:aux,Aux), !.
aux(Comp,Aux) :-
	rdf(Root,dep:infmod,Comp),
	rdf(Root,dep:infmod,Comp2),
	rdf(Comp2,dep:aux,Aux), !.
aux(Comp,Aux) :-
	rdf(Root,dep:xcomp,Comp),
	rdf(Root,dep:xcomp,Comp2),
	rdf(Comp2,dep:aux,Aux).

mark(Comp,Mark) :-
	rdf(Comp,dep:mark,Mark).
mark(Comp,Mark) :-
	rdf(Comp,dep:conj_and,Comp2),
	rdf(Comp2,dep:mark,Mark).
mark(Comp,Mark) :-
	rdf(Comp2,dep:conj_and,Comp),
	rdf(Comp2,dep:mark,Mark).


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
distribute_args([S|Rest],[[],V2|Rest2], [S|Rest],[S,V2|Rest2]) :-
	\+ rdf(V2,dep:auxpass,_), % not passive
	S \= [], !.
% use subject from child clause
distribute_args([[]|Rest],[S2|Rest2], [S2|Rest],[S2|Rest2]) :-
	S2 \= [], !.
% write unchanged
distribute_args(Action,Purpose, Action,Purpose).


