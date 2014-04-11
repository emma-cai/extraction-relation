
:- rdf_meta dependency(r,r,-).
:- rdf_meta argument(r,r,-).
:- rdf_meta dep(r,r,-).

:- rdf_register_prefix(mod, 'http://aristo.allenai.org/mod/').

% rdf access
dependency(Arg1,Dep,Arg2) :-
	rdf(Arg1,Dep,Arg2).
text(Token,Value) :-
	rdf(Token,token:text,literal(Value)).
lemma(Token,Value) :-
	rdf(Token,token:lemma,literal(Value)).
pos(Token,Value) :-
	rdf(Token,token:pos,literal(Value)).


% normalize to verb if possible
denominalize(Ent,[Subj,Ent-Verb,Obj]) :-
	atom(Ent),
	lemma(Ent,Lemma),
	( (wn-denom(Lemma,Verb), !)
	; (rdf(Ent,token:pos,literal('VBG')), Verb = Lemma) ),
	argument(Ent,dep:prep_of,Obj),
	argument(Ent,dep:prep_by,Subj),
	!.
denominalize(Ent,Ent).


% copular
tuple(Root,[Subj,Cop,Root|Mods]) :-
	rdf(Root,basic:cop,Cop),
	rdf(Root,dep:expl,Subj), !,
	modifiers(Root,Mods).
tuple(Root,[Subj,Cop,Root|Mods]) :-
	rdf(Root,basic:cop,Cop), !,
	argument(Root,dep:nsubj,Subj),
	modifiers(Root,Mods).
% passive
tuple(Root,[Agent,Root,Subj|Mods]) :-
	rdf(Root,basic:auxpass,_), !,
	argument(Root,dep:agent,Agent),
	argument(Root,dep:nsubjpass,Subj),
	modifiers(Root,Mods).
% active - ditransitive
tuple(Root,[Subj,Root,IObj,DObj|Mods]) :-
	rdf(Root,basic:iobj,_), !,
	argument(Root,dep:iobj,IObj),
	argument(Root,dep:dobj,DObj),
	argument(Root,dep:nsubj,Subj),
	modifiers(Root,Mods).
% active - transitive
tuple(Root,[Subj,Root,Obj|Mods]) :-
	rdf(Root,basic:dobj,_), !,
	argument(Root,dep:dobj,Obj),
	argument(Root,dep:nsubj,Subj),
	modifiers(Root,Mods).
% active - rcmod
tuple(Root,[Subj,Root,Obj|Mods]) :-
	rdf(Obj,basic:rcmod,Root),
	\+ rdf(_,dep:root,Obj), % not clause root, e.g. 'is a way that'
	rdf(Root,dep:nsubj,NSubj),
	NSubj \= Obj, !,
	argument(Root,dep:nsubj,Subj),
	modifiers(Root,Mods).
% active - partmod
tuple(Root,[Subj,Root,Obj|Mods]) :-
	rdf(Obj,basic:partmod,Root), 
	\+ rdf(_,dep:root,Obj), % not clause root, e.g. 'is a way that'
	argument(Root,dep:nsubj,Subj),
	modifiers(Root,Mods).
% active - instransitive
tuple(Root,[Subj,Root,[]|Mods]) :-
%	rdf(Root,token:pos,literal(Pos)),
%	atom_concat('VB',_,Pos),
	argument(Root,dep:nsubj,Subj),
	modifiers(Root,Mods).


% reattach temporal PP to verb
argument(Root,dep:dobj,Arg) :-
	dep(Root,dep:dobj,Arg),
	rdf(Arg,basic:prep,Prep),
	rdf(Prep,basic:pobj,Pobj),
	rdf(Pobj,ne:type,literal('DATE')),
	rdf_update(Arg,basic:prep,Prep,predicate(mod:prep)), % overwrite
	rdf_assert(Root,basic:prep,Prep), !. % add new
% reattach 'for' PP: add modified xcomp nsubj from 'dobj for pobj'
argument(Root,basic:dobj,Arg) :-
	dep(Root,basic:dobj,Arg),
	rdf(Root,basic:xcomp,Comp),
	\+ rdf(Comp,basic:nsubj,_),
	rdf(Arg,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(for)),
	rdf(Prep,basic:pobj,Pobj),
	rdf_assert(Comp,mod:nsubj,Pobj), !.
% check for modified xcomp nsubj
argument(Root,Dep,Arg) :-
	atom_concat('http://nlp.stanford.edu/basic/',DepType,Dep),
	atom_concat('http://aristo.allenai.org/mod/',DepType,ModDep),
	rdf(Root,ModDep,_Arg), !,
	argument(Root,ModDep,Arg).

% inherit missing subj from parent xcomp
argument(Root,Dep,Arg) :-
	( Dep = 'http://nlp.stanford.edu/dep/nsubj'
	; Dep = 'http://nlp.stanford.edu/dep/nsubjpass' ),
	\+ rdf(Root,Dep,_Arg),
	rdf(Parent,dep:xcomp,Root),
	rdf(Parent,dep:agent,_Subj), !,
	argument(Parent,dep:agent,Arg).
argument(Root,Dep,Arg) :-
	( Dep = 'http://nlp.stanford.edu/dep/nsubj'
	; Dep = 'http://nlp.stanford.edu/dep/nsubjpass' ),
	\+ rdf(Root,Dep,_Arg),
	rdf(Parent,dep:xcomp,Root),
	\+ rdf(Parent,dep:cop,_),
	rdf(Parent,dep:nsubj,_Subj), !,
	argument(Parent,dep:nsubj,Arg).
% inherit missing subj from parent infmod
argument(Root,dep:nsubj,Parent) :-
	\+ rdf(Root,dep:nsubj,_),
	rdf(Parent,dep:infmod,Root),
	rdf(Prep,basic:pobj,Parent),
	rdf(Prep,token:lemma,literal(for)), !.
% inherit missing subj from parent partmod
argument(Root,dep:nsubj,Parent) :-
	\+ rdf(Root,dep:nsubj,_),
	rdf(Parent,dep:partmod,Root), !.
% inherit missing subj from parent partmod
argument(Root,dep:nsubj,Subj) :-
	\+ rdf(Root,dep:nsubj,_),
	rdf(Parent,dep:ccomp,Root),
	argument(Parent,dep:nsubj,Subj), !.

argument(Root,Dep,Arg) :-
	dep(Root,Dep,Arg).
argument(Root,Dep,Conj) :-
	dep(Root,Dep,Arg),
	rdf(Arg,basic:conj,Conj),
	\+ rdf(Root,Dep,Conj). % from collapsed cc
argument(Root,Dep,Conj) :-
	dep(Root,Dep,Arg),
	rdf(Arg,dep:dep,Conj), % from failed conj
	rdf(Conj,dep:cc,_).
argument(Root,Dep,[]) :-
	\+ rdf(Root,Dep,_).


% optional mods
% reattach 'for' PP: add modified xcomp nsubj from 'cop for pobj'
modifiers(Root,[]) :-
	rdf(Root,basic:cop,_),
	rdf(Root,basic:xcomp,Comp),
	\+ rdf(Comp,basic:nsubj,_),
	rdf(Root,basic:prep,Prep),
	rdf(Prep,token:lemma,literal(for)),
	rdf(Prep,basic:pobj,Pobj),
	rdf_assert(Comp,mod:nsubj,Pobj),
	fail.
modifiers(Root,Mods) :-
	findall(Mod,modifiers1(Root,Mod),Mods).

modifiers1(Root,Prep) :- %%% TODO: return Pobj not Prep
	rdf(Root,PrepRel,Pobj),
	atom_concat('http://nlp.stanford.edu/dep/prep_',_,PrepRel),
	prep(Pobj,Prep),
	\+ rdf(Root,dep:agent,Pobj), % exclude agent
	\+ purpose(Root,Pobj,_),
	\+ effect(Root,Pobj,_), % "in preparation for"
	\+ function(Pobj,Root,_), % "by Pobj"
	\+ ( rdf(Prep,token:lemma,literal(in)),
	     rdf(Pobj,token:lemma,literal(order)) ),
	\+ ( rdf(Prep,token:lemma,literal(by)),
	     rdf(Pobj,token:lemma,literal(which)) ),
	\+ ( rdf(Prep,token:lemma,literal(for)),
	     rdf(Pobj,dep:infmod,_)).
modifiers1(Root,Prep) :-
	rdf(Root,basic:prep,Prep),
	rdf(Prep,basic:pcomp,Comp),
	\+ effect(Comp,Root,_), % exclude purpose relations
	\+ effect(Root,Comp,_). % exclude purpose relations
modifiers1(Root,Comp) :-
	argument(Root,dep:xcomp,Comp), Comp \= [],
	\+ effect(Root,Comp,_), % exclude purpose relations
	\+ effect(Root,_,[_,Comp]). % exclude purpose relations
modifiers1(Root,Comp) :-
	argument(Root,dep:ccomp,Comp), Comp \= [],
	\+ effect(Root,Comp,_). % exclude purpose relations
modifiers1(Root,Adv) :-
	argument(Root,dep:advcl,Adv), Adv \= [],
	\+ effect(Adv,Root,_), % exclude purpose relations
	\+ effect(Root,Adv,_). % exclude purpose relations
modifiers1(Root,Mod) :-
	argument(Root,dep:advmod,Mod), Mod \= [],
	rdf(Mod,token:lemma,literal(Lemma)),
	( \+ memberchk(Lemma,[so,when,also,often,still,very])
	; (rdf(Mod,token:lemma,literal(so)),
	   rdf(Mod,basic:prep,_Prep)) ).
modifiers1(Root,Comp) :-
	argument(Root,dep:acomp,Comp), Comp \= [].
modifiers1(Root,Comp) :-
	argument(Root,dep:npadvmod,Comp), Comp \= [].
modifiers1(Root,Comp) :-
	argument(Root,dep:tmod,Comp), Comp \= [].


dep(Root,Dep,Ref) :-
	rdf(Root,Dep,Arg),
	rdf(Ref,dep:ref,Arg), !.
% pronoun antecedent
dep(Root,Dep,Coref) :-
	rdf(Root,Dep,Arg),
	coref(Arg,Coref), !.
% relative pronoun
dep(Root,Dep,Arg) :-
	rdf(Root,Dep,Wh),
	( rdf(Wh,token:pos,literal('WDT'))
	; (rdf(Wh,token:pos,literal('IN')),
	   rdf(Wh,token:lemma,literal(that))) ),
	!,
	rdf(Parent,dep:rcmod,Root),
	( (rdf(Parent,dep:cop,_),
	   dep(Parent,dep:nsubj,Arg))
	; Arg = Parent).
% direct dependency
dep(Root,Dep,Arg) :-
	rdf(Root,Dep,Arg).
% apposition
dep(Root,Dep,Appos) :-
	rdf(Root,Dep,Arg),
	rdf(Arg,dep:appos,Appos).


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

prep(Pobj,Prep) :-
	rdf(Prep,basic:pobj,Pobj), !.	
prep(Pobj,Prep) :-
	rdf(Prep,basic:pcomp,Pobj), !.	
prep(Pobj,Prep) :-
	rdf(Pobj2,basic:conj,Pobj),
	prep(Pobj2,Prep).

