
% EXAMPLE: NP is example (of how) Tuple
example_NP_Tuple(Entity,Comp,['EXAMPLE'-1,Example]) :-
	dependency(Entity,dep:cop,_),
	dependency(Entity,dep:nsubj,Example),
	lemma(Example,example),
	dependency(Example,dep:ccomp,Comp).
% EXAMPLE: NP is NP that Tuple
example_NP_Tuple(Entity,Comp,Rel) :-
	example_NP_NP(Entity,Entity2,Rel),
 	( dependency(Entity2,dep:partmod,Comp)
 	; dependency(Entity2,dep:rcmod,Comp) ).
% EXAMPLE: NP is way that Tuple
example_NP_Tuple(Entity,Comp,['EXAMPLE'-3,Cop,Way]) :-
	dependency(Way,dep:nsubj,Entity),
	dependency(Way,dep:cop,Cop),
	lemma(Way,way),
	( dependency(Way,dep:partmod,Comp)
	; dependency(Way,dep:rcmod,Comp) ).

% EXAMPLE: Tuple is example of NP
example_Tuple_NP(Entity,Comp,['EXAMPLE'-4,Example,'of']) :-
	dependency(Example,dep:prep_of,Entity),
	lemma(Example,example),
	dependency(Be,dep:nsubj,Example),
	lemma(Be,be),
	dependency(Be,dep:advcl,Comp).
example_Tuple_NP(Entity,Comp,['EXAMPLE'-5,Example,'of']) :-
	dependency(Example,dep:prep_of,Entity),
	lemma(Example,example),
	dependency(Example,dep:cop,_),
	dependency(Example,dep:csubj,Comp).
% EXAMPLE: NP example includes Tuple
example_Tuple_NP(Entity,Comp,['EXAMPLE'-6,Example]) :-
	dependency(Example,dep:nn,Entity),
	lemma(Example,example),
	dependency(Include,dep:nsubj,Example),
	lemma(Include,include),
	( dependency(Include,dep:ccomp,Comp)
	; dependency(Include,dep:conj_and,Comp) ).
% EXAMPLE: Tuple is called NP
example_Tuple_NP(Entity,Comp,['EXAMPLE'-7,Is,Called]) :-
	dependency(Called,dep:xcomp,Entity),
	text(Called,called),
	dependency(Called,dep:auxpass,Is),
	( dependency(Called,dep:advcl,Comp)
	; dependency(Called,dep:csubjpass,Comp) ).

% EXAMPLE: NP is a NP
example_NP_NP(Entity1,Entity2,['EXAMPLE'-8,Cop]) :-
	dependency(Entity2,dep:nsubj,Entity1),
	\+ lemma(Entity2,example),
	\+ lemma(Entity2,way),
	dependency(Entity2,dep:cop,Cop),
	( dependency(Entity2,dep:det,_)
	; pos(Entity2,'NNS') ). % plural
% EXAMPLE: NP is called NP
example_NP_NP(Entity1,Entity2,['EXAMPLE'-9,Is,Called]) :-
	dependency(Called,dep:nsubjpass,Entity1),
	\+ dependency(Called,dep:advcl,_),
	text(Called,called),
	dependency(Called,dep:auxpass,Is),
	dependency(Called,dep:xcomp,Entity2).
% EXAMPLE: NP such as NP
example_NP_NP(Entity1,Entity2,['EXAMPLE'-10,'such as']) :-
	dependency(Entity2,dep:prep_such_as,Entity1).
% EXAMPLE: NP is example of NP
example_NP_NP(Entity1,Entity2,['EXAMPLE'-11,Example,'of']) :-
	dependency(Entity1,dep:cop,_),
	dependency(Entity1,dep:nsubj,Example),
	lemma(Example,example),
	dependency(Example,dep:prep_of,Entity2).
example_NP_NP(Entity1,Entity2,['EXAMPLE'-12,Example,'of']) :-
	dependency(Example,dep:nsubj,Entity1),
	lemma(Example,example),
	dependency(Example,dep:cop,_),
	dependency(Example,dep:prep_of,Entity2).
example_NP_NP(Entity1,Entity2,['EXAMPLE'-13,Example,'of']) :-
	dependency(Ent,dep:conj_and,Entity1),
	dependency(Ent,dep:cop,_),
	dependency(Ent,dep:nsubj,Example),
	lemma(Example,example),
	dependency(Example,dep:prep_of,Entity2).
% failed cop
example_NP_NP(Entity1,Entity2,['EXAMPLE'-14,Example,'of']) :-
	dependency(Be,dep:nsubj,Entity1),
	lemma(Be,be),
	dependency(Be,dep:tmod,Example),
	lemma(Example,example),
	dependency(Example,dep:prep_of,Entity2).
% EXAMPLE: example of NP includes NP
example_NP_NP(Entity1,Entity2,['EXAMPLE'-15,Example,'of']) :-
	dependency(Include,dep:dobj,Entity1),
	lemma(Include,include),
	dependency(Include,dep:nsubj,Example),
	lemma(Example,example),
	dependency(Example,dep:prep_of,Entity2).


% CAUSE: NP "causes" Tuple
cause(Root,Entity,['CAUSE'-16,Cause]) :-
	dependency(Cause,dep:xcomp,Root),
	causes(Cause),
	( dependency(Entity,dep:rcmod,Cause)
	; dependency(Cause,dep:nsubj,Entity) ), !.
% CAUSE: NP-RelC "is caused by" NP
cause(Root,Entity,['CAUSE'-17,Cause]) :-
	dependency(Subj,dep:rcmod,Root),
	dependency(Cause,dep:nsubjpass,Subj),
	causes(Cause),
	dependency(Cause,dep:agent,Entity), !.
% CAUSE: Tuple "because of" NP
cause(Root,Entity,['CAUSE'-18,'because of']) :-
	dependency(Root,dep:prep_because_of,Entity).
/*
% NP "results from" Tuple
cause2(Root,Entity,['CAUSE'-19,'results from']) :-
	dependency(Result,dep:prepc_from,Root),
        lemma(Result,result),
	dependency(Result,dep:nsubj,Entity).
% Tuple "results in" NP
cause2(Root,Entity,['CAUSE'-20,'results in']) :-
	dependency(Root,dep:xcomp,Result),
        lemma(Result,result),
	dependency(Result,dep:prep_in,Entity).
*/	


% PURPOSE: Tuple "is for" NP
purpose(Root,Entity,['PURPOSE'-21,'for']) :-
	dependency(Root,dep:cop,_),
	dependency(Root,dep:prep_for,Entity).
% PURPOSE: Tuple "for" NP
purpose(Root,Entity,['PURPOSE'-22,'for']) :- % dobj-PP
	dependency(Root,dep:dobj,Dobj),
	dependency(Dobj,dep:prep_for,Entity).
purpose(Root,Entity,['PURPOSE'-23,'for']) :- % pobj-PP
	\+ dependency(Root,dep:dobj,_),
	dependency(Root,basic:prep,Prep),
	dependency(Prep,basic:pobj,Pobj),
	dependency(Pobj,dep:prep_for,Entity).


% ENABLE: Tuple "by" NP
function(Entity,Root,['ENABLE'-24,'by']) :-
	dependency(Root,dep:prep_by,Entity),
	pos(Root,'VBG'),
	pos(Entity,'NN').
% copular
function(Entity,Comp,Rel) :-
	dependency(Root,dep:nsubj,Entity),
	dependency(Root,dep:cop,_),
	function(Root,Comp,Rel).
% FUNCTION: NP "helps" Tuple
function(Entity,Comp,['FUNCTION'-26,Help]) :-
	dependency(Help,dep:nsubj,Entity),
	\+ dependency(_,dep:rcmod,Help),
	helps(Help),
	( dependency(Help,dep:ccomp,Comp)
	; dependency(Help,dep:xcomp,Comp) ).
function(Entity,Comp,['FUNCTION'-27,Help]) :-
	dependency(Entity,dep:rcmod,Help),
	helps(Help),
	( dependency(Help,dep:ccomp,Comp)
	; dependency(Help,dep:xcomp,Comp) ).
% FUNCTION: NP "helps in" Tuple
function(Entity,Comp,['FUNCTION'-28,Help]) :-
	dependency(Help,dep:nsubj,Entity),
	\+ dependency(_,dep:rcmod,Help),
	helps(Help),
	dependency(Help,dep:prepc_in,Comp),
	pos(Comp,'VBG').
% FUNCTION: NP "is used to" Tuple
function(Entity,Comp,['FUNCTION'-29,Aux,Rel]) :-
	dependency(Rel,dep:nsubjpass,Entity),
	dependency(Rel,dep:auxpass,Aux),
	lemma(Rel,use),
	\+ dependency(_,dep:rcmod,Rel),
	dependency(Rel,dep:xcomp,Comp).
function(Entity,Comp,['FUNCTION'-30,Aux,Rel]) :-
	dependency(Entity,dep:cop,_),
	dependency(Entity,dep:nsubj,Subj),
	dependency(Subj,dep:infmod,Rel),
	lemma(Rel,use),
	dependency(Rel,dep:xcomp,Comp),
	aux(Rel,Aux).
function(Entity,Comp,['FUNCTION'-31,Rel,Aux]) :-
	dependency(Entity,dep:partmod,Rel),
	lemma(Rel,use),
	\+ dependency(_,dep:rcmod,Rel),
	dependency(Rel,dep:xcomp,Comp),
	aux(Comp,Aux).
% FUNCTION: NP "to" Tuple
function(Entity,Comp,['FUNCTION'-32,'to']) :-
	dependency(Entity,dep:nsubj,Subj),
	\+ dependency(Entity,dep:cop,_),
	\+ lemma(Entity,be),
	dependency(Subj,dep:infmod,Comp),
	\+ lemma(Comp,use).
% FUNCTION: NP "measures" NP
function(Entity,Measure,['FUNCTION'-33,Measure]) :-
	dependency(Measure,dep:nsubj,Entity),
	lemma(Measure,measure).
% FUNCTION: NP "by which" Tuple
function(Entity,Comp,['FUNCTION'-34,'by',Which]) :-
	dependency(Root,dep:nsubj,Entity),
	dependency(Root,dep:cop,_),
	dependency(Root,dep:rcmod,Comp),
	dependency(Comp,dep:prep_by,Which),
	lemma(Which,which).
% FUNCTION: "function of" NP is Tuple
function(Entity,Comp,['FUNCTION'-35,Function]) :-
	dependency(Function,dep:prep_of,Entity),
	lemma(Function,function),
	dependency(Comp,dep:nsubj,Function),
	\+ lemma(Comp,be).
function(Entity,Comp,['FUNCTION'-36,Function,'of']) :-
	dependency(Function,dep:prep_of,Entity),
	lemma(Function,function),
	dependency(Be,dep:nsubj,Function),
	lemma(Be,be),
	dependency(Be,dep:xcomp,Comp).
% FUNCTION: NP "is responsible for" Tuple
function(Entity,Comp,['FUNCTION'-37,Responsible,'for']) :-
	dependency(Responsible,dep:nsubj,Entity),
	lemma(Responsible,responsible),
	dependency(Responsible,dep:cop,_),
	dependency(Responsible,dep:prepc_for,Comp).
% FUNCTION: NP "is responsible for" NP
function(Entity,Comp,['FUNCTION'-38,Responsible,'for']) :-
	dependency(Responsible,dep:nsubj,Entity),
	lemma(Responsible,responsible),
	dependency(Responsible,dep:cop,_),
	dependency(Responsible,dep:prep_for,Comp).
% FUNCTION: NP "is necessary for" Tuple
function(Entity,Comp,['REQUIREMENT'-39,Necessary]) :-
	dependency(Necessary,dep:nsubj,Entity),
	lemma(Necessary,necessary),
	dependency(Necessary,dep:cop,_),
	dependency(Necessary,dep:advcl,Comp).
	

%%%effect(Root,Comp,['EFFECT'-40,Aux]) :-
%%%	argument(Root,dep:purpcl,Comp), % Sapir
%%%	Comp \= [],
%%%	aux(Comp,Aux).
%%%effect(Root,Comp,['EFFECT'-41]) :-
%%%	argument(Root,dep:advcl,Comp), % Sapir
%%%	Comp \= [].


% PART: Tuple is part of Tuple
effect(Root,Comp,['PART'-42,Part,'of']) :-
	dependency(Be,dep:advcl,Root),
	lemma(Be,be),
	dependency(Be,dep:nsubj,Part),
	lemma(Part,part),
	dependency(Part,dep:prep_of,Comp).
effect(Root,Comp,['PART'-43,Part,'of']) :-
	dependency(Part,dep:csubj,Root),
	dependency(Part,dep:cop,_),
	lemma(Part,part),
	dependency(Part,dep:prep_of,Comp).
effect(Root,Comp,['PART'-44,Part,Prep]) :-
	dependency(Part,dep:csubj,Root),
	dependency(Part,dep:cop,_),
	lemma(Part,part),
	dependency(Part,basic:prep,Prep),
	dependency(Prep,basic:dep,Comp). % failed PP
		    
% EFFECT: Tuple "in preparation for" Tuple
effect(Root,Comp,['EFFECT'-45,'in',Comp,'for']) :-
	dependency(Root,dep:prep_in,Comp),
	lemma(Comp,preparation),
	dependency(Comp,dep:prep_for,_).

% EFFECT: Tuple "helps to" Tuple
effect(Root,Comp,['EFFECT'-46,Aux,Help]) :-
	dependency(Root,dep:xcomp,Help),
	helps(Help), !,
	aux(Help,Aux),
	( dependency(Help,dep:xcomp,Comp)
	; dependency(Help,dep:ccomp,Comp)).
effect(Root,Comp,['EFFECT'-47,Aux,Help]) :- % infmod on dobj
	dependency(Root,dep:dobj,Dobj),
	dependency(Dobj,dep:infmod,Help),
	helps(Help),
	dependency(Help,dep:ccomp,Comp),
	aux(Help,Aux).
effect(Root,Comp,['EFFECT'-48,Aux,Help]) :- % infmod on pobj
	dependency(Root,basic:prep,Prep),
	dependency(Prep,basic:pobj,Pobj),
	dependency(Pobj,dep:infmod,Help),
	helps(Help),
	dependency(Help,dep:ccomp,Comp),
	aux(Help,Aux).
% EFFECT: Tuple "in order to" Tuple
effect(Root,Comp,['EFFECT'-49,'in',Pobj,Aux]) :- % xcomp on pobj
	dependency(Root,dep:prep_in,Pobj),
	lemma(Pobj,order),
	dependency(Root,dep:xcomp,Comp),
	\+ dependency(Pobj,dep:xcomp,_), !,
	aux(Comp,Aux).
effect(Root,Comp,['EFFECT'-50,'in',Pobj,Aux]) :- % infmod on dobj
	dependency(Root,dep:dobj,Dobj),
	dependency(Dobj,dep:prep_in,Pobj),
	lemma(Pobj,order),
	dependency(Dobj,dep:infmod,_), !,
	dependency(Dobj,dep:infmod,Comp),
	\+ dependency(Pobj,dep:xcomp,_),
	aux(Comp,Aux).
effect(Root,Comp,['EFFECT'-51,Mark,Dep,Aux]) :- % advcl
	( dependency(Root,dep:advcl,Comp)
	; dependency(Root,dep:dep,Comp)),
	mark(Comp,Mark),
	lemma(Mark,in),
	dependency(Comp,dep:dep,Dep),
	lemma(Dep,order),
	aux(Comp,Aux),
	lemma(Aux,to).
% EFFECT: Tuple "to" Tuple
effect(Root,Comp,['REQUIREMENT'-52,Aux]) :-
	dependency(Root,dep:xcomp,Comp),
	aux(Root,Modal),
	lemma(Modal,must),
	\+ dependency(_,dep:rcmod,Root),
	\+ dependency(Comp,dep:cop,_),
	aux(Comp,Aux).
effect(Root,Comp,['EFFECT'-53,Aux]) :-
	dependency(Root,dep:xcomp,Comp),
	( (aux(Root,Modal),
	   \+ lemma(Modal,must))
	; \+ aux(Root,Modal) ),
	\+ dependency(_,dep:rcmod,Root),
	\+ dependency(Comp,dep:cop,_),
	aux(Comp,Aux).
effect(Root,Comp,['EFFECT'-54,Aux]) :-
	dependency(Root,dep:xcomp,Comp),
	\+ dependency(_,dep:rcmod,Root),
	dependency(Comp,dep:cop,Cop),
	\+ lemma(Cop,be),
	aux(Comp,Aux).
effect(Root,Comp,['EFFECT'-55]) :-
	dependency(Root,dep:xcomp,Comp),
	pos(Comp,'VBG'),
	\+ dependency(Comp,dep:ccomp,_).
effect(Root,Comp,['EFFECT'-56,Aux]) :- % infmod on dobj
	dependency(Root,dep:dobj,Dobj),
	dependency(Dobj,dep:infmod,Comp),
	\+ lemma(Dobj,ability),
	\+ helps(Comp),
	aux(Comp,Aux).
effect(Root,Comp,['EFFECT'-57,Prep]) :- % infmod on pobj
	dependency(Root,basic:prep,Prep),
	dependency(Prep,basic:pobj,Pobj),
	dependency(Pobj,dep:infmod,Comp),
	\+ helps(Comp).
effect(Root,Comp,['EFFECT'-58,Mod|Aux]) :- % rcmod on dobj
	\+ dependency(Root,dep:cop,_),
	dependency(Root,basic:dobj,Dobj),
	dependency(Dobj,dep:rcmod,Mod),
	dependency(Mod,dep:ccomp,Comp),
	(aux(Comp,Aux) ; Aux = []). % optional aux
effect(Root,Comp,['EFFECT'-59,Mod|Aux]) :- % rcmod on dobj
	\+ dependency(Root,dep:cop,_),
	dependency(Root,basic:dobj,Dobj),
	dependency(Dobj,dep:rcmod,Mod),
	dependency(Mod,dep:xcomp,Comp),
	aux(Comp,Aux). % required aux
effect(Root,Comp,['EFFECT'-60,Mod|Aux]) :- % rcmod on pobj
	\+ dependency(Root,dep:cop,_),
	dependency(Root,basic:prep,Prep),
	dependency(Prep,basic:pobj,Pobj),
	dependency(Pobj,dep:rcmod,Mod),
	dependency(Mod,dep:ccomp,Comp),
	(aux(Comp,Aux) ; Aux = []). % optional aux
% EFFECT: Tuple "by" Tuple
effect(Root,Comp,['ENABLE'-61,'by']) :- % by-VBG
	dependency(Comp,dep:prepc_by,Root),
	pos(Root,'VBG').
effect(Root,Comp,['ENABLE'-62,'by']) :- % passive by-VBG
	dependency(Comp,dep:agent,Root),
	pos(Comp,'VBG'),
	dependency(Comp,dep:auxpass,_).
% EFFECT: partmod
effect(Root,Comp,['EFFECT'-63]) :-
	pos(Root,'VBG'),
	dependency(Subj,dep:partmod,Root),
	dependency(Comp,dep:nsubj,Subj).
% EFFECT: Tuple "in" Tuple
effect(Root,Comp,['EFFECT'-64,'in']) :-
	dependency(Root,dep:prepc_in,Comp),
	pos(Comp,'VBG').
% EFFECT: Tuple "is for" Tuple
effect(Root,Comp,['EFFECT'-65,Mark]) :-
	dependency(Root,dep:cop,_),
	dependency(Root,dep:advcl,Comp),
	mark(Comp,Mark),
	lemma(Mark,for),
	!.
% EFFECT: Tuple "for there" Tuple
effect(Root,Comp,['EFFECT'-66,Mark]) :-
	dependency(Root,dep:advcl,Comp),
	mark(Comp,Mark),
	lemma(Mark,for),
	dependency(Comp,dep:expl,Expl),
	lemma(Expl,there).
% EFFECT: Tuple "for" Tuple
effect(Root,Comp,['EFFECT'-67,Aux]) :-
	dependency(Root,dep:advcl,Comp),
	mark(Comp,Mark),
	lemma(Mark,for),
	\+ dependency(Comp,dep:expl,_),
	dependency(Comp,dep:aux,Aux),
	lemma(Aux,to).
% EFFECT: Tuple "because" Tuple
effect(Root,Comp,['EFFECT'-68,Mark]) :-
	dependency(Comp,dep:advcl,Root),
	mark(Root,Mark),
	lemma(Mark,because).
% EFFECT: "As" Tuple Tuple
effect(Root,Comp,['EFFECT'-69,Mark]) :-
	dependency(Comp,dep:advcl,Root),
	mark(Root,Mark),
	text(Mark,'As').
% EFFECT: "If" Tuple Tuple
effect(Root,Comp,['EFFECT'-70,Mark]) :-
	dependency(Comp,dep:advcl,Root),
	mark(Root,Mark),
	text(Mark,'If').
% EFFECT: Tuple "because of" Tuple
effect(Root,Comp,['EFFECT'-71,Mwe,Prep]) :-
	dependency(Pobj,dep:rcmod,Root),
	dependency(Prep,basic:pobj,Pobj),
	lemma(Prep,of),
	dependency(Prep,basic:mwe,Mwe),
	lemma(Mwe,because),
	dependency(Comp,basic:prep,Prep).
% EFFECT: Tuple "when" Tuple
effect(Root,Comp,['WHEN'-72,Adv]) :-
	( dependency(Help,dep:xcomp,Root)
	; dependency(Help,dep:ccomp,Root)),
	helps(Help),
	dependency(Help,dep:advcl,Comp),
	( dependency(Comp,dep:advmod,Adv) ; dependency(Comp,dep:mark,Adv) ), % for Sapir
	lemma(Adv,when).
% EFFECT: Tuple "when" Tuple
effect(Root,Comp,['WHEN'-73,Adv]) :-
	dependency(Root,dep:advcl,Comp),
	\+ helps(Root),
	( dependency(Comp,dep:advmod,Adv) ; dependency(Comp,dep:mark,Adv) ), % for Sapir
	lemma(Adv,when).
% EFFECT: Tuple "in order for" Tuple
effect(Root,Comp,['EFFECT'-74,'in',Pobj,Prep2]) :-
	dependency(Root,dep:prep_in,Pobj),
	lemma(Pobj,order),
	dependency(Pobj,dep:infmod,Comp),
	( dependency(Pobj,basic:prep,Prep2)
	; dependency(Comp,basic:mark,Prep2) ),
	lemma(Prep2,for).
effect(Root,Comp,['EFFECT'-75,'in',Pobj,Prep2]) :- % via xcomp
	dependency(Root2,dep:xcomp,Root),
	dependency(Root2,dep:prep_in,Pobj),
	lemma(Pobj,order),
	dependency(Pobj,dep:infmod,Comp),
	( dependency(Pobj,basic:prep,Prep2)
	; mark(Comp,Prep2) ),
	lemma(Prep2,for).
% EFFECT: Tuple "and helps" Tuple
effect(Root,Comp,['EFFECT'-76,Csubj]) :-
	dependency(Root,dep:conj_and,Comp),
	dependency(Comp,dep:csubj,Csubj),
	helps(Csubj).
effect(Root,Comp,['EFFECT'-77,Help]) :-
	( dependency(Root,dep:conj_and,Help)
	; dependency(Root,dep:ccomp,Help)
	; dependency(Help,dep:csubj,Root) ),
	helps(Help),
	dependency(Help,dep:ccomp,Comp).
% EFFECT: Tuple "to help" Tuple
effect(Root,Comp,['EFFECT'-78,Help]) :-
	dependency(Root,dep:csubj,Help),
	helps(Help),
	dependency(Help,dep:ccomp,Comp).
% EFFECT: Tuple "results in" Tuple
effect(Root,Comp,['EFFECT'-79,Result,'in']) :-
	dependency(Result,dep:csubj,Root),
	lemma(Result,result),
	dependency(Result,dep:prepc_in,Comp).
% EFFECT: Tuple "so that" Tuple
effect(Root,Comp,['EFFECT'-80,So,Mark]) :-
	dependency(Root,dep:advcl,Adv),
	mark(Adv,Mark),
	lemma(Mark,that),
	( dependency(Adv,dep:advmod,So) ; dependency(Adv,dep:mark,So)),
	lemma(So,so),
	!,
	argument(Root,dep:advcl,Comp). % allow conjuncts
effect(Root,Adv,['EFFECT'-81,So,Mark]) :-
	dependency(Root,dep:dep,Adv), % unknown dep
	mark(Adv,Mark),
	lemma(Mark,that),
	( dependency(Adv,dep:advmod,So) ; dependency(Adv,dep:mark,So)),
	lemma(So,so),
	!.
effect(Root,Comp,['EFFECT'-82,So,Mark]) :-
	dependency(Root,dep:advmod,So),
	lemma(So,so),
	dependency(Root,dep:ccomp,Ccomp),
	mark(Ccomp,Mark),
	lemma(Mark,that),
	!,
	argument(Root,dep:ccomp,Comp). % allow conjuncts
% EFFECT: Tuple "so" Tuple
effect(Root,Comp,['EFFECT'-83,Mark]) :-
	dependency(Root,dep:advcl,Comp),
	dependency(Comp,dep:mark,Mark),
	lemma(Mark,so).
% EFFECT: Tuple "and by doing so" Tuple
effect(Root,Comp,['EFFECT'-84,'by',Pcomp,So]) :-
	dependency(Root,dep:conj_and,Comp),
	dependency(Comp,dep:prepc_by,Pcomp),
	text(Pcomp,doing),
	( dependency(Pcomp,dep:advmod,So) ; dependency(Pcomp,dep:mark,So)),
	lemma(So,so).
% EFFECT: Tuple "is a/one/another way that/to' Tuple
effect(Root,Comp,['EFFECT'-85,Cop,Way]) :-
	( dependency(Way,dep:csubj,Root)
	; dependency(Way,dep:nsubj,Root) ),
	dependency(Way,dep:cop,Cop),
	lemma(Way,way),
	( dependency(Way,dep:rcmod,Comp)
	; dependency(Way,dep:infmod,Comp) ),
	!.
