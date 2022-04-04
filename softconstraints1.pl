% Define module
:- module(softconstraints1,[createAllMatches/2,filterValidMatches/5]).

% Import other modules (aka other prolog files)
:- use_module(hardconstraints,[tooNear/4]).

% Sets of values
isMachine(Elem):- member(Elem,["1","2","3","4","5","6","7","8"]).
isTask(Elem):- member(Elem,["A","B","C","D","E","F","G","H"]).

value('1',1).
value('2',2).
value('3',3).
value('4',4).
value('5',5).
value('6',6).
value('7',7).
value('8',8).

% create all possible permutations of tasks
createAllMatches(Tasks, AllMatches):-
    permutation(Tasks, AllMatches).

% filter all permutations based on hard constraints
filterValidMatches(AllMatches,Forced,Forbid,TooNear,ValidMatches):-
    keepForcedMatches(AllMatches,AllMatches,Forced,ForcedMatches),
    dropForbidMatches(ForcedMatches,ForcedMatches,Forbid,ForcedForbidMatches),
    tooNear(ForcedForbidMatches,ForcedForbidMatches,TooNear,ValidMatches).

keepForcedMatches([],_,_,_).
keepForcedMatches([Matches|AllMTail],AllMatches,Forced,ValidMatches):-
    checkHardConstraint(Forced,Matches,Matches) ->
    keepForcedMatches(AllMTail,AllMatches,Forced,ValidMatches) ;
    select(Matches,AllMatches,ValidMatches),
    keepForcedMatches(AllMTail,AllMatches,Forced,ValidMatches).

dropForbidMatches([],_,_,_).
dropForbidMatches([Matches|AllMTail],AllMatches,Forbid,ValidMatches):-
    checkHardConstraint(Forbid,Matches,Matches) ->
    select(Matches,AllMatches,ValidMatches),
    dropForbidMatches(AllMTail,AllMatches,Forbid,ValidMatches);
    dropForbidMatches(AllMTail,AllMatches,Forbid,ValidMatches).

% returns true if (M,T) is in Matches
checkHardConstraint([],_,_).
checkHardConstraint([[MachF,TaskF]|Constraints],Matches,Matches):-
    isHardConstraint(MachF,TaskF,Matches,Matches),
    checkHardConstraint(Constraints,Matches,Matches).

% isHardConstraint(_,_,[],_).
isHardConstraint(MachF,TaskF,[Task|MTail],Matches):-
    indexOf(Matches,Task,MIndex),
    value(MachF,MachInt),
    MachInt = MIndex,
    TaskF = Task;
    isHardConstraint(MachF,TaskF,MTail,Matches).

% additional functors
indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), !,
    Index is Index1+1.