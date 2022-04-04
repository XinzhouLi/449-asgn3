% Define module
:- module(hardconstraints,[checkValidForced/2,checkForcedForbid/3,tooNear/4]).

%Import readLines
:- use_module(input_output,[readLines/3,printErrorAndClose/2]).

%Checks if forced mach/task repeats in other forced pairs
checkValidForced([]).
checkValidForced([_,[]]).
checkValidForced([H|T],OutputFile):-
    not(member(H,T)) ->
    distinct(T) ;
    printErrorAndClose(OutputFile,"No valid solution possible!").

%Checks if two lists have distinct elements, covers forced-forbidden conflicts
checkForcedForbid([],[]).
checkForcedForbid([], _).
checkForcedForbid(_, []).
checkForcedForbid([H|T], T2, OutputFile) :-
    not(member(H, T2)) ->
    distinct(T, T2) ;
    printErrorAndClose(OutputFile,"No valid solution possible!").


% Check for too-near invalid pairs
tooNear([],_,_).
tooNear([Matches|ListMTail],ListOfMatches,TooNear,ValidMatches):-
    % head(ListOfMatches,Matches),
    noTooNear(Matches,Matches,TooNear) ->
    tooNear(ListMTail,TooNear,ValidMatches) ;
    select(Matches,ListOfMatches,ValidMatches),
    tooNear(ListMTail,TooNear,ValidMatches).

% See if too-near violation in a set of matches
noTooNear(_,[],_).
noTooNear(Matches,[M|MTail],TooNear):-
    checkLeft(M,Matches,TooNear),
    checkRight(M,Matches,TooNear),
    noTooNear(Matches,MTail,TooNear).

checkLeft(_,_,[]).
checkLeft(M,Matches,[[Left,Right]|TooNear]):-
    checkSame(M,Right) ->
    indexOf(Matches,M,MIndex),
    getLeftIndex(MIndex,Index),
    nth1(Index,Matches,MLeft),
    checkDiff(MLeft,Left),
    checkLeft(M,Matches,TooNear) ;
    checkLeft(M,Matches,TooNear).

checkRight(_,_,[]).
checkRight(M,Matches,[[Left,Right]|TooNear]):-
    checkSame(M,Left) ->
    indexOf(Matches,M,MIndex),
    getRightIndex(MIndex,Index),
    nth1(Index,Matches,MRight),
    checkDiff(MRight,Right),
    checkRight(M,Matches,TooNear) ;
    checkRight(M,Matches,TooNear).

indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), !,
    Index is Index1+1.

getLeftIndex(Index,IndexL):-
    Index > 1 -> IndexL is Index-1 ; IndexL is 8.

getRightIndex(Index,IndexR):-
        Index < 8 -> IndexR is Index+1 ; IndexR is 1.

checkSame(X,Y):- X == Y.
checkDiff(X,Y):- X \== Y.

