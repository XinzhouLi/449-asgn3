% Define module
:- module(softconstraints2,[getBestMatches/6]).


% check grid totals for each match and keep all lowest matches
getBestMatches([],_,_,_,_).
getBestMatches([VM|VMTail],ValidMatches,Grid,TooNearPen,Total,BestMatches):-
    gridTotal(Grid,Grid,VM,GridTotal),
    tooNearTotal(VM,VM,TooNearPen,TooNearTotal),
    NewTotal = GridTotal + TooNearTotal,
    NewTotal < Total ->
    getBestMatches(VMTail,ValidMatches,Grid,NewTotal,BestMatches);
    select(ValidMatches,VM,BestMatches),
    getBestMatches(VMTail,ValidMatches,Grid,Total,BestMatches).

% get total for a given set of matches
gridTotal([],_,_,0).
gridTotal([Row|GridTail],Grid,[Task|Matches],Total):-
    indexOf(['A','B','C','D','E','F','G','H'],Task,Index),
    getRowValue(Row,Row,Index,Value),
    gridTotal(GridTail,Grid,Matches,Sum),
    Total is Value + Sum.

getRowValue([R|RowTail],Row,Index,Value):-
    indexOf(Row,R,VIndex),
    Index = VIndex ->
    Value is R;
    getRowValue(RowTail,Row,Index,Value).

% check too-near penalties for all matches and keep a lowest matches
% getTooNearMatches([Matches],_,_,Matches).
% getTooNearMatches([GM|GMTail],GridMatches,TooNearPen,Total,TooNearMatches):-
%     getTooNearTotal(TooNearPen,TooNearPen,GM,NewTotal),
%     NewTotal < Total ->
%     getTooNearMatches(GMTail,GridMatches,TooNearPen,NewTotal,TooNearMatches);
%     select(GridMatches,GM,TooNearMatches),
%     getTooNearMatches(GMTail,GridMatches,TooNearPen,Total,TooNearMatches).

tooNearTotal([Task|MTail],Matches,TooNearPen,Total):-
    checkLeft(Task,Matches,TooNearPen,LeftPen),
    checkRight(Task,Matches,TooNearPen,RightPen),
    tooNearTotal(MTail,Matches,TooNearPen,Sum),
    Total is LeftPen + RightPen + Sum.

checkLeft(_,_,[],_).
checkLeft(M,Matches,[[Left,Right,Pen]|TooNearPen],Total):-
    checkSame(M,Right) ->
    indexOf(Matches,M,MIndex),
    getLeftIndex(MIndex,Index),
    nth1(Index,Matches,MLeft),
    checkDiff(MLeft,Left),
    checkLeft(M,Matches,TooNearPen,Total) ;
    checkLeft(M,Matches,TooNearPen,Sum),
    Total is Sum + Pen.

checkRight(_,_,[],_).
checkRight(M,Matches,[[Left,Right,Pen]|TooNearPen],Total):-
    checkSame(M,Left) ->
    indexOf(Matches,M,MIndex),
    getRightIndex(MIndex,Index),
    nth1(Index,Matches,MRight),
    checkDiff(MRight,Right),
    checkRight(M,Matches,TooNearPen,Total) ;
    checkRight(M,Matches,TooNearPen,Sum),
    Total is Sum + Pen.

% other functors
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