% :- module(format_check,[check_two_format/2]).



isRightBracket(')').
isLeftBracket('(').
isComma(',').


get_fir([X|_],X).
get_sec([_,X|_],X).
get_thr([_,_,X|_],X).
get_fou([_,_,_,X|_],X).
get_fri([_,_,_,_,X|_],X).


valMachine(Elem):- 
    member(Elem, ['1','2','3','4','5','6','7','8']).
valTask(Elem):- 
    member(Elem,['A','B','C','D','E','F','G','H']).
valPen(Num):- 
    Num >= 0.


%check partial assignment 
%check forbidden machine
% they have the same format.
check_PA_FM_format([],_).
check_PA_FM_format([H|T],OutputFile):-
    % check '(' ',' ')'
    atom_chars(H,X),
    get_fir(X,LB),
    get_thr(X,CM),
    get_fri(X,RB),

    isLeftBracket(LB),
    isComma(CM),
    isRightBracket(RB) ->
    % check Machine and Task 
    get_sec(X, Ma),
    get_fou(X, Ta),
    check_M_T(Ma, Ta, T, OutputFile);
    printErrorAndClose(OutputFile, 'Error while parsing input file').


% check Machine and Task
check_M_T(X,Y,Z,OutputFile):-
    write(X),write(Y),
    valMachine(X),
    valTask(Y) ->
    check_PA_FM_format(Z, OutputFile);
    printErrorAndClose(OutputFile, 'invalid machine/task').



%check too near task 
check_TK_format([],_).
check_TK_format([H|T],OutputFile):-
    % check '(' ',' ')'
    atom_chars(H,X),
    get_fir(X,LB),
    get_thr(X,CM),
    get_fri(X,RB),
    isLeftBracket(LB),
    isComma(CM),
    isRightBracket(RB) ->
    % check Machine and Task 
    get_sec(X, Ma),
    get_fou(X, Ta),
    check_T_T(Ma, Ta, T, OutputFile);
    printErrorAndClose(OutputFile, 'Error while parsing input file').


% check Task and Task
check_T_T(X,Y,Z,OutputFile):-
    valTask(X),
    valTask(Y) ->
    check_TK_format(Z, OutputFile);
    printErrorAndClose(OutputFile, 'invalid machine/task').


