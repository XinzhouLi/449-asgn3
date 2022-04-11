% the file to check the format of input file
:- dynamic(fp/2, fm/2, mp/3, tp/3, tk/2).

isRightBracket(')').
isLeftBracket('(').
isComma(',').
isEqual(X,X).
isNotEqual(X,Y) :- X\=Y.


get_fir([X|_],X).
get_sec([_,X|_],X).
get_thr([_,_,X|_],X).
get_fou([_,_,_,X|_],X).
get_fri([_,_,_,_,X|_],X).
get_last([X],X).
get_last([_|T],X):-
    get_last(T,X).

valMachine(Elem):- 
    member(Elem, ['1','2','3','4','5','6','7','8']).
valTask(Elem):- 
    member(Elem,['A','B','C','D','E','F','G','H']).
valPen(Num):- 
    member(Num, ['1','2','3','4','5','6','7','8','9','0']).

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

%parse MP by space and store in a 2D list
parse_MP([],[]).
parse_MP([H|T], Res):-
    parse_MP(T,Z),
    atom_chars(H,Y),
    splitOnSpace(Y, X),
    append([X],Z,Res).

%splitting at space
% eg ['A A AB','A'] -> [[[A],[A],[A,B]],[[A]]
splitOnSpace([],[]).
splitOnSpace(Text,Out):- 
    append(A1,[' '|A2],Text), 
    splitOnSpace(A2,Mid),
    append([A1],Mid,Some),
    subtract(Some, [[],[' ']], Out).
splitOnSpace(Text,[Text]).

%splitting at comma
splitOncomma([],[]).
splitOncomma(Text,Out):- 
    append(A1,[','|A2],Text), 
    splitOnSpace(A2,Mid),
    append([A1],Mid,Some),
    subtract(Some, [[],[',']], Out).
splitOncomma(Text,[Text]).

% check MP has 8 rows
check_MP_format([],_).
check_MP_format(X,OutputFile):-
    length(X, Len),
    isNotEqual(Len, 8)->
    printErrorAndClose(OutputFile, 'machine penalty error');
    check_MT_col_format(X,OutputFile).

% check every row has 8 elem
check_MT_col_format([],_).
check_MT_col_format([H|T],OutputFile):-
    length(H, Len),
    isNotEqual(Len, 8)-> 
    printErrorAndClose(OutputFile, 'machine penalty error');
    check_MT_col_format(T,OutputFile),check_MP_pen(H,OutputFile).
    
% check every penalty is valid
check_MP_pen([],_).
check_MP_pen([H|T],OutputFile):-
    % write(H),nl,
    check_penalty(H)->
    check_MP_pen(T,OutputFile);
    printErrorAndClose(OutputFile, 'invalid penalty').

% check every char in penalty is number
check_penalty([]).
check_penalty([H|T]):-
    % number_atom(X, H),
    valPen(H),
    check_penalty(T).

% check two tasks are valid
check_TT(X,Y,OutputFile):-
    valTask(X),
    valTask(Y)->
    write('');
    printErrorAndClose(OutputFile, 'invalid task').
% check Too near penalty 

check_TP_format([],_).
check_TP_format([H|T],OutputFile):-
    atom_chars(H, X),
    % write(H),
    get_fir(X, LB), isLeftBracket(LB),
    get_thr(X, Ca), isComma(Ca),
    get_fri(X, Cb), isComma(Cb),
    get_last(X, RB), isRightBracket(RB),
    get_sec(X, Ta),
    get_fou(X, Tb),
    check_TT(Ta,Tb,OutputFile),
    splitOncomma(X, Y), 
    get_last(Y, R),
    splitOncomma(R, S),
    get_last(S, Pen),
    ignore(without_last(Pen,P))
    ->
    % write('pass parsing check'),nl,
    check_MP_pen([P],OutputFile), 
    check_TP_format(T, OutputFile);
    printErrorAndClose(OutputFile, 'Error while parsing input file').
   
without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).

ignore(Goal) :-
    Goal,!.
ignore(_).




% Add the constrains dynamic facts


% partialAssignment add 
add_FP([]).
add_FP([H|T]):-
    atom_chars(H,X),
    get_sec(X, Ma),
    get_fou(X, Ta),
    asserta(fp(Ma, Ta)),
    add_FP(T).


% forbidden machine add
add_FM([]).
add_FM([H|T]):-
    atom_chars(H,X),
    get_sec(X, Ma),
    get_fou(X, Ta),
    asserta(fm(Ma, Ta)),
    add_FM(T).

add_MP([],_,_).
add_MP([H|T], Row ,Col):-
    add_mPen(H, Row, Col),
    Col is 1,
    R is Row + 1,
    add_MP(T, R, Col).

add_mPen([],_,_).
add_mPen([H|T], R, C):-
    convertLongPen(H, X),
    asserta(mp(R,C,X)),
    Col is C + 1,
    add_mPen(T, R, Col).

% add toonear task
add_TK([]).
add_TK([H|T]):-
    atom_chars(H, X),
    get_sec(X, Ta),
    get_fou(X, Tb),
    asserta(tk(Ta, Tb)),
    add_TK(T).



% add toonear task pen 
add_TP([]).
add_TP([H|T]):-
    atom_chars(H,X),
    get_sec(X, Ta),
    get_fou(X, Tb),
    splitOncomma(X, Y), 
    get_last(Y, R),
    splitOncomma(R, S),
    get_last(S, Pen),
    ignore(without_last(Pen,P)),
    convertLongPen(P, Z),
    asserta(tp(Ta, Tb, Z)),
    add_TP(T).


% sum up the Char number to Number
convertLongPen(List, X) :-
    length(List, 1),
    get_fir(List,A),
    number_atom(X, A), 
    !.
convertLongPen(List, Y) :-
    length(List, 2),
    get_fir( List, A), 
    get_sec( List, B),
    atom_concat(A, B, X), 
    number_atom(Y, X), 
    !.
convertLongPen(List, Y) :-
    length(List, 3),
    get_fir( List, A), 
    get_sec( List, B),
    get_thr(List, C),
    atom_concat(A, B, X), 
    atom_concat(X,C, Z),
    number_atom(Y, Z),
    !.
convertLongPen(List, Q) :-
    [Head|Tail] = List,
    convertLongPen(Tail, Y),
    number_atom(Y, Z),
    atom_concat(Head, Z, X),
    number_atom(Q, X).

% check name format
check_name([X], Output_file) :-
	% convert to char list
    atom_chars(X, Name_char_list),
	check_leading_space(Name_char_list, Output_file),
	check_mid_space(Name_char_list, Output_file).

	
% error as the name should only contain one line
check_name([_|_], Output_file) :- printErrorAndClose(Output_file, 'Error while parsing input file').

check_leading_space(Elem, OutputFile) :-
    % if space at the beginning
    get_fir(Elem, First),
    ==(First, ' ')->
        printErrorAndClose(OutputFile, 'Error while parsing input file');
        % else
		true.

check_mid_space(Elem, OutputFile) :-
	splitOnSpace(Elem, No_trailing_space),
	!,
	length(No_trailing_space, List_num),
	% if contain space in the middle
	==(List_num, 1) ->
		true;
		printErrorAndClose(OutputFile, 'Error while parsing input file').
