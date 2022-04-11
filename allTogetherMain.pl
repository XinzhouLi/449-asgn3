% the main exection file
:- initialization(main).
:- dynamic(fp/2, fm/2, mp/3, tp/3, tk/2,sol/2).
% Label constants
label('Name:').
label('forced partial assignment:').
label('forbidden machine:').
label('too-near tasks:').
label('machine penalties:').
label('too-near penalities').

main :-
    % get input/output file name from CMD
    argument_value(1, InStream),          % get the first parameter
    argument_value(2, OutStream),         % get the second parameter
    readFile(InStream, OutStream, Data),  % read the input file

    get_name(Data, Name),
    get_forced_partial(Data, FP),
    get_forbidden_machine(Data, FB),
    get_toonear_tasks(Data, TK),
    get_machine_penalties(Data, MP),
    get_toonear_penalties(Data, TP),
    write(Name),nl,
    write(FP),nl,
    write(FB),nl,
    write(TK),nl,
    write(MP),nl,
    write(TP),nl,
    check_name(Name, OutStream),
    check_PA_FM_format(FP, OutStream),
    check_PA_FM_format(FB, OutStream),
    check_TK_format(TK, OutStream),
    parse_MP(MP, MP_da),
    check_MT_content(MP_da, OutStream),
    check_MP_format(MP_da, OutStream),
    check_TP_format(TP, OutStream),
    add_FP(FP),
    add_FM(FB),
    add_TK(TK),
    add_MP(MP_da,1,1),
    add_TP(TP),
    find_dup_mach(['A','B','C','D','E','F','G','H'],OutStream),
    find_dup_task(['1','2','3','4','5','6','7','8'],OutStream),

    % initial soltion
    asserta(sol(['A','A','A','A','A','A','A','A'], 9999999)),    

    % run the algorithm to get the minimal penalty solution
    ignore(algo(['A','B','C','D','E','F','G','H'])),
    ignore(sol(List, Pen)),
    write(List), write(Pen), nl,
    output_result(OutStream),
    


    halt(0).

find_dup_mach([],_).
find_dup_mach([H|T],OutStream):- 
    fp(X,H),
    fp(Y,H),
    % write(X),
    % write(Y),nl,
    isNotEqual(X,Y)
    ->printErrorAndClose(OutStream,'partial assignment error');
    find_dup_mach(T, OutStream).


find_dup_task([],_).
find_dup_task([H|T],OutStream):- 
    (fp(H,X),
    fp(H,Y),
    % write(X),
    % write(Y),nl,
    isNotEqual(X,Y))
    ->printErrorAndClose(OutStream,'partial assignment error');
    find_dup_task(T, OutStream).


readFile(InStream, OutStream, Data_list) :-
	% dealing the input file.
	open(InStream, read, Stream),
    read_lines(Stream, List_of_line, OutStream),

	% read input file convert to list
	split_string_lines(List_of_line, List_lines),
	clear_empty_element(List_lines, Data_list),
	% write(Data_list),
	close(Stream),

    % check the input file labels
    check_labels(Data_list, OutStream).

% read line from input file
read_lines(InStream, Output_List, OutputFile):- 
	get_code(InStream, Char), 
	checkCharAndReadRest(Char, Chars, InStream, OutputFile), 
	atom_codes(Output_List, Chars). 

% error checking (see if there is '#')
checkCharAndReadRest(35, [], _, OutputFile):-  
	printErrorAndClose(OutputFile, 'Error while parsing input file').

% error checking (see if there is '%')
checkCharAndReadRest(37, [], _, OutputFile):-  
	printErrorAndClose(OutputFile, 'Error while parsing input file').

% to see if reach the end of file
checkCharAndReadRest(-1, [], _, _):-  !. 

% recuresively check the input file
checkCharAndReadRest(Char,[Char|Chars],InStream,OutputFile):- 
	get_code(InStream,NextChar), 
	checkCharAndReadRest(NextChar,Chars,InStream,OutputFile).

% write the error message on output file and close the program
printErrorAndClose(FileName,ErrorMsg):-
    open(FileName, write, OutStream),
    write(OutStream, ErrorMsg),
    close(OutStream),
    halt(0).

split_string_lines(InputText,List):-
	new_split_string(InputText, [], Temp),
	convert_back(Temp, List).

new_split_string([],CurrentList, ListOfLines):-
	CurrentList = [_|Final],
	ListOfLines = Final.
new_split_string(List, [], ListOfLines):-
	atom_chars(List, NewList),
	new_split_string(NewList, [""], ListOfLines).
new_split_string(List, CurrentList, ListOfLines):-
	append(First,['\n'|Rest],List),
	append(CurrentList,[First],NewCurr),
	new_split_string(Rest,NewCurr,ListOfLines).

convert_back([],_).
convert_back(List, FixedList):-
    List = [X|Rest],
    atom_chars(FixedFirst,X),
    convert_back(Rest, RestFixed),
    append([FixedFirst],RestFixed,FixedList).

% delete the empty lines
clear_empty_element([],[]).
clear_empty_element([H|T], Output_list) :-
	clear_empty_element(T, Result),
	check_empty(H, Result, Output_list ).
	% write(Output_list),nl.

% to check if there contain empty line or blank line
check_empty(' ',Input,Output):-
	append(Input,[] , Output).
check_empty('',Input,Output):-
	append(Input,[] , Output).
check_empty(El, Input, Output):- 
	append([El], Input , Output).

% to check the input file labels
check_labels(ListOfLines, OutputFile):-
    % write(X),
    label(X), list_contain_label(X, ListOfLines, OutputFile).

% recursively check if input list contains labels
list_contain_label(_, [], OutputFile):-
    printErrorAndClose(OutputFile, 'Error while parsing input file').
list_contain_label(X, [X|_], _):- !.        % find the label
list_contain_label(X, [_|Xs], OutputFile):-   % recursively check next line
    list_contain_label(X, Xs, OutputFile).

% to get the data after/before the pivot
split(List, Pivot, Left, Right) :- append(Left, [Pivot|Right], List).

% get name
get_name(List, Output):-
    split(List, 'Name:', _, Tmp),                       % get the data after name label
    split(Tmp, 'forced partial assignment:', Output, _).% get the data before force partial label

% get forced partial machine
get_forced_partial(List, Output):-
    split(List, 'forced partial assignment:', _, Tmp),  % get the data after force partial label
    split(Tmp, 'forbidden machine:', Output, _).        % get the data before forbidden machine

% get forbidden machine    
get_forbidden_machine(List, Output):-
    split(List, 'forbidden machine:', _, Tmp),          % get the data after forbidden machine label
    split(Tmp, 'too-near tasks:', Output, _).           % get the data before noo-near tasks label
    
% get too near tasks
get_toonear_tasks(List, Output):-
    split(List, 'too-near tasks:', _, Tmp),             % get the data after too-near taskes label
    split(Tmp, 'machine penalties:', Output, _).        % get the data before machine penalties label

% get machine penalties
get_machine_penalties(List, Output):-
    split(List, 'machine penalties:', _, Tmp),          % get the data after machine penalties label
    split(Tmp, 'too-near penalities', Output, _).       % get the data before too-near penalties label

% get too near penalties
get_toonear_penalties(List, Output):-
    split(List, 'too-near penalities', _, Output).      % get the data after too-near penalties label



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
    check_MT_col_format(T,OutputFile).
    % check_MT_col_format(T,OutputFile),check_MP_pen(H,OutputFile).

% check MP content
check_MT_content([],_).
check_MT_content([H|T],OutputFile):-
    check_MP_pen(H, OutputFile),
    check_MT_content(T, OutputFile).


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



    get_list1([X,_,_,_,_,_,_,_], X).
get_list2([_,X,_,_,_,_,_,_], X).
get_list3([_,_,X,_,_,_,_,_], X).
get_list4([_,_,_,X,_,_,_,_], X).
get_list5([_,_,_,_,X,_,_,_], X).
get_list6([_,_,_,_,_,X,_,_], X).
get_list7([_,_,_,_,_,_,X,_], X).
get_list8([_,_,_,_,_,_,_,X], X).

task_to_int('A', 1).
task_to_int('B', 2).
task_to_int('C', 3).
task_to_int('D', 4).
task_to_int('E', 5).
task_to_int('F', 6).
task_to_int('G', 7).
task_to_int('H', 8).

% to calculate to near penalty
cal_too_near_pen(Mach1, Mach2, Pen) :-
	tp(Mach1, Mach2, TP) ->
		Pen is TP, !;
		Pen is 0.

% to calculate machine penalty + too near penalty
cal_pen(List, Pen) :-
	% get the each machine task
	get_list1(List, Task1),
	get_list2(List, Task2),
	get_list3(List, Task3),
	get_list4(List, Task4),
	get_list5(List, Task5),
	get_list6(List, Task6),
	get_list7(List, Task7),
	get_list8(List, Task8),

	task_to_int(Task1, Int_task1),
	task_to_int(Task2, Int_task2),
	task_to_int(Task3, Int_task3),
	task_to_int(Task4, Int_task4),
	task_to_int(Task5, Int_task5),
	task_to_int(Task6, Int_task6),
	task_to_int(Task7, Int_task7),
	task_to_int(Task8, Int_task8),
	
	mp(1, Int_task1, MP1),
	mp(2, Int_task2, MP2),
	mp(3, Int_task3, MP3),
	mp(4, Int_task4, MP4),
	mp(5, Int_task5, MP5),
	mp(6, Int_task6, MP6),
	mp(7, Int_task7, MP7),
	mp(8, Int_task8, MP8),

	cal_too_near_pen(Task1, Task2, TP1),
	cal_too_near_pen(Task2, Task3, TP2),
	cal_too_near_pen(Task3, Task1, TP3),
	cal_too_near_pen(Task4, Task5, TP4),
	cal_too_near_pen(Task5, Task6, TP5),
	cal_too_near_pen(Task6, Task7, TP6),
	cal_too_near_pen(Task7, Task8, TP7),
	cal_too_near_pen(Task8, Task1, TP8),
	
	Pen is MP1+MP2+MP3+MP4+MP5+MP6+MP7+MP8+TP1+TP2+TP3+TP4+TP5+TP6+TP7+TP8.

% to compare input list penalty with the minimal penalty
% if smaller, then change the sol in the cluse
cmp_pen(List) :-
	cal_pen(List, Pen),
	sol(Minimal_list, Minimal_pen),

	@<(Pen, Minimal_pen) ->
		retract(sol(Minimal_list, Minimal_pen)),
		asserta(sol(List, Pen));
		true.


algo(Base_set) :-
	% generate permutation based on the base set
	permutation(Base_set, List),

	% testing message
	% cal_pen(List, Pen),
	% write(List), write(Pen),nl,

	% check the hard constraints
	check_hard_constraints(List),

	% compare the current assignment with the minimal assignment/penalty
	cmp_pen(List),

	% to see if meet the end of the Base_set
	reverse(Base_set, End_base_set),
	==(List, End_base_set).

% to check forced partial assignment
check_hard_constraints(List) :-
	% get the each machine task
	get_list1(List, Task1),
	get_list2(List, Task2),
	get_list3(List, Task3),
	get_list4(List, Task4),
	get_list5(List, Task5),
	get_list6(List, Task6),
	get_list7(List, Task7),
	get_list8(List, Task8),

	% to check each forced partial
	check_fp('1', Task1), !,
	check_fp('2', Task2), !,
	check_fp('3', Task3), !,
	check_fp('4', Task4), !,
	check_fp('5', Task5), !,
	check_fp('6', Task6), !,
	check_fp('7', Task7), !,
	check_fp('8', Task8), !,

	% to check each forbidden machine
	check_fm('1', Task1), !,
	check_fm('2', Task2), !,
	check_fm('3', Task3), !,
	check_fm('4', Task4), !,
	check_fm('5', Task5), !,
	check_fm('6', Task6), !,
	check_fm('7', Task7), !,
	check_fm('8', Task8), !,

	% to check each too near assignment
	check_tk(Task1, Task2), !,
	check_tk(Task2, Task3), !,
	check_tk(Task3, Task4), !,
	check_tk(Task4, Task5), !,
	check_tk(Task5, Task6), !,
	check_tk(Task6, Task7), !,
	check_tk(Task7, Task8), !,
	check_tk(Task8, Task1), !.

% to check each forced partial assignment
% false when it is NOT partial assignment
check_fp(Mach, Task) :-
	fp(Mach, Task) ->
		true;
		fp(Mach, _) ->
		false;
		true.

% to check each forbidden machine
% false when it IS forbidden machine assign
check_fm(Mach, Task) :-
	fm(Mach, Task) ->
		false;
		true.

% to check each too near task
% false when it IS too near machine assign
check_tk(Task1, Task2) :-
	tk(Task1, Task2) ->
		false;
		true.

% write the result to the output file
output_result(Out_file) :-
	% get the final solution
	sol(List, Pen),
	==(List, ['A','A','A','A','A','A','A','A']) ->
	printErrorAndClose(Out_file, 'No valid solution possible!');
	% get the final solution
	sol(List, Pen),

	% get the each machine task
	get_list1(List, Task1),
	get_list2(List, Task2),
	get_list3(List, Task3),
	get_list4(List, Task4),
	get_list5(List, Task5),
	get_list6(List, Task6),
	get_list7(List, Task7),
	get_list8(List, Task8),

	% write the result to the output file
	open(Out_file, write, OutStream),
    write(OutStream, 'Solution '),
	write(OutStream, Task1),
	write(OutStream, ' '),
	write(OutStream, Task2),
	write(OutStream, ' '),
	write(OutStream, Task3),
	write(OutStream, ' '),
	write(OutStream, Task4),
	write(OutStream, ' '),
	write(OutStream, Task5),
	write(OutStream, ' '),
	write(OutStream, Task6),
	write(OutStream, ' '),
	write(OutStream, Task7),
	write(OutStream, ' '),
	write(OutStream, Task8),
	write(OutStream, '; Quality: '),
	write(OutStream, Pen).

