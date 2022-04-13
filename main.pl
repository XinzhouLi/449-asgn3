% the main exection file
:- initialization(main).

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