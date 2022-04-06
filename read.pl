:- initialization(main).

main :-
	open('machpen1.txt',read, Stream),
    readLines(Stream, List_of_line, 'res.txt'),
	split_input_to_list(List_of_line, List_lines),
	% write(List_lines),
	clear_empty_element([a,'',d,'',e], Perfect_lines),

	% check_empty(a,X,[b]),
	% delete_empty([a,b,'',c,'',d], Perfect_lines),
	% X = [a,b],
	% append([],X,Y),
	% write(Y),
	% write(Perfect_lines),
	% sum_of_list([1,2,3],X),
	write(Perfect_lines),
	close(Stream),
    halt(0).

readLines(InStream,W,OutputFile):- 
	get_code(InStream,Char), 
	checkCharAndReadRest(Char,Chars,InStream,OutputFile), 
	atom_codes(W,Chars). 

checkCharAndReadRest(37,[],_,OutputFile):-  
	printErrorAndClose(OutputFile,"Error while parsing input file").
	
	% if char is # symbol for comment
checkCharAndReadRest(35,[],_,OutputFile):-  
	printErrorAndClose(OutputFile,"Error while parsing input file").
   
	% if at end of stream
checkCharAndReadRest(-1,[],_,_):-  !. 
	
	% if at end of file
checkCharAndReadRest(end_of_file,[],_,_):-  !. 
	
	% otherwise keep reading
checkCharAndReadRest(Char,[Char|Chars],InStream,OutputFile):- 
	get_code(InStream,NextChar), 
	checkCharAndReadRest(NextChar,Chars,InStream,OutputFile).

% Output if error message produced and close program
% ErrorMsg is a stream
printErrorAndClose(FileName,ErrorMsg):-
    open(FileName,write,OutputFileStream),
    write(OutputFileStream,ErrorMsg), nl(OutputFileStream),
    close(OutputFileStream).
    %halt. %Closes SWI-Prolog, but probably needed for final version


split_input_to_list(InputText,List):-
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


delete_empty(Input, Output):-
	clear_empty_element(Input,Output).

clear_empty_element([],[]).
clear_empty_element([H|T], Output_list) :-
	clear_empty_element(T, Result),
	check_empty(H, Result, Output_list ).
	% write(Output_list),nl.

check_empty(' ',Input,Output):-
	append(Input,[] , Output).
check_empty('',Input,Output):-
	append(Input,[] , Output).
check_empty(El, Input, Output):- 
	append([El], Input , Output).

% clear_empty_element([],_).
% clear_empty_element([H|T], Output_list) :-
% 	% write(Output_list),nl,
% 	check_empty(H, Output_list),
% 	write(Output_list),write(H),nl,
% 	clear_empty_element(T, Output_list).

% check_empty('',Input):-
% 	append(Input,[],Input).
% check_empty(El, Input):- 
% 	append(Input,[El] , Input).

sum_of_list( []     , 0 ) .
sum_of_list( [N|Ns] , S ) :-
  sum_of_list(Ns,T) ,
  S is T+N
  .
	