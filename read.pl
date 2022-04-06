:- initialization(main).

main :-
	% dealing the input file.
	open('machpen1.txt',read, Stream),
    read_lines(Stream, List_of_line, 'res.txt'),
	% convert content into our 
	split_string_lines(List_of_line, List_lines),
	clear_empty_element([a,'',d,'',e], Perfect_lines),
	write(Perfect_lines),
	close(Stream),
    halt(0).

read_lines(InStream,W,OutputFile):- 
	get_code(InStream,Char), 
	checkCharAndReadRest(Char,Chars,InStream,OutputFile), 
	atom_codes(W,Chars). 

checkCharAndReadRest(37,[],_,OutputFile):-  
	printErrorAndClose(OutputFile,"Error while parsing input file").
	
checkCharAndReadRest(35,[],_,OutputFile):-  
	printErrorAndClose(OutputFile,"Error while parsing input file").
   
checkCharAndReadRest(-1,[],_,_):-  !. 
	
checkCharAndReadRest(end_of_file,[],_,_):-  !. 
	
checkCharAndReadRest(Char,[Char|Chars],InStream,OutputFile):- 
	get_code(InStream,NextChar), 
	checkCharAndReadRest(NextChar,Chars,InStream,OutputFile).

% Output if error message produced and close program
% ErrorMsg is a stream
printErrorAndClose(FileName,ErrorMsg):-
    open(FileName,write,OutputFileStream),
    write(OutputFileStream,ErrorMsg), nl(OutputFileStream),
    close(OutputFileStream).

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
