%define module
:- module(labels,[checkLabels/2,get_forced_partial/2,get_forbidden_machine/2,get_toonear_tasks/2,get_machine_penalties/2,get_toonear_penalties/2,split_input_to_list/2]).

%Import readLines
:- use_module(input_output,[readLines/3,printErrorAndClose/2]).

label("Name:").
label("forced partial assignment:").
label("forbidden machine:").
label("too-near tasks:").
label("machine penalties:").
label("too-near penalities").

%Takes in list of lines from input, checks if the labels are all there
checkLabels(ListOfLines, OutputFile):-
        label(X), is_member(X, ListOfLines, OutputFile).

%Recursively checks if X is a member of a list
is_member(_,[], OutputFile):-
    printErrorAndClose(OutputFile,"Error while parsing input file"),
    false.
is_member(X,[X|_],_):- !.
is_member(X,[_|Ys],OutputFile):-
    is_member(X,Ys,OutputFile).

split(List, Pivot, Left, Right) :- append(Left, [Pivot|Right], List).

% Partial2 = forced pairs
get_forced_partial(List, Partial2):-
    split(List, 'forced partial assignment:', _, Partial1),
    split(Partial1, 'forbidden machine:', Partial2, _).

% Partial2 = forbidden machines      
get_forbidden_machine(List, Partial2):-
    split(List, 'forbidden machine:', _, Partial1),
    split(Partial1, 'too-near tasks:', Partial2, _).
    
% Partial2 = too near tasks
get_toonear_tasks(List, Partial2):-
    split(List, 'too-near tasks:', _, Partial1),
    split(Partial1, 'machine penalties:', Partial2, _).

% Partial2 = machine penalties
get_machine_penalties(List, Partial2):-
    split(List, 'machine penalties:', _, Partial1),
    split(Partial1, 'too-near penalities', Partial2, _).

% Partial2 = too near penalties
get_toonear_penalties(List, Partial2):-
    split(List, 'too-near penalities', _, Partial2).

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