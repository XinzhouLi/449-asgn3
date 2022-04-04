% Define module
:- module(validator,[validTupleMT/2, validTupleTT/2, validTriple/2, validGrid/2]).

%Import readLines
:- use_module(input_output,[readLines/3,printErrorAndClose/2]).

% Validator rules
isMachine(Elem):- 
    member(Elem, ["1","2","3","4","5","6","7","8"]).
isTask(Elem):- 
    member(Elem,["A","B","C","D","E","F","G","H"]).
isPen(Num):- 
    Num >= 0.

% Validate tuples (m,t)
validTupleMT([],_).
validTupleMT([[Mach,Task|_]|Tuples],OutputFile):-
    isMachine(Mach),
    isTask(Task) -> 
    validTupleMT(Tuples,OutputFile);
    printErrorAndClose(OutputFile,"invalid machine/task").

% Validate tuples (t,t)
validTupleTT([],_).
validTupleTT([[Task1,Task2|_]|Tuples],OutputFile):-
    isTask(Task1),
    isTask(Task2) ->
    validTupleMT(Tuples,OutputFile);
    printErrorAndClose(OutputFile,"invalid machine/task").

% Validate triples (t,t,p)
validTriple([],_).
validTriple([[Task1,Task2,Pen|_]|Triples],OutputFile):-
    checkTask(Task1,Task2,OutputFile),
    checkPen(Pen,OutputFile),
    validTupleMT(Triples,OutputFile).

checkTask(Task1,Task2,OutputFile):-
    isTask(Task1),
    isTask(Task2) -> true ;
    printErrorAndClose(OutputFile,"invalid task").

checkPen(Pen,OutputFile):-
    isPen(Pen) -> true ;
    printErrorAndClose(OutputFile,"invalid penalty").

% Validate grid
validRow([]).
validRow([Num|Row]):-
    isPen(Num),
    validRow(Row).

validGrid([],_).
validGrid([Row|Grid],OutputFile):-
    validRow(Row) ->
    validGrid(Grid,OutputFile);
    printErrorAndClose(OutputFile,"invalid penalty").