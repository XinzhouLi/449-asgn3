/* 
Failed Tests
-wrongkeyword2.txt				blank output; should be 'Error while parsing input file' (too-near penalities:)
-wrongnumbermachine.txt			blank output; should be 'invalid penalty' (1.3 machine penalty)
-wrongtask.txt					blank output; should be 'invalid machine/task' (forbidden machine (6,6))
-2wrong.txt						no output; should be 'invalid penalty' (1.5 machine penalty)
*/

:- dynamic(
	error/1,
	partialAssignment/2,
	forbiddenMachine/2,
	tooNear/2,
	machinePenalty/3,
	bestPenalty/1,
	bestList/1,
	tooNearPenalty/3
	).
		
%facts to hold best pairs list and corresponding penalty value
bestPenalty(1000000).
bestList([]).

%facts to determine if task letter is in list; used to get list of remaining tasks to assign
taskLetter('A').
taskLetter('B').
taskLetter('C').
taskLetter('D').
taskLetter('E').
taskLetter('F').
taskLetter('G').
taskLetter('H').

taskToLetter(1,'A').
taskToLetter(2,'B').
taskToLetter(3,'C').
taskToLetter(4,'D').
taskToLetter(5,'E').
taskToLetter(6,'F').
taskToLetter(7,'G').
taskToLetter(8,'H').

%starts program right away and gets file arguments from command line; passes arguments on to "main" 
:- initialization(getArguments).

getArguments :- argument_value(1, X), argument_value(2, Y), start(X,Y).

start(X,Y) :-	
	%asserta(error(none)),		%no errors at start
	read_from_file(X),			%pass file to be read and parsed
	assertz(error(none)),
	!,
	error(Z),					%check if any errors appeared when parsing
	nl,nl, write('ERRORS AFTER PARSING: '), nl, forall(error(ABS), (write(ABS),nl)),
	!,
	parseErrors(Z,Y),			%writes parsing errors to file if any and stop program; if only none then do nothing
	retractall(error(_)),		%removes any errors just to make to make sure 
	asserta(error(none)),		%adds none error
	branchBound, 				%branch and bound begins
	checkForNoSol, 				%checks current solution and if empty then add no valid solution error
	error(A), 					%check if error rule present
	nl,nl, write('ERRORS AFTER BRANCH: '), nl, forall(error(ABS), (write(ABS),nl)),
	!,
	parseErrors(A,Y), 			%writes no valid solution error to file if any and stop program; if only none then do nothing
	retract(error(_)), 			%removes any errors just to make to make sure 
	solutionformat(Out),		%formats solution
	writeFile(Y,Out),			%writes solution to file
	!.					
	

	
%----List of Facts----
nameTitle("Name:").
forcedPartialAssignTitle("forced partial assignment:").
forbiddenTitle("forbidden machine:").
tooNearTitle("too-near tasks:").
machinePenTitle("machine penalties:").
tooNearPenTitle("too-near penalities").


%----List of functions----

%Compare is only for comparing the Name of each section
%Compare each ascii character
%If Error asserts parsing error
%
compare([],[]):- !, write('Compare Succesful'),nl.
compare([H1|T1], [H2|T2]):-
	%write('compare: '), write(H1),write(' to '), write(H2), nl,
	H1 == H2 
		-> compare(T1, T2)
		; assertz(error(parseError)), nl,write('Parsing Error'),nl.
		
		%assertz(error(parseError))
		

%readLine_code
%Takes FileStream and returns List of ascii values for 1 Line
%

readLine_code(Stream, _) :-
	at_end_of_stream(Stream).
readLine_code(Stream, Line) :-
	\+  at_end_of_stream(Stream), 
	CharList = [],
	readChar_code(Stream, CharList, Line).


readChar_code(Stream, CharList, Line):-
	at_end_of_stream(Stream),
	Line = CharList.
readChar_code(Stream, CharList, Line):-
	\+  at_end_of_stream(Stream),
	get_code(Stream, Char),
	isOK(Stream, Char, CharList, Line).
	
isOK(_, 10, CharList, Line):-
	Line = CharList.
isOK(Stream, Char, CharList, Line) :-
	append(CharList, [Char], X),
	%write('(RL)Append: '), write(X),nl,
	readChar_code(Stream, X, Line).
	
	

	


%---Beginning---
%Opens file ready to be read
%
read_from_file(File) :-					
	open(File, read, Stream),
	read_name(Stream),
	write('DONE PARSING'),
	close(Stream).


	
%---Reads "Name:"---
read_name(Stream):-
	write('----- Name: -----'), nl,
	readLine_code(Stream, List),
	write('Name List:    '), write(List),nl,
	%nameTitle([X,Y,Z,W,V]),
	nameTitle(X),
	write('Expect List:  '), write(X), nl,
	compare(List, X),
	read_name_2(Stream).
	
read_name_2(Stream):-
	get_code(Stream, Char),
	put_code(Char),
	the_Name(Stream, Char),
	write('DONE NAME'),nl,nl,
	skip_line_forced(Stream).			%Continues to next section

the_Name(_, 10):-
	write('End of Name found'),nl.
the_Name(Stream, _):-
	get_code(Stream, Char),
	put_code(Char),
	the_Name(Stream, Char).
	


%Function to skip all \n before forced
%
skip_line_forced(Stream) :-				
	get_code(Stream, Char),
	skip_line_forced(Stream, Char).

skip_line_forced(Stream, 10):-			%If Char is \n(10), continue skipping
	write('skip newLine forced'), nl,
	skip_line_forced(Stream).
skip_line_forced(Stream, 32):-			%If Char is ' '(32), continue skipping
	write('skip space forced'), nl,
	skip_line_forced(Stream).
skip_line_forced(Stream, Char) :-		%If Char is something, send to forced
	write('CharFound: '), write(Char), nl,
	read_forced(Stream, Char).			




%---Reads "forced partial assignment"---
read_forced(Stream, Char1):-
	write('----- forced partial assignment -----'), nl,
	readLine_code(Stream, List),
	forcedPartialAssignTitle(X),
	append([Char1],List, List2),
	write('forced List:  '), write(List2), nl,
	write('Expect List:  '), write(X), nl,
	compare(List2, X),
	read_forced_math(Stream).
	

read_forced_math(Stream) :-
	readLine_code(Stream, Line),
	forced_MaybeEnd(Stream, Line).
	

forced_MaybeEnd(Stream, []):-
	write('End of Forced'), nl, nl,		%Should stop when \n\n is found
	skip_line_forbidden(Stream).
forced_MaybeEnd(Stream, [32]):-			%Space is found, Do nothing and continue
	write('End of Forced'), nl, nl,		%Should stop when \n\n is found
	skip_line_forbidden(Stream).
forced_MaybeEnd(Stream, Line):-			%Not \n -- [40,65,44,56,41]
	length(Line, Len),
	\+ Len = 5 	
		-> assertz(error(invalidMachineTask)), nl, write('forced FAIL 1'),nl
	; [_,X,_,_,_] = Line,
	  X < 49 	-> assertz(error(invalidMachineTask)), nl, write('forced FAIL 2'),nl
	; [_,X,_,_,_] = Line,
	  X > 56	-> assertz(error(invalidMachineTask)), nl, write('forced FAIL 3'),nl
	; [_,_,_,Y,_] = Line,
	  Y < 65	-> assertz(error(invalidMachineTask)), nl, write('forced FAIL 4'),nl
	; [_,_,_,Y,_] = Line,
	  Y > 72	-> assertz(error(invalidMachineTask)), nl, write('forced FAIL 5'),nl
	; [_,X,_,Y,_] = Line,
	char_code(NumC, X),
	number_chars(Num, [NumC]),
	char_code(Lett, Y),
	checkForForcedPartial(Num, Lett),
	asserta(partialAssignment(Num,Lett)), 
	write('assert: '), write(Num), tab(1), write(Lett),nl,
	read_forced_math(Stream).
	
checkForForcedPartial(Num, Lett) :-
	partialAssignment(Num, _) -> assertz(error(invalidPartialAssignment));
	partialAssignment(_,Lett) -> assertz(error(invalidPartialAssignment));
	!.
	%assertz(error(invalidMachineTask))

%Skips \n before forbidden
skip_line_forbidden(Stream) :-				%Function to skip all \n before forbidden
	get_code(Stream, Char),
	skip_line_forbidden(Stream, Char).

skip_line_forbidden(Stream, 10):-			%If Char is \n(10), continue skipping
	write('skip newLine forbidden'), nl,
	skip_line_forbidden(Stream).
skip_line_forbidden(Stream, 32):-			%If Char is ' '(32), continue skipping
	write('skip space forbidden'), nl,
	skip_line_forbidden(Stream).
skip_line_forbidden(Stream, Char) :-		%If Char is something, send to forbidden
	write('CharFound: '), write(Char), nl,
	read_forbidden(Stream, Char).			



%---Reads "forbidden partial assignment"---
read_forbidden(Stream, Char1):-
	write('----- forbidden machine: -----'), nl,
	readLine_code(Stream, List),
	forbiddenTitle(X),
	append([Char1],List, List2),
	write('forbidden List: '), write(List2), nl,
	write('Expect List:    '), write(X), nl,
	compare(List2, X),
	read_forbidden_math(Stream).



read_forbidden_math(Stream) :-
	readLine_code(Stream, Line),
	forbidden_MaybeEnd(Stream, Line).

forbidden_MaybeEnd(Stream, []):-
	write('End of Forbidden'), nl, nl,		%Should stop when \n\n is found
	skip_line_tooNear(Stream).
forbidden_MaybeEnd(Stream, [32]):-
	write('End of Forbidden'), nl, nl,		%Should stop when \n\n is found
	skip_line_tooNear(Stream).
forbidden_MaybeEnd(Stream, Line):-			%Not \n -- [40,65,44,56,41]
	length(Line, Len),
	\+ Len = 5 	
		-> assertz(error(invalidMachineTask)), nl, write('Forbidden FAIL 1'),nl
	; [_,X,_,_,_] = Line,
	  X < 49 	-> assertz(error(invalidMachineTask)), nl, write('Forbidden FAIL 2'),nl
	; [_,X,_,_,_] = Line,
	  X > 56	-> assertz(error(invalidMachineTask)), nl, write('Forbidden FAIL 3'),nl
	; [_,_,_,Y,_] = Line,
	  Y < 65	-> assertz(error(invalidMachineTask)), nl, write('Forbidden FAIL 4'),nl
	; [_,_,_,Y,_] = Line,
	  Y > 72	-> assertz(error(invalidMachineTask)), nl, write('Forbidden FAIL 5'),nl
	; [_,X,_,Y,_] = Line,
	  char_code(NumC, X),
	  number_chars(Num, [NumC]),
	  char_code(Lett, Y),
	  asserta(forbiddenMachine(Num,Lett)), write('assert: '), write(X), tab(1), write(Y), nl,
	read_forbidden_math(Stream).
	
	%assertz(error(invalidMachineTask))

	


%Skips \n before TOONEAR
skip_line_tooNear(Stream) :-				%Function to skip all \n before forced
	get_code(Stream, Char),
	skip_line_tooNear(Stream, Char).

skip_line_tooNear(Stream, 10):-			%If Char is \n(10), continue skipping
	write('skip newLine tooNear'), nl,
	skip_line_tooNear(Stream).
skip_line_tooNear(Stream, 32):-			%If Char is ' '(32), continue skipping
	write('skip space tooNear'), nl,
	skip_line_tooNear(Stream).
skip_line_tooNear(Stream, Char) :-		%If Char is something, send to forced
	write('CharFound: '), write(Char), nl,
	read_tooNear(Stream, Char).	


%----Read tooNear----
read_tooNear(Stream, Char1) :-
	write('----- too-near task: -----'), nl,
	readLine_code(Stream, List),
	tooNearTitle(X),
	append([Char1],List, List2),
	write('tooNear List: '), write(List2), nl,
	write('Expect List:  '), write(X), nl,
	compare(List2, X),
	read_tooNear_math(Stream).


read_tooNear_math(Stream):-
	readLine_code(Stream, Line),
	tooNear_MaybeEnd(Stream, Line).

tooNear_MaybeEnd(Stream, []):-
	write('End of tooNear'), nl, nl,		%Should stop when \n\n is found
	skip_line_machPen(Stream).
tooNear_MaybeEnd(Stream, [32]):-
	write('End of tooNear'), nl, nl,		%Should stop when \n\n is found
	skip_line_machPen(Stream).
tooNear_MaybeEnd(Stream, Line):-			%Not \n -- [40,50,44,55,41]
	length(Line, Len),
	\+ Len = 5	-> assertz(error(invalidMachineTask)), nl, write('tooNear FAIL 1'),nl
	; [_,X,_,_,_] = Line,
	  X < 65 	-> assertz(error(invalidMachineTask)), nl, write('tooNear FAIL 2'),nl
	; [_,X,_,_,_] = Line,
	  X > 72	-> assertz(error(invalidMachineTask)), nl, write('tooNear FAIL 3'),nl
	; [_,_,_,Y,_] = Line,
	  Y < 65	-> assertz(error(invalidMachineTask)), nl, write('tooNear FAIL 4'),nl
	; [_,_,_,Y,_] = Line,
	  Y > 72	-> assertz(error(invalidMachineTask)), nl, write('tooNear FAIL 5'),nl
	; [_,X,_,Y,_] = Line,
	char_code(Lett1, X),
	char_code(Lett2, Y),
	  asserta(tooNear(Lett1,Lett2)), write('assert: '), write(X), tab(1), write(Y), nl,
	read_tooNear_math(Stream).
	
	%assertz(error(invalidMachineTask))
	
	
	
	
%Skips \n before machinePen
skip_line_machPen(Stream) :-				%Function to skip all \n before forced
	get_code(Stream, Char),
	skip_line_machPen(Stream, Char).

skip_line_machPen(Stream, 10):-			%If Char is \n(10), continue skipping
	write('skip newLine machPen'), nl,
	skip_line_machPen(Stream).
skip_line_machPen(Stream, 32):-			%If Char is ' '(32), continue skipping
	write('skip space machPen'), nl,
	skip_line_machPen(Stream).
skip_line_machPen(Stream, Char) :-		%If Char is something, send to forced
	write('CharFound: '), write(Char), nl,
	read_machPen(Stream, Char).	

%----Read machPen----
read_machPen(Stream, Char1) :-
	write('----- Machine Pen: -----'), nl,
	readLine_code(Stream, List),
	machinePenTitle(X),
	append([Char1],List, List2),
	write('machine List: '), write(List2), nl,
	write('Expect List:  '), write(X), nl,
	compare(List2, X),
	read_machPen_math(Stream).
	

read_machPen_math(Stream):-
	machPenAssertStart(Stream),
	write('End of Machine Pen'), nl, nl,
	%CHECK NUMBER OF ASSERTS FOR MACHPEN
	factCount(Num),
	%\+Num == 64 -> assertz(error(invalidMachinePenalty));
	write('Num of Machine Asserts: '), write(Num), nl, nl,
	skip_line_tooNearPen(Stream).			%Continues to the next section

factCount(Num) :- 
	findall((_,_,_), machinePenalty(_,_,_), List), 
	%write('List of Facts(ALOT): '), write(List), nl,
	length(List, Num).


%MACHINE PEN ASSERT STUFF
machPenAssertStart(Stream) :- 
	machPenAssertLeft(Stream, 1, 1, []).
	
machPenAssertLeft(Stream, Row, Col, List) :-
%write('Row: '), write(Row), nl,
%write('Col: '), write(Col), nl,
	Row > 8 -> machPenAssertRow(Stream)
	; get_code(Stream, Char),
	machPenAssertWHAT(Stream, Row, Col, List, Char).
	
%Checks for anything after 8 rows that is not a newLine
machPenAssertRow(Stream) :-
	get_code(Stream, Char),
	\+ Char == 10 ->  assertz(error(invalidMachinePenalty)), write('machine Pen Row FAIL'), nl
	; write('Done MachinePenAssert').

machPenAssertWHAT(Stream, Row, Col, List, 10):-					%n\ is found
	List == [] 	-> Row2 is Row + 1, machPenAssertLeft(Stream, Row2, 1, [])
	; number_chars(Val, List),
	write('MACH ASSERT: '), write(Row), tab(1), write(Col), tab(1), write(Val), nl, 
	taskToLetter(Col, Lett),
	assertz(machinePenalty(Row, Lett, Val)),
	Row2 is Row + 1, machPenAssertLeft(Stream, Row2, 1, []).
machPenAssertWHAT(Stream, Row, Col, List, 32):-					%Space is found
	List == [] 	-> machPenAssertLeft(Stream, Row, Col, [])
	; number_chars(Val, List),
	write('MACH ASSERT: '), write(Row), tab(1), write(Col), tab(1), write(Val),nl, 
	taskToLetter(Col, Lett),
	assertz(machinePenalty(Row, Lett, Val)),
	Col2 is Col + 1, machPenAssertLeft(Stream, Row, Col2, []).
machPenAssertWHAT(Stream, Row, Col, List, Char):-
	Char < 48   -> assertz(error(invalidMachineTask)), nl, write('machine FAIL 1'),nl
	; Char > 57 -> assertz(error(invalidMachineTask)), nl, write('machine FAIL 2'),nl
	; atom_codes(X, [Char]), 
	append(List, [X], NewList),
	machPenAssertLeft(Stream, Row, Col, NewList).
	



	
%Skips \n before tooNearPen
skip_line_tooNearPen(Stream) :-				%Function to skip all \n before forced
	get_code(Stream, Char),
	skip_line_tooNearPen(Stream, Char).

skip_line_tooNearPen(Stream, 10):-			%If Char is \n(10), continue skipping
	write('skip newLine tooNearPen'), nl,
	skip_line_tooNearPen(Stream).
skip_line_tooNearPen(Stream, 32):-			%If Char is ' '(32), continue skipping
	write('skip space tooNearPen'), nl,
	skip_line_tooNearPen(Stream).
skip_line_tooNearPen(Stream, Char) :-		%If Char is something, send to forced
	write('CharFound: '), write(Char), nl,
	read_tooNearPen(Stream, Char).	
	
checkMachPenTotal(X) :-
	factCount(Num),
	Num =\= 64 -> assertz(error(invalidMachinePenalty));
	!.
	
	
%----Read TooNearPen----
read_tooNearPen(Stream, Char1) :-
	checkMachPenTotal(Num),
	write('----- Too Near Pen: -----'), nl,
	readLine_code(Stream, List),
	tooNearPenTitle(X),
	append([Char1],List, List2),
	write('tooNearPen List: '), write(List2), nl,
	write('Expect List:     '), write(X), nl,
	compare(List2, X), nl,
	write('Start tooNearPen Math'),nl,
	read_tooNearPen_math(Stream).
	
%ADD STUFF HERE ----------------------------------------
read_tooNearPen_math(Stream):-
	at_end_of_stream(Stream),
	write('END OF FILE'), nl.
read_tooNearPen_math(Stream):-
	\+  at_end_of_stream(Stream), 
	readLine_code(Stream, Line),
	tooNearPen_MaybeEnd(Stream, Line).


tooNearPen_MaybeEnd(Stream, []):-
	read_tooNearPen_math(Stream).		%Should stop when \n\n is found
tooNearPen_MaybeEnd(Stream, [32]):-
	read_tooNearPen_math(Stream).		%Should stop when \n\n is found
tooNearPen_MaybeEnd(Stream, Line):-			%Not \n -- [40,50,44,55,41]
	write('tooNearPen Line: '), write(Line), nl,
	length(Line, Len),
	Len < 7	-> assertz(error(invalidMachineTask)), nl, write('tooNearPen FAIL 1'),nl
	; [_,X,_,_,_|T] = Line,
	  X < 65 	-> assertz(error(invalidTask)), nl, write('tooNearPen FAIL 2'),nl
	; [_,X,_,_,_|T] = Line,
	  X > 72	-> assertz(error(invalidTask)), nl, write('tooNearPen FAIL 3'),nl
	; [_,_,_,Y,_|T] = Line,
	  Y < 65	-> assertz(error(invalidTask)), nl, write('tooNearPen FAIL 4'),nl
	; [_,_,_,Y,_|T] = Line,
	  Y > 72	-> assertz(error(invalidTask)), nl, write('tooNearPen FAIL 5'),nl
	; [_,X,_,Y,_|T] = Line,
	char_code(Lett1, X),
	char_code(Lett2, Y),
	tooNearPenGet(Line, Val),
	write('assert: '), write(X), tab(1), write(Y), tab(1), write(Val), nl,
	asserta(tooNearPenalty(Lett1,Lett2,Val)),
	read_tooNearPen_math(Stream).


tooNearPenGet([_,_,_,_,_|Num], Val):-
	write('START tooNearPenGet'),nl, write('Num List: '), write(Num), nl,
	tooNearPenGetCheck(Num, [], Val).

tooNearPenGetCheck([], Good, Val) :-
	assertz(error(invalidPenalty)), nl, write('tooNearPen Pen FAIL 1'), nl.
tooNearPenGetCheck([H|T], Good, Val):-
	H == 41 ->  number_chars(Val, Good)
	; H < 48 -> assertz(error(invalidPenalty)), nl, write('tooNearPen Pen FAIL 2'), nl
	; H > 57 -> assertz(error(invalidPenalty)), nl, write('tooNearPen Pen FAIL 3'), nl
	; 
	char_code(Char, H),
	append(Good, [Char], NewGood),
	tooNearPenGetCheck(T, NewGood, Val).
	


	%assertz(error(invalidMachineTask))
	%assertz(error(invalidTask))
	
	
	%FOR REMAINING ERROR MESSAGES
	%assertz(error(invalidPenalty))  => error for number in both penalties is not a natural number
	%assertz(error(invalidMachinePenalty))  => error for rows and columns in machine penalty constraint =/= 8
	
branchBound :-
	setForcedPartial([0,0,0,0,0,0,0,0], 1, Pairs), %assign forced partials
	getRemainingTasks(Pairs, ['A','B','C','D','E','F','G','H'], 1, Remaining), %determine what tasks need to be assigned after forced partials
	solve(Pairs, Remaining),%start making 'nodes'
	!.
branchBound.

solve(Pairs, []) :-  %no remaining tasks to assign; all forced
	evaluate(Pairs),  %check hard constraints
	calculatePenaltyOfList(Pairs, Penalty), %calculate penalty
	retract(bestList(_List)), %get rid of previous best list facts
	asserta(bestList(Pairs)), %add this list as best list
	retract(bestPenalty(_Val)), %get rid of previous best penalty facts
	asserta(bestPenalty(Penalty)). %add this penalty as best panalty
solve(_Pairs, []).
solve(Pairs, Remaining) :-
	evaluate(Pairs), %check hard constraints 
	solveHelper(Pairs, Remaining, Remaining). %start to add remaining tasks
solve(_Pairs, _Remaining).

solveHelper(_Pairs, _Remaining, []). %base case emtpy remaining tasks
solveHelper(Pairs, Remaining, [Task|Tasks]) :-
	removeElement(Task, Remaining, Remaining_), %remove tasks at head of remaining  
	assignTask(Pairs, Task, Pairs_), %assign task to first dummy value; i.e. 0
	solve(Pairs_, Remaining_), %continue with rest of remaining tasks on same node
	solveHelper(Pairs, Remaining, Tasks). %create additional branches from previous node 

assignTask([], _Task, []).
assignTask([0|List], Task, [Task|List]). %base case to assign task to first dummy value; i.e. 0
assignTask([X|List], Task, [X|Pairs_]) :- 
	assignTask(List, Task, Pairs_), %head is not a dummy value, i.e. 0, recursively assign tasks remaining 
	!.

%check hard constraints and calculate penalty and compare
evaluate(Pairs) :-
	validHelper(Pairs, Pairs, 1), %check that hard constraints are met
	!,
	calculatePenaltyOfList(Pairs, Penalty), %if so only then calculate penalty of pairs list
	!,
	bestPenalty(BestPenalty), %check best pen
	Penalty < BestPenalty. %compare pen

%check too near and forbidden hard constraints 
validHelper([Task2|_Pairs], [Task1], Machine) :- %base case; only one left to check
	\+forbiddenMachine(Machine,Task1), %task at machine is not forbidden
	\+tooNear(Task1, Task2). %tasks are not next to each other
validHelper(Pairs, [Task1, Task2|Tasks], Machine) :-
	\+forbiddenMachine(Machine,Task1), %task at machine is not forbidden
	\+tooNear(Task1, Task2), %tasks are not next to each other
	NextMachine is Machine + 1, %increment counter
	validHelper(Pairs, [Task2|Tasks], NextMachine). %recursive call 

setForcedPartial(List,9,List). %stop when mach is 9; base case
setForcedPartial(List,Mach,Returned) :-
	error(none), %no errors so far
	NewMach is Mach +1, %assign next machine
	setupHelper(List,Mach,ModList), %check for same forced and forbidden and replace
	!,
	error(none), %no errors so far
	setForcedPartial(ModList,NewMach,Returned), %recursion for rest of list
	!.
	
setupHelper(List,Mach,Return) :-
	partialAssignment(Mach,Task), %check forced fact
	\+forbiddenMachine(Mach,Task), %check if forced is also forbidden 
	replacePosition(List,Task,Mach,Return). %add to list
setupHelper(List,Mach,List) :- \+partialAssignment(Mach,_Task). %only 8 machines 
	
replacePosition([_H|T],Task,1,[Task|T]). %first index replaced; base case
replacePosition([H|T],Task,Position,[H|Rest]) :- %task replaces dummy value zero; recursively 
	NextPosition is Position - 1, %work backwards to base case
	replacePosition(T,Task,NextPosition,Rest). %recursive call
	
getRemainingTasks([H|_T],ListOfRemaining,8,RemainingTasks) :-  %base case; last index
	taskLetter(H), %check is task at head of current pairs list is an actual task
	removeElement(H,ListOfRemaining,RemainingTasks). %if so remove from list of all tasks and add to return list with remaing tasks
getRemainingTasks([H|T],ListOfRemaining,N,RemainingTask) :- %recursively check if pairs lists contains a task; if so remove it from list with all tasks; what's left is list with remaining tasks
	taskLetter(H), %check is task at head of current pairs list is an actual task
	removeElement(H,ListOfRemaining,List),%if so remove from list of all tasks and add to return list with remaing tasks
	Next is N+1, %increment counter
	getRemainingTasks(T,List,Next,RemainingTask). %recursive call
getRemainingTasks([H|_T],ListOfRemaining,8,ListOfRemaining) :- \+taskLetter(H). %base case; task at head of pairs list is not a tasks; i.e. still zero
getRemainingTasks([H|T],ListOfRemaining,N,RemainingTask) :-
	\+taskLetter(H), %task at head of pairs list is not a tasks; i.e. still zero
	Next is N+1, %increment counter
	getRemainingTasks(T,ListOfRemaining,Next,RemainingTask). %recursive call
		
removeElement(_X, [], []). %list is empty
removeElement(X, [X], []). %only thing in list
removeElement(_X, [Y], [Y]). %not in list
removeElement(X, [X|XS], XS). %head of list
removeElement(X, [Y|XS], [Y|YS]) :- removeElement(X, XS, YS). %recursively find element
	
calculatePenaltyOfList([],0). %empty list
calculatePenaltyOfList(L,P) :-
	calcTooNearPen(L,0,Result), %get too near penalty; in Result
	calcMachPen(L,1,X), %get machine penalty; in X; i.e. list of penalties for pairs list
	sumOfListPen(X,Z), %sum the list of machine penalties for pairs list
	P is Result + Z. %total penalty P is machine penalty + too near penalty

sumOfListPen([],0). %base case; empty list
sumOfListPen([H|T],S) :- 
	sumOfListPen(T,Rest), %recursive call
	S is H + Rest. %S = head penalty value plus rest of list
	
calcMachPen([0|T],N,[0|Y]) :- %still a dummy value	at head	
	Next is N+1, %increment counter
	calcMachPen(T,Next,Y), %recursive call for rest of pairs list
	!.
calcMachPen([H|T],N,[X|Y]) :- 
	machinePenalty(N,H,X), %get machine penalty fact for head
	Next is N+1, %increment counter
	calcMachPen(T,Next,Y), %recursive call for rest of pairs list
	!.
calcMachPen([],_,[]).
	
getLastElement([E],E). %base case only one thing in list; last element
getLastElement([_E|T],X) :- getLastElement(T,X). %recursive call 

calcTooNearPen(L,Current,R) :-
	calcTooNearPenHelper(L,Current,Result), %get too near penalty for too near that are not machine 8 and 1
	!,
	getLastElement(L,Y), %get final task; i.e. machine 8 task
	!,
	[Head|_T]=L, %get task at head of list; i.e. machine 1 task
	getTooNearPen(Y,Head,V), %check for machine 8 and 1 too near
	R is Result + V. %total too near penalty in R

calcTooNearPenHelper([_E],Current,Current).	%return current value for any term
calcTooNearPenHelper([E,E1|Rest],Current,Result) :-
	getTooNearPen(E,E1,Val), %get too near penalty
	Sum is Current + Val, %add value to current sum
	calcTooNearPenHelper([E1|Rest],Sum,Result). %recursive call for rest of list
	
getTooNearPen(X,Y,V) :-
	tooNearPenalty(X,Y,Z), %get too near penalty fact
	!,
	0<Z, %if return penalty from fact is greater then 0
	V is Z; %return that penalty (; means disjunction)
	V is 0. %or return 0; i.e. too near penalty fact does not exist in constraint for tasks X and Y

		
%check if list is not empty otherwise no valid solution
checkForNoSol:-
	bestList([]), %best list empty
	retract(error(_)), %remove all other error facts present
	assertz(error(noValidSolution)). %add noValidSolution fact
checkForNoSol.

%write error message to file is there is one; if not then do nothing	
parseErrors(none, _). %error(none)
parseErrors(invalidPartialAssignment, X):- %error(invalidPartialAssignment)
	writeFile(X,"partial assignment error"),
	fail.
parseErrors(invalidMachineTask, X):- %error(invalidMachineTask)
	writeFile(X,'invalid machine/task'),
	fail.
parseErrors(invalidMachinePenalty, X):- %error(invalidMachinePenalty)
	writeFile(X,"machine penalty error"),
	fail.
parseErrors(invalidTask, X):- %error(invalidTask)
	writeFile(X,"invalid task"),
	fail.
parseErrors(invalidPenalty, X):- %error(invalidPenalty)
	writeFile(X,"invalid penalty"),
	fail.
parseErrors(parseError, X):- %error(parseError)
	writeFile(X,"Error while parsing input file"),
	fail.
parseErrors(noValidSolution, X):-
	writeFile(X,"No valid solution possible!"),
	fail.
	
%format solution 
solutionformat(FinalOutput):-
	Output1 = "Solution ",
	bestList(X),
	getElement(1,X,Y1),
	atom_codes(Y1,Z1),
	append(Output1,Z1,Output1_),
	append(Output1_," ",Output2),
	getElement(2,X,Y2),
	atom_codes(Y2,Z2),
	append(Output2,Z2,Output2_),
	append(Output2_," ",Output3),
	getElement(3,X,Y3),
	atom_codes(Y3,Z3),
	append(Output3,Z3,Output3_),
	append(Output3_," ",Output4),
	getElement(4,X,Y4),
	atom_codes(Y4,Z4),
	append(Output4,Z4,Output4_),
	append(Output4_," ",Output5),
	getElement(5,X,Y5),
	atom_codes(Y5,Z5),
	append(Output5,Z5,Output5_),
	append(Output5_," ",Output6),
	getElement(6,X,Y6),
	atom_codes(Y6,Z6),
	append(Output6,Z6,Output6_),
	append(Output6_," ",Output7),
	getElement(7,X,Y7),
	atom_codes(Y7,Z7),
	append(Output7,Z7,Output7_),
	append(Output7_," ",Output8),
	getElement(8,X,Y8),
	atom_codes(Y8,Z8),
	append(Output8,Z8,Output9),
	append(Output9,"; Quality: ",Output10),
	bestPenalty(Y9),
	number_codes(Y9,Z9),
	append(Output10,Z9,FinalOutput), !.	
	
%get element from list for writing output
getElement(1,[H|_],H).
getElement(X,[_|T],Z):-
	Y is X - 1,
	getElement(Y,T,Z).
	
	
%write solution to file 
%pass file and what to write
writeFile(X,Y):-
	tell(X),
	atom_codes(W,Y),
	write(W),
	nl,
	told,
	!.