% initialization needed for exe to run main automatically 
% (COMMENT OUT TO PREVENT RUNNING AUTOMATICALLY WHEN TESTING)

% :- initialization(main).
:- dynamic(main/0).

getInputName('TestFiles/invalid2.txt').
getOutputName('output_iv2.txt').

% Main functor
main:-
    % Get console argument values (COMMENT OUT FOR TESTING)
    % current_prolog_flag(argv, [_,InputFileName,OutputFileName|_]),

    % Manually set input file (COMMENT OUT FOR FINAL VERS)
    getInputName(InputFileName),
    getOutputName(OutputFileName),

    % Open file and get lines of text
    getLines(InputFileName,OutputFileName,LinesOfFile),

    % spy(splitlines),
    splitlines(LinesOfFile,ListOfLines),
    print(ListOfLines),

    % Check file text for comments or label errors (Scott)
    % spy(checkLabels),
    Labels = ['Name:','forced partial assignment:','forbidden machine:','too-near tasks:','machine penalties:','too-near penalities'],
    checkLabels(Labels,ListOfLines,OutputFileName),
    % print(ListOfLines),

    % spy(get_forced_partial),
    get_forced_partial(ListOfLines, ForcedPartial),
    get_forbidden_machine(ListOfLines, ForbiddenMachine),
    get_toonear_tasks(ListOfLines, TooNearTasks),
    get_machine_penalties(ListOfLines, MachinePenalties),
    get_toonear_penalties(ListOfLines, TooNearPenalties),
    print(ForcedPartial),
    print(ForbiddenMachine),
    print(TooNearTasks),
    print(MachinePenalties),
    print(TooNearPenalties),

    % Parse lines of text to get values (Oliver)
    % spy(parse_pairs),
    parse_pairs(ForcedPartial,Forced),
    parse_pairs(ForbiddenMachine,Forbid),
    parse_pairs(TooNearTasks,TooNear),
    parse_penalty_grid(MachinePenalties,Grid,OutputFileName),
    parse_triples(TooNearPenalties,TooNearPen),

    % Check parsed values for errors (Khalid)
    % spy(validTupleMT),
    validTupleMT(Forced,OutputFileName),
    validTupleMT(Forbid,OutputFileName),
    validTupleTT(TooNear,OutputFileName),
    validGrid(Grid,OutputFileName),
    validTriple(TooNearPen,OutputFileName),

    % Check hard constraints (Fungai, Jacky)
    % spy(forcedDouble),
    forcedDouble(Forced,OutputFileName),
    forcedConflicts(Forced,Forbid,OutputFileName),

    % Make matches
    spy(createForcedMatches),
    Empty = ['x','x','x','x','x','x','x','x'],
    AllTasks = ['A','B','C','D','E','F','G','H'],
    createForcedMatches(Forced,Empty,ForcedMatches),
    print(ForcedMatches),
    getRemainingTasks(ForcedMatches,AllTasks,RemainingTasks),
    print(RemainingTasks),
    fillMatches(RemainingTasks,TooNear,ForcedMatches,FilledMatches,OutputFileName),

    % Check soft constraints
    % createAllMatches(['A','B','C','D','E','F','G','H'],AllMatches),
    % filterValidMatches(AllMatches,Forced,Forbid,TooNear,ValidMatches),
    % getBestMatches(ValidMatches,ValidMatches,Grid,TooNearPen,Total,BestMatches),

    % Close file and write output
    % createSolution(BestMatches,Total,Solution),
    % write(Solution,OutputFileName),  
    nl.


%%%%%%%%%%%%%%%%%% Input / Output %%%%%%%%%%%%%%%%

getLines(InputFileName,OutputFileName,LinesOfFile):-
    open(InputFileName,read,Str), 
    readLines(Str,LinesOfFile,OutputFileName),
    close(Str).
    
% Read all lines in file
readLines(InStream,W,OutputFile):- 
    get_code(InStream,Char), 
    checkCharAndReadRest(Char,Chars,InStream,OutputFile), 
    atom_codes(W,Chars). 

% if char is newline
% checkCharAndReadRest(10,[],_):-  !. 

% if char is blank space
% checkCharAndReadRest(32,[],_):-  !. 

% if char is % symbol for comment
checkCharAndReadRest(37,[],_,OutputFile):-  
    printErrorAndClose(OutputFile,'Error while parsing input file').

% if char is # symbol for comment
checkCharAndReadRest(35,[],_,OutputFile):-  
    printErrorAndClose(OutputFile,'Error while parsing input file').

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
    close(OutputFileStream),
    stop. %Closes SWI-Prolog, but probably needed for final version

% Create solution output
createSolution(BestMatches,Quality,Solution):-
    addSpaces(BestMatches, ' ', MatchesSpaced),
    append('Solution: ', MatchesSpaced, SolPart),
    append(SolPart, '; Quality: ', NoQual),
    append(NoQual, Quality, Solution).

addSpaces([],_,[]).
addSpaces([Task|[]],_,[Task]).
addSpaces([Task|BestMatches],Char,MatchesSpaced):-
    addSpaces(BestMatches,Char,X),
    append([Char],X,Y),
    append([Task],Y,MatchesSpaced).

%%%%%%%%%%%%%%%% Labels %%%%%%%%%%%%%%%%%%

label('Name:').
label('forced partial assignment:').
label('forbidden machine:').
label('too-near tasks:').
label('machine penalties:').
label('too-near penalities').
% label(['Name:','forced partial assignment:','forbidden machine:','too-near tasks:','machine penalties:','too-near penalities']).

%Takes in list of lines from input, checks if the labels are all there
checkLabels([],_,_).
checkLabels([L|Labels], ListOfLines, OutputFile):-
    is_member(L,ListOfLines,OutputFile),
    checkLabels(Labels,ListOfLines,OutputFile).
    % printErrorAndClose(OutputFile,'Error while parsing input file').

%Recursively checks if X is a member of a list
is_member(_, [], OutputFile):-
    printErrorAndClose(OutputFile,'Error while parsing input file'),
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


% Split input into lists based on newline
splitlines(InputText,List):-
    atom_chars(InputText, Chars),
    new_split_string(Chars, [], Temp),
    convert_back(Temp, List).

% new_split_string(['\n'|Last],CurrentList,Final):-
%     append(CurrentList,[Last],Final).
    % CurrentList = [_|Final],
    % ListOfLines = Final.
% new_split_string(List, [], ListOfLines):-
%     new_split_string(NewList, [], ListOfLines).
new_split_string(List, CurrentList, ListOfLines):-
    check_newline(List,First,Rest) ->
    append(CurrentList,[First],NewCurr),
    new_split_string(Rest,NewCurr,ListOfLines);
    append(CurrentList,[List],ListOfLines).

check_newline(List,First,Rest):-
    append(First,['\n'|Rest],List).


convert_back([],_).
convert_back(List, FixedList):-
    List = [X|Rest],
    atom_chars(FixedFirst,X),
    convert_back(Rest, RestFixed),
    append([FixedFirst],RestFixed,FixedList).

%%%%%%%%%%%%%%%%%%%%% Parser %%%%%%%%%%%%%%%%%%%%%%

/*
 * parse_pairs(Lines, [[M,T]...[M,T]])
 * Ex: parse_pairs([['(', 1, ',', A, ')'], ['(', 2, ',', B, ')']], X).
 */
% parse_pairs([],_).
parse_pairs([''],[]).
parse_pairs([' '],[]).
parse_pairs(Lines, X) :-
    Lines = [],
    X = Lines. 
parse_pairs(Lines, X):-	
    [Head|Tail] = Lines,
    atom_chars(Head,Chars),
    parse_pair(Chars, Head_pair),
    parse_pairs(Tail, Y),
    X = [Head_pair|Y].

/** 
 * parse_pair(Line, [M,T])
 * obtains the pair as a tuple from a line
 * Ex: parse_pair(['(', 1, ',', A, ')'], X).
*/
parse_pair([],[]).
parse_pair(Line, Values) :-
    nth(2,Line,Val1),
    nth(4,Line,Val2),
    Values = [Val1,Val2].
    
    
/*
    * parse_triples(Lines, [[M,T,P]...[M,T,P]])
    * Ex: parse_triples([['(', 1, ',', A, ',', 1,2, ')'], ['(', 2, ',', B, ',', 3,4, ')']], X).
    */
% parse_triples([],_).
parse_triples([''],[]).
parse_triples([' '],[]).
parse_triples(Lines, X) :-
    Lines = [],
    X = Lines. 
parse_triples(Lines, X):-	
    [Head|Tail] = Lines,
    atom_chars(Head,Chars),
    parse_triple(Chars, Triple),
    parse_triples(Tail, Y),
    X = [Triple|Y].

/**
 * parse_triple(Line, [M,T,P])
 * Third element of a triple is always a penalty
 * Ex: parse_triple(['(', 1, ',', A, ',', 3, 4,')'], X).
 */
parse_triple([],[]).
parse_triple(Line, Values) :-
    nth(2, Line, Machine),
    nth(4, Line, Task),
    nth(6, Line, Penalty),
    % ['(', Machine, ',', Task, ',', Pen, ')'] = Line,
    % parse_penalty_triple(PEN, PenaltyList),
    % concatenate_num(PenaltyList, PenaltyAtom),
    % number_atom(Penalty, Pen),
    Values = [Machine, Task, Penalty].

    
/*
    * parse_penalty_grid(Lines, [[p1_1...p1_8]...[p8_1...p8_8]])
    * Ex : parse_penalty_grid([[8,8,8,',',7,7,7,',',6,6,',',5,5,',',4,4,',',3,',',2,',',1], [8,',',7,',',6,',',5,',',4,',',3,',',2,',',1]], X).
    */

parse_penalty_grid([''],[], OutputFile).
parse_penalty_grid([' '],[], OutputFile).
parse_penalty_grid(Lines, X, OutputFile) :-
    Lines = [],
    X = Lines. 
parse_penalty_grid(Lines, X, OutputFile):-	
    [Head|Tail] = Lines,
    atom_chars(Head,Row),
    \+ checkPeriod(Row,OutputFile),
    parseRow(Row,Values),
    parse_penalty_grid(Tail, Y, OutputFile),
    X = [Values|Y].

parseRow([],[]).
parseRow([R|Row],Values):-
    checkSpace(R) ->
    number_atom(Num,R),
    parseRow(Row,Y),
    Values = [Num|Y];
    parseRow(Row,Values).

checkPeriod(Row,OutputFile):-
    memberchk('.',Row),
    printErrorAndClose(OutputFile,'invalid penalty').

checkSpace(R):-
    char_code(SP,32),
    R \= SP.
    
    
/** parse_penalty_row
 * obtains a pentalty row as an 8-tuple (assumed to all be positive integers as per validation)
 * Ex: parse_penalty_row([8,8,8, ',', 7,7,7, ',', 6,6, ',', 5,5, ',', 4,4, ',', 3, ',', 2, ',', 1], X).
 */
% parse_penalty_row(_, RemainingPenalties, X) :-	/*base case; no req penalties remain*/
%     RemainingPenalties = 0, 
%     X = [].
% parse_penalty_row([Head|Tail], RemainingPenalties, X) :-	/*actual recursive call, THREE ARGS, called by standard*/
%     % grab_first_penalty(Line, Head),
%     % drop_first_penalty(Line, Tail),
%     NewRemainingPenalties is RemainingPenalties - 1,
%     parse_penalty_row(Tail, NewRemainingPenalties, Y),
%     concatenate_num(Head, Head_atom),
%     X = [Head_atom|Y].
% parse_penalty_row(Line, X) :-	/*standard call, TWO ARGS ONLY, defaults to 8 loops*/
%     parse_penalty_row(Line, 8, X).	
    
    
/**
 * takes a line of chars of form '<penalty>)    ' and returns penalty as an atom
 * Ex: parse_penalty_triple([2, 4, 8, ')', '\t', ' '], X).
 * >> returns 248 as an atom
 */
% parse_penalty_triple(PEN, X) :-	/*base rightbracket case*/
%     [H|_] = PEN,
%     char_code(RB, 41),
%     H = RB,
%     X = [].
% parse_penalty_triple(PEN, X) :-	/*recursive L->R gather*/
%     [H|Tail] = PEN,
%     parse_penalty_triple(Tail, Y),
%     X = [H|Y].


/**
 * 
 * Ex: concatenate_num([1,2,3,4,5,6],X).
 */	
% concatenate_num([List], X) :-
%     atom_length(List, 1),
%     nth(1, List, A),
%     number_atom(A, X), 
%     !.
% concatenate_num(List, X) :-	/*for when List ain't a list, it's a number*/
%     integer(List),
%     number_atom(List, X),
%     !.
% concatenate_num([A,B|List], X) :-
%     atom_length(List, 2),
%     nth(1, List, A), 
%     nth(2, List, B),
%     number_atom(A, A_atom), 
%     number_atom(B, B_atom),
%     atom_concat(A_atom, B_atom, X), 
%     !.
% concatenate_num(List, X) :-
%     [Head|Tail] = List,
%     number_atom(Head, H_atom),
%     concatenate_num(Tail, Y),
%     atom_concat(H_atom, Y, X).
        
% /**
%  * Gets the first penalty of the row **as a character list**
%  * Ex: grab_first_penalty([8,8,8, ',', 7, ',', 6, ',', 5, ',', 4, ',', 3, ',', 2, ',', 1], X).
%  */
% grab_first_penalty(List, X) :-	/*Base case, delimited by comma*/
%     [Head|_] = List,
%     char_code(COM, 32),
%     Head = COM, 
%     X = [].
% grab_first_penalty([List], X) :-	/*single element (ie end of line) base case*/
%     atom_length(List, 1), 
%     X = List.	
% grab_first_penalty(List, X):-	/*recursive case*/
%     [Head|Tail] = List,
%     grab_first_penalty(Tail, Y),
%     X = [Head|Y].

% /**
%  * drop_first_penalty
%  * obtains the inverse of the function grab_first_penalty, save for the ','
%  */
% drop_first_penalty(List, X) :-
%     List = [],
%     X = List.
% drop_first_penalty(List, X) :-	/*comma recursion interupt*/
%     [Head|Tail] = List,
%     char_code(COM, 44),
%     Head = COM,
%     X = Tail.
% drop_first_penalty(List, X) :-	/*newline recursion interupt*/
%     [Head|Tail] = List,
%     char_code(NL, 10),
%     Head = NL,
%     X = Tail.
% drop_first_penalty(List, X) :-	/*recursive case*/
%     [_|Tail] = List,
%     drop_first_penalty(Tail, X).    

%%%%%%%%%%%%%%%%%%%% Validate values %%%%%%%%%%%%%%%%%%%%%%%

% Validator rules
isMachine(Elem):- 
    member(Elem, ['1','2','3','4','5','6','7','8']).
isTask(Elem):- 
    member(Elem,['A','B','C','D','E','F','G','H']).
isPen(Num):-
    integer(Num),
    Num >= 0.

% Validate tuples (m,t)
validTupleMT([],_).
validTupleMT([[Mach,Task|_]|Tuples],OutputFile):-
    isMachine(Mach),
    isTask(Task) -> 
    validTupleMT(Tuples,OutputFile);
    printErrorAndClose(OutputFile,'invalid machine/task').

% Validate tuples (t,t)
validTupleTT([],_).
validTupleTT([[Task1,Task2|_]|Tuples],OutputFile):-
    isTask(Task1),
    isTask(Task2) ->
    validTupleTT(Tuples,OutputFile);
    printErrorAndClose(OutputFile,'invalid machine/task').

% Validate triples (t,t,p)
validTriple([],_).
validTriple([[]],_).
validTriple([[Task1,Task2,Pen|_]|Triples],OutputFile):-
    checkTask(Task1,Task2,OutputFile),
    checkPen(Pen,OutputFile),
    validTriple(Triples,OutputFile).

checkTask(Task1,Task2,OutputFile):-
    isTask(Task1),
    isTask(Task2) -> true ;
    printErrorAndClose(OutputFile,'invalid task').

checkPen(Pen,OutputFile):-
    number_atom(Num,Pen),
    isPen(Num) -> true ;
    printErrorAndClose(OutputFile,'invalid penalty').

% Validate grid
validGrid(Grid,OutputFile):-
    validGridSizeRow(Grid,OutputFile),
    validGridSizeCol(Grid,OutputFile),
    validGridValues(Grid,OutputFile).

validGridSizeRow(Grid,OutputFile):-
    length(Grid,8);
    printErrorAndClose(OutputFile,'machine penalty error').

validGridSizeCol([],_).
validGridSizeCol([Row|Grid],OutputFile):-
    length(Row,8),
    validGridSizeCol(Grid,OutputFile);
    printErrorAndClose(OutputFile,'machine penalty error').

validGridValues([],_).
validGridValues(Grid,OutputFile):-
    [Row|GTail] = Grid,
    validRow(Row) ->
    validGridValues(GTail,OutputFile);
    printErrorAndClose(OutputFile,'invalid penalty').

validRow([]).
validRow([Num|Row]):-
    isPen(Num),
    validRow(Row).

%%%%%%%%%%%%%%%%%%%% Hard constraints %%%%%%%%%%%%%%%%%%%%%%%

%Checks if all pair are valid. Return false if they are not. True if they are
forcedDouble([],_).
forcedDouble([[Mach,Task]|ForcedListRem], OutputFile) :- 
    checkTail(Mach,ForcedListRem), 
    checkTail(Task,ForcedListRem), 
    forcedDouble(ForcedListRem, OutputFile);
    printErrorAndClose(OutputFile,'partial assignment error').

checkTail(_, []).
checkTail(MachOrTask, [Head|Tail]) :- 
    \+ member(MachOrTask ,Head), 
    checkTail(MachOrTask,Tail).

% Check for repeating forced elements, makes invalid solution
% forcedDouble([[Mach,Task]|ListOfForced],OutputFileName):-
%     checkForcedTailMach(Mach,ListOfForced);
%     checkForcedTailTask(Task,ListOfForced);
%     forcedDouble(ListOfForced,OutputFileName).

% checkForcedTailMach(_,[]).
% checkForcedTailMach(Mach,[[Mach2,_]|ListOfForced]):-
%     Mach = Mach2;
%     checkForcedTailMach(Mach,ListOfForced).

% checkForcedTailTask(_,[]).
% checkForcedTailTask(Task,[[_,Task2]|ListOfForced]):-
%     Task = Task2;
%     checkForcedTailTask(Task,ListOfForced).

% Check for forced/forbidden conflicts, makes invalid solution
forcedConflicts([],_).
forcedConflicts([[Mach,Task]|ListOfForced],ListofForbid,OutputFileName):-
    checkForbidTail(Mach,Task,ListofForbid);
    forcedConflicts(ListOfForced,ListofForbid,OutputFileName).

checkForbidTail(_,_,[]).
checkForbidTail(Mach,Task,[[Mach2,Task2]|ListofForbid]):-
    Mach = Mach2, Task = Task2;
    checkForbidTail(Mach,Task,ListofForbid).


% Check for too-near invalid pairs
tooNear([],_,_,_).
tooNear([Matches|ListMTail],ListOfMatches,TooNear,ValidMatches,OutputFile):-
    % head(ListOfMatches,Matches),
    noTooNear(Matches,Matches,TooNear,OutputFile) ->
    tooNear(ListMTail,TooNear,ValidMatches,OutputFile) ;
    select(Matches,ListOfMatches,ValidMatches),
    tooNear(ListMTail,TooNear,ValidMatches,OutputFile).

% See if too-near violation in a set of matches
noTooNear(_,[],_,_).
noTooNear(Matches,[M|MTail],TooNear,OutputFile):-
    checkLeft(M,Matches,TooNear),
    checkRight(M,Matches,TooNear),
    noTooNear(Matches,MTail,TooNear,OutputFile);
    printErrorAndClose(OutputFile,'No valid solution possible!').


checkLeft(_,_,[]).
checkLeft(M,Matches,[[Left,Right]|TooNear]):-
    checkSame(M,Right) ->
    indexOf(Matches,M,MIndex),
    getLeftIndex(MIndex,Index),
    nth(Index,Matches,MLeft),
    checkDiff(MLeft,Left),
    checkLeft(M,Matches,TooNear) ;
    checkLeft(M,Matches,TooNear).

checkRight(_,_,[]).
checkRight(M,Matches,[[Left,Right]|TooNear]):-
    checkSame(M,Left) ->
    indexOf(Matches,M,MIndex),
    getRightIndex(MIndex,Index),
    nth(Index,Matches,MRight),
    checkDiff(MRight,Right),
    checkRight(M,Matches,TooNear) ;
    checkRight(M,Matches,TooNear).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createForcedMatches([],X,X).
createForcedMatches([[Mach,Task]|Forced],Temp,ForcedMatches):-
    number_atom(Index,Mach),
    replace_nth(Temp,Index,Task,Filled),
    createForcedMatches(Forced,Filled,ForcedMatches).

getRemainingTasks(['x'|_],X,X).
getRemainingTasks([FM|ForcedMatches],Temp,RemainingTasks):-
    memberchk(FM,Temp),
    select(FM,Temp,Remaining),
    getRemainingTasks(ForcedMatches,Remaining,RemainingTasks);
    getRemainingTasks(ForcedMatches,Temp,RemainingTasks).

fillMatches([],_,X,X,_).
fillMatches([T|Tasks],TooNear,ForcedMatches,FilledMatches,OutputFile):-
    nth(Index,ForcedMatches,'x'),
    replace_nth(ForcedMatches,Index,T,Filled),
    noTooNear(Filled,Filled,TooNear,OutputFile),
    fillMatches(Tasks,TooNear,Filled,FilledMatches,OutputFile);
    printErrorAndClose(OutputFile,'No valid solution possible!').

replace_nth(Matches,Index,Task,Filled):-
    NI is Index -1,
    length(Matches,A),
    length(Filled,B),
    A = B,
    append(Prefix, [_|Suffix], Matches),
    length(Prefix, NI),
    append(Prefix, [Task|Suffix], Filled).


%%%%%%%%%%%%%%%%%%%%% Soft Constraints, valid matches %%%%%%%%%%%%%%%%%%%%%%

value('1',1).
value('2',2).
value('3',3).
value('4',4).
value('5',5).
value('6',6).
value('7',7).
value('8',8).

% create all possible permutations of tasks
createAllMatches(Tasks, AllMatches):-
    permutation(Tasks, AllMatches).

% filter all permutations based on hard constraints
filterValidMatches(AllMatches,Forced,Forbid,TooNear,ValidMatches):-
    keepForcedMatches(AllMatches,AllMatches,Forced,ForcedMatches),
    dropForbidMatches(ForcedMatches,ForcedMatches,Forbid,ForcedForbidMatches),
    tooNear(ForcedForbidMatches,ForcedForbidMatches,TooNear,ValidMatches).

keepForcedMatches([],_,_,_).
keepForcedMatches([Matches|AllMTail],AllMatches,Forced,ValidMatches):-
    checkHardConstraint(Forced,Matches,Matches) ->
    keepForcedMatches(AllMTail,AllMatches,Forced,ValidMatches) ;
    select(Matches,AllMatches,ValidMatches),
    keepForcedMatches(AllMTail,AllMatches,Forced,ValidMatches).

dropForbidMatches([],_,_,_).
dropForbidMatches([Matches|AllMTail],AllMatches,Forbid,ValidMatches):-
    checkHardConstraint(Forbid,Matches,Matches) ->
    select(Matches,AllMatches,ValidMatches),
    dropForbidMatches(AllMTail,AllMatches,Forbid,ValidMatches);
    dropForbidMatches(AllMTail,AllMatches,Forbid,ValidMatches).

% returns true if (M,T) is in Matches
checkHardConstraint([],_,_).
checkHardConstraint([[MachF,TaskF]|Constraints],Matches,Matches):-
    isHardConstraint(MachF,TaskF,Matches,Matches),
    checkHardConstraint(Constraints,Matches,Matches).

% isHardConstraint(_,_,[],_).
isHardConstraint(MachF,TaskF,[Task|MTail],Matches):-
    indexOf(Matches,Task,MIndex),
    value(MachF,MachInt),
    MachInt = MIndex,
    TaskF = Task;
    isHardConstraint(MachF,TaskF,MTail,Matches).

%%%%%%%%%%%%%%%%%%% Soft Constraints, Penalties %%%%%%%%%%%%%%%%%%%%%%%

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
    checkLeft2(Task,Matches,TooNearPen,LeftPen),
    checkRight2(Task,Matches,TooNearPen,RightPen),
    tooNearTotal(MTail,Matches,TooNearPen,Sum),
    Total is LeftPen + RightPen + Sum.

checkLeft2(_,_,[],_).
checkLeft2(M,Matches,[[Left,Right,Pen]|TooNearPen],Total):-
    checkSame(M,Right) ->
    indexOf(Matches,M,MIndex),
    getLeftIndex(MIndex,Index),
    nth(Index,Matches,MLeft),
    checkDiff(MLeft,Left),
    checkLeft2(M,Matches,TooNearPen,Total) ;
    checkLeft2(M,Matches,TooNearPen,Sum),
    Total is Sum + Pen.

checkRight2(_,_,[],_).
checkRight2(M,Matches,[[Left,Right,Pen]|TooNearPen],Total):-
    checkSame(M,Left) ->
    indexOf(Matches,M,MIndex),
    getRightIndex(MIndex,Index),
    nth(Index,Matches,MRight),
    checkDiff(MRight,Right),
    checkRight2(M,Matches,TooNearPen,Total) ;
    checkRight2(M,Matches,TooNearPen,Sum),
    Total is Sum + Pen.

%%%%%%%%%%%%%%%%%% Other Predicates %%%%%%%%%%%%%%%%%%%%%%%%%

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
 
not(X):- X -> false ; true.
 
 
