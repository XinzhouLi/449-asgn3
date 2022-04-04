% Import other modules (aka other prolog files)
:- use_module(input_output,[readLines/3,createSolution/3,printErrorAndClose/2]).
:- use_module(labels,[checkLabels/2,get_forced_partial/2,get_forbidden_machine/2,get_toonear_tasks/2,get_machine_penalties/2,get_toonear_penalties/2,split_input_to_list/2]).
:- use_module(parser,[parse_pairs/2,parse_triples/2,parse_penalty_grid/2]).
:- use_module(validator,[validTupleMT/2, validTupleTT/2, validTriple/2, validGrid/2]).
:- use_module(hardconstraints,[checkValidForced/2,checkForcedForbid/3,tooNear/4]).
:- use_module(softconstraints1,[createAllMatches/2,filterValidMatches/5]).
:- use_module(softconstraints2,[getBestMatches/6]).


% Main functor
main:-
    % Get console argument values
    current_prolog_flag(argv, [_,InputFileName,OutputFileName|_]),

    % Open file and get lines of text
    open(InputFileName,read,Str), 
    readLines(Str,LinesOfFile,OutputFileName),
    close(Str), 

    split_input_to_list(LinesOfFile,ListOfLines),

    % Check file text for comments or label errors (Scott)
    checkLabels(ListOfLines,OutputFileName),

    get_forced_partial(ListOfLines, ForcedPartial),
    get_forbidden_machine(ListOfLines, ForbiddenMachine),
    get_toonear_tasks(ListOfLines, TooNearTasks),
    get_machine_penalties(ListOfLines, MachinePenalties),
    get_toonear_penalties(ListOfLines, TooNearPenalties),

    % Parse lines of text to get values (Oliver)
    parse_pairs(ForcedPartial,Forced),
    parse_pairs(ForbiddenMachine,Forbid),
    parse_pairs(TooNearTasks,TooNear),
    parse_penalty_grid(MachinePenalties,Grid),
    parse_triples(TooNearPenalties,TooNearPen),

    % Check parsed values for errors (Khalid)
    validTupleMT(Forced,OutputFileName),
    validTupleMT(Forbid,OutputFileName),
    validTupleTT(TooNear,OutputFileName),
    validGrid(Grid,OutputFileName),
    validTriple(TooNearPen,OutputFileName),

    % Check hard constraints (Fungai, Jacky)
    checkValidForced(Forced,OutputFileName),
    checkForcedForbid(Forced,Forbid,OutputFileName),

    % Check soft constraints
    createAllMatches(["A","B","C","D","E","F","G","H"],AllMatches),
    filterValidMatches(AllMatches,Forced,Forbid,TooNear,ValidMatches),
    getBestMatches(ValidMatches,ValidMatches,Grid,TooNearPen,Total,BestMatches),

    % Close file and write output
    createSolution(BestMatches,Total,Solution),
    write(Solution,OutputFileName),  nl.
