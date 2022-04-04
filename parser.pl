% Define module
:- module(parser,[parse_pairs/2,parse_triples/2,parse_penalty_grid/2]).

/*
 * parse_pairs(Lines, [[M,T]...[M,T]])
 * Ex: parse_pairs([['(', 1, ',', A, ')'], ['(', 2, ',', B, ')']], X).
 */
parse_pairs(Lines, X) :-
	Lines = [],
	X = Lines. 
parse_pairs(Lines, X):-	
	[Head|Tail] = Lines,
	parse_pair(Head, Head_pair),
	parse_pairs(Tail, Y),
	X = [Head_pair|Y].

	
/*
 * parse_triples(Lines, [[M,T,P]...[M,T,P]])
 * Ex: parse_triples([['(', 1, ',', A, ',', 1,2, ')'], ['(', 2, ',', B, ',', 3,4, ')']], X).
 */
parse_triples(Lines, X) :-
	Lines = [],
	X = Lines. 
parse_triples(Lines, X):-	
	[Head|Tail] = Lines,
	parse_triple(Head, Head_pair),
	parse_triples(Tail, Y),
	X = [Head_pair|Y].

	
/*
 * parse_penalty_grid(Lines, [[p1_1...p1_8]...[p8_1...p8_8]])
 * Ex : parse_penalty_grid([[8,8,8,',',7,7,7,',',6,6,',',5,5,',',4,4,',',3,',',2,',',1], [8,',',7,',',6,',',5,',',4,',',3,',',2,',',1]], X).
 */
parse_penalty_grid(Lines, X) :-
	Lines = [],
	X = Lines. 
parse_penalty_grid(Lines, X):-	
	[Head|Tail] = Lines,
	parse_penalty_row(Head, Head_pair),
	parse_penalty_grid(Tail, Y),
	X = [Head_pair|Y].


/** 
 * parse_pair(Line, [M,T])
 * obtains the pair as a tuple from a line
 * Ex: parse_pair(['(', 1, ',', A, ')'], X).
*/
parse_pair(Line, X) :-
	nth1(2, Line, Machine),
	nth1(4, Line, Task),
	X = [Machine, Task].

	
/**
 * parse_triple(Line, [M,T,P])
 * Third element of a triple is always a penalty
 * Ex: parse_triple(['(', 1, ',', A, ',', 3, 4,')'], X).
 */
parse_triple(Line, X) :-
	nth1(2, Line, Machine),
	nth1(4, Line, Task),
	['(', Machine, ',', Task, ','|PEN] = Line,
	parse_penalty_triple(PEN, PenaltyList),
	concatenate_num(PenaltyList, PenaltyAtom),
	number_atom(Penalty, PenaltyAtom),
	X = [Machine, Task, Penalty].
	
	
/** parse_penalty_row
 * obtains a pentalty row as an 8-tuple (assumed to all be positive integers as per validation)
 * Ex: parse_penalty_row([8,8,8, ',', 7,7,7, ',', 6,6, ',', 5,5, ',', 4,4, ',', 3, ',', 2, ',', 1], X).
 */
parse_penalty_row(_, RemainingPenalties, X) :-	/*base case; no req penalties remain*/
	RemainingPenalties = 0, 
	X = [].
parse_penalty_row(Line, RemainingPenalties, X) :-	/*actual recursive call, THREE ARGS, called by standard*/
	grab_first_penalty(Line, Head),
	drop_first_penalty(Line, Tail),
	NewRemainingPenalties is RemainingPenalties - 1,
	parse_penalty_row(Tail, NewRemainingPenalties, Y),
	concatenate_num(Head, Head_atom),
	X = [Head_atom|Y].
parse_penalty_row(Line, X) :-	/*standard call, TWO ARGS ONLY, defaults to 8 loops*/
		parse_penalty_row(Line, 8, X).	
	
	
/**
 * takes a line of chars of form "<penalty>)    " and returns penalty as an atom
 * Ex: parse_penalty_triple([2, 4, 8, ')', '\t', ' '], X).
 * >> returns 248 as an atom
 */
parse_penalty_triple(PEN, X) :-	/*base rightbracket case*/
	[H|_] = PEN,
	char_code(RB, 41),
	H = RB,
	X = [].
parse_penalty_triple(PEN, X) :-	/*recursive L->R gather*/
	[H|Tail] = PEN,
	parse_penalty_triple(Tail, Y),
	X = [H|Y].


/**
 * 
 * Ex: concatenate_num([1,2,3,4,5,6],X).
 */	
concatenate_num(List, X) :-
	length(List, 1),
	nth1(1, List, A),
	number_atom(A, X), 
	!.
concatenate_num(List, X) :-	/*for when List ain't a list, it's a number*/
	integer(List),
	number_atom(List, X),
	!.
concatenate_num(List, X) :-
	length(List, 2),
	nth1(1, List, A), 
	nth1(2, List, B),
	number_atom(A, A_atom), 
	number_atom(B, B_atom),
	atom_concat(A_atom, B_atom, X), 
	!.
concatenate_num(List, X) :-
	[Head|Tail] = List,
	number_atom(Head, H_atom),
	concatenate_num(Tail, Y),
	atom_concat(H_atom, Y, X).
 	
/**
 * Gets the first penalty of the row **as a character list**
 * Ex: grab_first_penalty([8,8,8, ',', 7, ',', 6, ',', 5, ',', 4, ',', 3, ',', 2, ',', 1], X).
 */
grab_first_penalty(List, X) :-	/*Base case, delimited by comma*/
	[Head|_] = List,
	char_code(COM, 44),
	Head = COM, 
	X = [].
grab_first_penalty(List, X) :-	/*single element (ie end of line) base case*/
	length(List, 1), 
	X = List.	
grab_first_penalty(List, X):-	/*recursive case*/
	[Head|Tail] = List,
	grab_first_penalty(Tail, Y),
	X = [Head|Y].

/**
 * drop_first_penalty
 * obtains the inverse of the function grab_first_penalty, save for the ','
 */
drop_first_penalty(List, X) :-
	List = [],
	X = List.
drop_first_penalty(List, X) :-	/*comma recursion interupt*/
	[Head|Tail] = List,
	char_code(COM, 44),
	Head = COM,
	X = Tail.
drop_first_penalty(List, X) :-	/*newline recursion interupt*/
	[Head|Tail] = List,
	char_code(NL, 10),
	Head = NL,
	X = Tail.
drop_first_penalty(List, X) :-	/*recursive case*/
	[_|Tail] = List,
	drop_first_penalty(Tail, X).
 
 
 
 
 
 
 