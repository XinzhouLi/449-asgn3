% the file that contains the main algorithm by applying the cluse stored in format_check
:- dynamic(sol/2).

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
	cal_too_near_pen(Task3, Task4, TP3),
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

