:- use_module(input2).
:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(library(lists)).



first(tuple(X, _), X).
first(empty, inf).
second(tuple(_, Y), Y).
second(empty, 1).

solved_rows([]).
solved_rows([Row|Rows]) :-
    maplist(second, Row, Numbers),
    sort(Numbers, Numbers),
    solved_rows(Rows).

solved_columns([]).
solved_columns([Col|Cols]) :-
    maplist(first, Col, Numbers),
    sort(Numbers, Numbers),
    solved_columns(Cols).

solved_tower(Tower) :-
    Rows = Tower,
    solved_rows(Rows),
    transpose(Rows, Cols),
    solved_columns(Cols).

legal_tower(Tower) :-
    [Row|Rows] = Tower,
    maplist(same_length(Row), Rows),
    transpose(Rows, Cols),
    transpose(Rows, Cols).

tower_cols(Tower, Cols) :-
    Tower = [FirstRow|_],
    length(FirstRow, Cols).

tower_rows(Tower, Rows) :-
    length(Tower, Rows).

% ---------------------- TOWER RULES ----------------------

rotate(List, 0, List).
rotate([H|T], N, Rotated) :-
    N #> 0,
    N1 #= N-1,
    append(T, [H], NewList),
    rotate(NewList, N1, Rotated).

rotate(List, N, Rotated) :-
    N #< 0,
    length(List, Len),
    N1 #= N mod Len,
    rotate(List, N1, Rotated).

replace_nth([_|T], 1, X, [X|T]).
replace_nth([H|T], I, X, [H|R]) :-
    I #> 1,
    I1 #= I-1,
    replace_nth(T, I1, X, R).

% finds index of sublist, that contains empty
empty_coord(Tower, RowIndex, ColIndex) :-
    nth1(RowIndex, Tower, Row),
    nth1(ColIndex, Row, empty).

insert_at(Elem, 1, List, [Elem|List]).
insert_at(Elem, Index, [H|TOld], [H|TNew]) :-
        Index #> 1,
        Index0 #= Index - 1,
        insert_at(Elem, Index0, TOld, TNew).

% moves empty in list
move_empty(Row, Index, NewRow) :-
    select(empty, Row, RowWithoutEmpty),
    insert_at(empty, Index, RowWithoutEmpty, NewRow).

% Rotation
% rotation can be uniquely defined using two parameters, in this case X and Y
% X specifies, which row will get rotated
% Y specifies the extent of rotation
move(Old, New) :-
    tower_rows(Old, NumRows),
    tower_cols(Old, NumCols),
    X in 1..NumRows,
    % if Y == NumCols, we woluld get the original tower
    Ymax #= NumCols-1,
    Y in 1..Ymax,
    nth1(X, Old, ChosenRow),
    rotate(ChosenRow, Y, RotatedRow),
    replace_nth(Old, X, RotatedRow, New).

% move UP/DOWN
% This move has only one varible, which determines extent of the movement.
% Only one column can move, since there is only one empty space in the tower.
move(Old, New) :-
    empty_coord(Old, RI, CI),
    transpose(Old, T),
    nth1(CI, T, ChosenCol),
    X #\= RI, % We would get the original tower
    move_empty(ChosenCol, X, ShiftedCol),
    replace_nth(T, CI, ShiftedCol, TNew),
    transpose(TNew, New).

% ---------------- SEARCH ALGORITHM ------------------

bfs(Start, Path) :-
    bfs_queue([[Start]], [], Path).

% in case the solution is found, bfs returns
bfs_queue([[Node|NPath]|_], _, Path) :-
    solved_tower(Node),
    reverse([Node|NPath], Path).

% Solution not found, generate more nodes
bfs_queue([[Node|NPath]|Rest], Visited, Path) :-
    findall(NewMove, (move(Node, NewMove), \+ member(NewMove, Visited)), NewMoves),
    append(Visited, NewMoves, NewVisited),
    maplist(list([Node|NPath]), NewMoves, NewMovesWithPaths),
    append(Rest, NewMovesWithPaths, Queue),
    bfs_queue(Queue, NewVisited, Path).

list(T, H, [H|T]).


% --------------------- IO -------------------------

letter_to_number(C, N) :-
    N #= Code - Acode + 1,
    char_code('A', Acode),
    char_code('Z', Zcode),
    char_code(C, Code),
    Code >= Acode,
    Code =< Zcode.

chars_to_tuple(['*', '*'], empty).

chars_to_tuple([Letter|Digits], tuple(Row, Col)) :-
    letter_to_number(Letter, Col),
    number_chars(Row, Digits).

inrow_to_towerrow(InRow, TowerRow) :-
    maplist(chars_to_tuple, InRow, TowerRow).

read_tower(Tower) :-
    read_lines(Lines),
    split_lines(Lines, Matrix),
    maplist(inrow_to_towerrow, Matrix, Tower).

print_tower(Tower) :-
    maplist(inrow_to_towerrow, Matrix, Tower),
    split_lines(Lines, Matrix),
    maplist(atom_chars, StringLines, Lines),
    maplist(writeln, StringLines),
    nl.


% ------------------ MAIN ---------------------------

start :-
    read_tower(Initial),
    legal_tower(Initial),
    bfs(Initial, Path),
    maplist(print_tower, Path),
    halt.

start :-
    writeln("This tower is illegal or has no solution."),
    halt.

% ----------------- Testing predicates ------------------

% [[empty, tuple(1,2)], [tuple(1,1), tuple(2,2)]]

read_and_print :-
    read_tower(Tower),
    print_tower(Tower),
    halt.

read_and_write :-
    read_tower(Tower),
    writeln(Tower),
    halt.

list_moves :-
    read_tower(Initial),
    findall(NewMove, move(Initial, NewMove), NewMoves),
    maplist(print_tower, NewMoves),
    halt.

solve_ex_d3 :-
    Tower = [[tuple(1,1),tuple(1,2),tuple(1,3),tuple(1,4),tuple(1,5),tuple(1,6)],[tuple(2,3),tuple(2,4),tuple(2,5),tuple(2,6),empty,tuple(2,2)],[tuple(2,1),tuple(3,2),tuple(3,3),tuple(3,4),tuple(3,5),tuple(3,6)],[tuple(3,1),tuple(4,2),tuple(4,3),tuple(4,4),tuple(4,5),tuple(4,6)]],
    bfs(Tower, _).



