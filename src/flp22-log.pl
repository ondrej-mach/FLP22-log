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

swapRight([X], [X]).
swapRight([empty, Tuple | Row], [Tuple, empty | Row]).
swapRight([X,Y|T], [X,Yout|Tout]) :- swapRight([Y|T], [Yout|Tout]).

swapLeft([X], [X]).
swapLeft([Tuple, empty | Row], [empty, Tuple | Row]).
swapLeft([X,Y|T], [X,Yout|Tout]) :- swapLeft([Y|T], [Yout|Tout]).

moveUp(Old, New) :-
    transpose(Old, T),
    moveLeft(T, Tnew),
    transpose(Tnew, New).

moveDown(Old, New) :-
    transpose(Old, T),
    moveRight(T, Tnew),
    transpose(Tnew, New).

moveLeft(Old, New) :- maplist(swapLeft, Old, New).
moveRight(Old, New) :- maplist(swapRight, Old, New).

move(Old, New) :- moveUp(Old, New).
move(Old, New) :- moveDown(Old, New).
move(Old, New) :- moveLeft(Old, New).
move(Old, New) :- moveRight(Old, New).

bfs(Start, Path) :-
    bfs_queue([[Start]], [], Path).

% in case the solution is found, bfs returns
bfs_queue([[Node|NPath]|_], _, Path) :-
    solved_tower(Node),
    reverse([Node|NPath], Path).

% Solution not found, generate more nodes
bfs_queue([[Node|NPath]|Rest], Visited, Path) :-
    findall(NewMove, (move(Node, NewMove), \+ member(NewMove, Visited), NewMove \= Node), NewMoves),
    append(Visited, NewMoves, NewVisited),
    maplist(list([Node|NPath]), NewMoves, NewMovesWithPaths),
    append(Rest, NewMovesWithPaths, Queue),
    bfs_queue(Queue, NewVisited, Path).

list(T, H, [H|T]).

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
    maplist(string_chars, StringLines, Lines),
    maplist(writeln, StringLines),
    nl.


start :-
    read_tower(Initial),
    legal_tower(Initial),
    bfs(Initial, Path),
    maplist(print_tower, Path),
    halt.

% ----------------- Testing predicates ------------------

read_and_print :-
    read_tower(Tower),
    print_tower(Tower),
    halt.




