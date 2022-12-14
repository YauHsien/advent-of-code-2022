%% --- Day 14: Regolith Reservoir ---
%% The distress signal leads you to a giant waterfall! Actually, hang on - the signal seems like it's coming from the waterfall itself, and that doesn't make any sense. However, you do notice a little path that leads behind the waterfall.
%%
%% Correction: the distress signal leads you behind a giant waterfall! There seems to be a large cave system here, and the signal definitely leads further inside.
%%
%% As you begin to make your way deeper underground, you feel the ground rumble for a moment. Sand begins pouring into the cave! If you don't quickly figure out where the sand is going, you could quickly become trapped!
%%
%% Fortunately, your familiarity with analyzing the path of falling material will come in handy here. You scan a two-dimensional vertical slice of the cave above you (your puzzle input) and discover that it is mostly air with structures made of rock.
%%
%% Your scan traces the path of each solid rock structure and reports the x,y coordinates that form the shape of the path, where x represents distance to the right and y represents distance down. Each path appears as a single line of text in your scan. After the first point of each path, each point indicates the end of a straight horizontal or vertical line to be drawn from the previous point. For example:
%%
%% 498,4 -> 498,6 -> 496,6
%% 503,4 -> 502,4 -> 502,9 -> 494,9
%% This scan means that there are two paths of rock; the first path consists of two straight lines, and the second path consists of three straight lines. (Specifically, the first path consists of a line of rock from 498,4 through 498,6 and another line of rock from 498,6 through 496,6.)
%%
%% The sand is pouring into the cave from point 500,0.
%%
%% Drawing rock as #, air as ., and the source of the sand as +, this becomes:
%%
%%
%%   4     5  5
%%   9     0  0
%%   4     0  3
%% 0 ......+...
%% 1 ..........
%% 2 ..........
%% 3 ..........
%% 4 ....#...##
%% 5 ....#...#.
%% 6 ..###...#.
%% 7 ........#.
%% 8 ........#.
%% 9 #########.
%% Sand is produced one unit at a time, and the next unit of sand is not produced until the previous unit of sand comes to rest. A unit of sand is large enough to fill one tile of air in your scan.
%%
%% A unit of sand always falls down one step if possible. If the tile immediately below is blocked (by rock or sand), the unit of sand attempts to instead move diagonally one step down and to the left. If that tile is blocked, the unit of sand attempts to instead move diagonally one step down and to the right. Sand keeps moving as long as it is able to do so, at each step trying to move down, then down-left, then down-right. If all three possible destinations are blocked, the unit of sand comes to rest and no longer moves, at which point the next unit of sand is created back at the source.
%%
%% So, drawing sand that has come to rest as o, the first unit of sand simply falls straight down and then stops:
%%
%% ......+...
%% ..........
%% ..........
%% ..........
%% ....#...##
%% ....#...#.
%% ..###...#.
%% ........#.
%% ......o.#.
%% #########.
%% The second unit of sand then falls straight down, lands on the first one, and then comes to rest to its left:
%%
%% ......+...
%% ..........
%% ..........
%% ..........
%% ....#...##
%% ....#...#.
%% ..###...#.
%% ........#.
%% .....oo.#.
%% #########.
%% After a total of five units of sand have come to rest, they form this pattern:
%%
%% ......+...
%% ..........
%% ..........
%% ..........
%% ....#...##
%% ....#...#.
%% ..###...#.
%% ......o.#.
%% ....oooo#.
%% #########.
%% After a total of 22 units of sand:
%%
%% ......+...
%% ..........
%% ......o...
%% .....ooo..
%% ....#ooo##
%% ....#ooo#.
%% ..###ooo#.
%% ....oooo#.
%% ...ooooo#.
%% #########.
%% Finally, only two more units of sand can possibly come to rest:
%%
%% ......+...
%% ..........
%% ......o...
%% .....ooo..
%% ....#ooo##
%% ...o#ooo#.
%% ..###ooo#.
%% ....oooo#.
%% .o.ooooo#.
%% #########.
%% Once all 24 units of sand shown above have come to rest, all further sand flows out the bottom, falling into the endless void. Just for fun, the path any new sand takes before falling forever is shown here with ~:
%%
%% .......+...
%% .......~...
%% ......~o...
%% .....~ooo..
%% ....~#ooo##
%% ...~o#ooo#.
%% ..~###ooo#.
%% ..~..oooo#.
%% .~o.ooooo#.
%% ~#########.
%% ~..........
%% ~..........
%% ~..........
%% Using your scan, simulate the falling sand. How many units of sand come to rest before sand starts flowing into the abyss below?
day14.
input(File) :-
    source_file(day14, F),
    relative_file_name(File, F, '../input/day-14.input').
output(File) :-
    source_file(day14, F),
    relative_file_name(File, F, 'day-14.output').

:- dynamic rock/2, void/2, pour/2, sand/2, meet_void/1.
:- abolish(pour/2), assertz(pour(500, 0)).
:- abolish(meet_void/1), assertz(meet_void(false)).

clear_database :-
    abolish(rock/2),
    abolish(void/2),
    abolish(sand/2),
    retractall(meet_void(true)).

read-input(StreamAlias, Result) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, [], Result),
    close(StreamAlias).

get-result(StreamIn, Acc, (X0-X2,Y0-Y2,Vs,Hs)) :-
    peek_char(StreamIn, end_of_file), !,
    sort(Acc, L0),
    ( findall([X,Z], member((X-_,Z-_),L0), Xs0),
      append(Xs0, Xs2),
      sort(Xs2, [X0|Xs]),
      append(_, [X2], Xs) ),
    ( findall([Y,Z], member((_-Y,_-Z),L0), Ys0),
      append(Ys0, Ys2),
      sort(Ys2, [Y0|Ys]),
      append(_, [Y2], Ys) ),
    findall((X-Y,X-Z), member((X-Y,X-Z),L0), Vs),
    findall((X-Y,Z-Y), member((X-Y,Z-Y),L0), Hs).
get-result(StreamIn, Acc, Result) :-
    read_line_to_string(StreamIn, S),
    split_string(S, " ", "", S0),
    findall(A-B, ( member(T, S0),
                   T \= "->",
                   split_string(T, ",", "", [A0,B0]),
                   number_string(A, A0),
                   number_string(B, B0) ), L0),
    pairs(L0, L),
    append(Acc, L, Acc2),
    get-result(StreamIn, Acc2, Result).

pairs([_], []).
pairs([A,B|L], [(A2,B2)|R]) :-
    sort([A,B], [A2,B2]),
    mark_rock(A2, B2),
    pairs([B|L], R).

mark_rock(X-Y, X-Z) :- !,
    foreach( between(Y, Z, Y0),
             assertz(rock(X, Y0)) ).
mark_rock(X-Y, Z-Y) :-
    foreach( between(X, Z, X0),
             assertz(rock(X0, Y)) ).

% My understanding:
% Sand stack is limited by
% 1. Free platform, out of walls;
% 2. Lower wall, when length of a wall is shorter than of a platform;
% 3. Shorter platform, whe length of a wall is longer than of a platform.

write_map(OutAlias, X0-X2, Y2) :-
    A is X0,
    B is X2,
    C is Y2,
    pour(I, J),
    foreach( between(J, C, Y),
             ( foreach( between(A, B, X),
                        ( X-Y = I-J,
                          put_char(OutAlias, +)
                        ; X-Y \= I-J,
                          sand(X, Y),
                          put_char(OutAlias, o)
                        ; X-Y \= I-J,
                          rock(X, Y),
                          put_char(OutAlias, #)
                        ; X-Y \= I-J,
                          void(X, Y),
                          put_char(OutAlias, v)
                        ; X-Y \= I-J,
                          air(X, Y),
                          put_char(OutAlias, '.')
                        ) )
               , put_char(OutAlias, '\n')
             ) ).

put_void(X, Y2-Y0) :-
    Y2 > Y0,
    put_void(X, Y2-Y0, []).

put_void(_, Y2-Y0, Acc) :-
    Y2 < Y0, !,
    first_void(Acc).
put_void(_, Y2-Y0, []) :-
    Y2 < Y0, !.
put_void(X, Y2-_, []) :-
    rock(X, Y2), !.
put_void(X, Y2-Y0, Acc) :-
    rock(X, Y2), !,
    Y is Y2 - 1,
    meet(Acc, rock(X,Y2), Acc2),
    put_void(X, Y-Y0, Acc2).
put_void(X, Y2-Y0, Acc) :-
    Y is Y2 - 1,
    ( X0 is X - 1,
      rock(X0, Y2), !,
      meet(Acc, void(X,Y2), Acc2),
      put_void(X, Y-Y0, Acc2)
    ; X2 is X + 1,
      rock(X2, Y2), !,
      meet(Acc, void(X,Y2), Acc2),
      put_void(X, Y-Y0, Acc2)
    ; put_void(X, Y-Y0, Acc)
    ).

first_void([void(X,Y)|_]) :- !,
    assertz(void(X, Y)).
first_void([_|L]) :-
    first_void(L).

meet(Acc, rock(X,Y), [rock(X,Y)|Acc]).
meet([rock(X,Y)|Acc], void(_,_), [rock(X,Y)|Acc]) :- !.
meet(Acc, void(X,Y), [void(X,Y)|Acc]).

put_void(X0, X2, Y0, Y2) :-
    A is X0,
    B is X2,
    C is Y0,
    D is Y2,
    foreach( between(A, B, X),
             put_void(X, D-C) ).

air(X, Y) :-
    \+ rock(X, Y),
    \+ sand(X, Y).

meet_void :-
    void(X, Y),
    sand(X, Y).

pour(Ylimit) :-
    pour(X, Y),
    pour1(Ylimit, sand(X, Y)).

pour1(Y, sand(_, Y)) :- !,
    assert(meet_void(true)).
pour1(_, sand(X, Y)) :-
    void(X, Y), !,
    assert(meet_void(true)).
pour1(Ylimit, sand(X, Y)) :-
    Y2 is Y + 1,
    air(X, Y2), !,
    pour1(Ylimit, sand(X, Y2)).
pour1(Ylimit, sand(X, Y)) :-
    X0 is X - 1,
    Y2 is Y + 1,
    %air(X0, Y),
    air(X0, Y2), !,
    pour1(Ylimit, sand(X0, Y2)).
pour1(Ylimit, sand(X, Y)) :-
    X2 is X + 1,
    Y2 is Y + 1,
    %air(X2, Y),
    air(X2, Y2), !,
    pour1(Ylimit, sand(X2, Y2)).
pour1(_, sand(X, Y)) :-
    assertz(sand(X, Y)).

keep_pour(Ylimit) :-
    repeat,
    ( meet_void(true)
    ; \+ meet_void(true),
      pour(Ylimit),
      fail
    ).

solution(X0-X2, Y0-Y2, Walls, Platforms, Rest) :-
    clear_database,
    read-input(input, (X0-X2,Y0-Y2,Walls,Platforms)),
    put_void(X0-1, X2+1, Y0, Y2),
    assertz(sand(0, 0)),
    keep_pour(Y2),
    ( output(FileName),
      OutAlias = output,
      open(FileName, write, _Fd, [alias(OutAlias)]),
      write_map(OutAlias, (X0-1)-(X2+1), Y2),
      close(OutAlias) ),
    ( findall(_, ( sand(X, Y)
                   %\+ void(X, Y)
                 ), L0),
      sort(L0, L),
      length(L, Rest) ).
%% (By query `?- solution(X0-X2, Y0-Y2, Vs, Hs, Rest)`
%% then consult file "day-14.output" and count up letter `o`,)
%% Your puzzle answer was `___`.

solution2(_).
