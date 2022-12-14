
day14.
input(File) :-
    source_file(day14, F),
    relative_file_name(File, F, '../input/day-14.input').
output(File) :-
    source_file(day14, F),
    relative_file_name(File, F, 'day-14.output').

:- dynamic rock/2, void/2, pour/2, sand/2.
:- assertz(pour(500, 0)).

clear_database :-
    abolish(rock/2),
    abolish(void/2),
    abolish(sand/2).

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

pour1(Y, sand(X, Y)) :- !,
    ( sand(X, Y), !
    ; assertz(void(X, Y)),
      assertz(sand(X, Y))
    ).
pour1(_, sand(X, Y)) :-
    void(X, Y), !,
    assertz(sand(X, Y)).
pour1(Ylimit, sand(X, Y)) :-
    Y2 is Y + 1,
    air(X, Y2), !,
    pour1(Ylimit, sand(X, Y2)).
pour1(Ylimit, sand(X, Y)) :-
    X0 is X - 1,
    Y2 is Y + 1,
    air(X0, Y),
    air(X0, Y2), !,
    pour1(Ylimit, sand(X0, Y2)).
pour1(Ylimit, sand(X, Y)) :-
    X2 is X + 1,
    Y2 is Y + 1,
    air(X2, Y),
    air(X2, Y2), !,
    pour1(Ylimit, sand(X2, Y2)).
pour1(_, sand(X, Y)) :-
    assertz(sand(X, Y)).

keep_pour(Ylimit) :-
    repeat,
    ( meet_void
    ; \+ meet_void,
      sand(_, Ylimit)
    ; \+ meet_void,
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
    ( findall(_, ( sand(X, Y),
                   \+ void(X, Y)
                 ), L0),
      sort(L0, L),
      length(L, Rest) ),
    true.

solution2(_).
