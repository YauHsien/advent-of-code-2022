
day12.
input(File) :-
    source_file(day12, F),
    relative_file_name(File, F, '../input/day-12.input').

read-input(StreamAlias, Result) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, (1,1), [], Result),
    close(StreamAlias).

get-result(StreamIn, (_,_), Acc, Result) :-
    peek_char(StreamIn, end_of_file), !,
    reverse(Acc, Result).
get-result(StreamIn, (_,Y), Acc, Result) :-
    peek_char(StreamIn, '\n'), !,
    get_char(StreamIn, _),
    Y2 is Y + 1,
    get-result(StreamIn, (1,Y2), Acc, Result).
get-result(StreamIn, (X,Y), Acc, Result) :-
    get_char(StreamIn, C),
    X2 is X + 1,
    group((X,Y)-C, Acc, Acc2),
    get-result(StreamIn, (X2,Y), Acc2, Result).

adjacent((X,Y), (X,Z)) :- abs(Y-Z) =:= 1.
adjacent((X,Y), (Z,Y)) :- abs(X-Z) =:= 1.

step(Coordinates-Symbol, C2-S2) :-
    adjacent(Coordinates, C2),
    step(Symbol, S2).
step('S', b).
step(y, 'E').
step(A, B) :-
    atom(A),
    atom(B),
    atomic_list_concat([A,B], Ab),
    atom_concat(_, R, 'SabcdefghijklmnopqrstuvwxyzE'),
    atom_prefix(R, Ab).

group(Coordinates-Symbol, Group) :-
    member(C2-Symbol, Group),
    adjacent(Coordinates, C2).

group(Coordinates-Symbol, Groups, R) :-
    findall(G, (member(G, Groups),
                group(Coordinates-Symbol, G)), Gs0),
    flatten(Gs0, G2),
    sort([Coordinates-Symbol|G2], Group0),
    subtract(Groups, Gs0, Groups2),
    R = [Group0|Groups2].

id(Groups, R) :-
    length(Groups, N),
    findall(I: G, (between(1, N, I),
                   I0 is I - 1,
                   nth0(I0, Groups, G)
                  ), R).

climbable(Id_groups, R) :-
    findall(I-J, (member(I:X, Id_groups),
                  findall(J, (member(J:Y, Id_groups),
                              I =\= J,
                              (member(A, X),
                                    member(B, Y),
                                    step(A, B))), Js0),
                  sort(Js0, Js),
                  member(J, Js)), R).

group_route(Climbables, I, J, [I|Route]) :-
    I =\= J,
    member(I-J0, Climbables),
    group_route(Climbables, J0, J, Route).
group_route(Climbables, I, I, [I]) :-
    member(_-I, Climbables).

group_route(Climbables, I, [I|Route]) :-
    member(I-J, Climbables),
    group_route(Climbables, J, Route).
group_route(Climbables, I, [I]) :-
    \+ member(I-_, Climbables),
    member(_-I, Climbables).

% By now, 2022/12/20, I cannot find any route from 'S' to 'E'.
% ?- read-input(input,R), id(R,R2), climbable(R2,Is), writeln(Is), foreach(group_route(Is,20,Route), writeln(Route))
% [3-51,7-17,8-51,11-24,13-7,14-30,16-27,17-33,18-51,19-11,20-18,20-50,20-51,21-51,23-22,24-38,25-13,27-26,27-34,28-29,29-23,30-43,33-42,34-28,35-16,36-19,37-35,38-37,40-25,41-36,43-47,44-51,46-40,47-46,48-51,50-51,51-52,51-54,54-14]
% [20,18,51,52]
% [20,18,51,54,14,30,43,47,46,40,25,13,7,17,33,42]
% [20,50,51,52]
% [20,50,51,54,14,30,43,47,46,40,25,13,7,17,33,42]
% [20,51,52]
% [20,51,54,14,30,43,47,46,40,25,13,7,17,33,42]
% ?- read-input(input,R), id(R,R2), findall(I,member(I:[(_,_)-o|_],R2),L), writeln(L), length(L,N).
% [19,42]

scheme(Scheme) :-
    Term = 'SabcdefghijklmnopqrstuvwxyzE',
    findall(A0, ( atom_concat(_, A, Term),
                 atom_prefix(A, A0),
                 atom_length(A0, 1) ), Scheme).

start(X-Y-'S', Map) :-
    member(X-Y-'S', Map).

goal(X-Y-'E', Map) :-
    member(X-Y-'E', Map).

:- table lower_letter/1 as incremental.
:- table climbable/2 as incremental.

lower_letter(A) :-
    char_code(a, C0),
    char_code(z, C2),
    char_code(A, C),
    between(C0, C2, C).


step(X-Y-E, Map, Route, X-Y2-E2) :-
    member(X-Y-E, Map),
    member(X-Y2-E2, Map),
    abs(Y-Y2) =:= 1,
    climbable(E, E2),
    \+ member(X-Y2-E2, Route).
step(X-Y-E, Map, Route, X2-Y-E2) :-
    member(X-Y-E, Map),
    member(X2-Y-E2, Map),
    abs(X-X2) =:= 1,
    climbable(E, E2),
    \+ member(X2-Y-E2, Route).

steps(A, B, C) :-
    findall((X-Y-A0,W-Z-B0), ( member(X-Y-A0, A),
                               member(W-Z-B0, B),
                               step(X-Y-A0, W-Z-B0) ), C0),
    sort(C0, C).

steps([_], []).
steps([A,B|L], [C|R]) :-
    steps(A, B, C),
    steps([B|L], R).

candidate(A, Map, B, Path) :-
    candidate(A, Map, B, [A], Path).

candidate(A, _Map, B, Acc, Path) :-
    step(A, B), !,
    %member(A, Acc),
    %\+ member(B, _Map),
    reverse([B|Acc], Path).
candidate(A, Map, B, Acc, Path) :-
    %member(A, Acc),
    %( \+ member(A, Map)
    %; true ),
    %\+ member(B, Map),
    member(X, Map),
    step(A, X),
    \+ member(X, Acc),
    candidate(X, Map, B, [X|Acc], Path).

path(Steps, Maps, Path) :-
    path(Steps, Maps, [], Path).

path([], [_], Acc, Path) :-
    reverse(Acc, Path).
path([X,Y|L], [_,Z|R], Acc, Path) :-
    member((A,B), X),
    member((C,D), Y),
    candidate(B,Z,C,P),
    P \= [], !,
    Path0 = [A|P],
    ( writeln(P),
      writeln(Path0),
      trace),
    path([[(C,D)]|L], [Z|R], [Path0|Acc], Path).
%path(A, B, _) :-
%    writeln(A), nl,
%    writeln(B).

%:- dynamic set/2.

solution(Map) :-
    read-input(input, Map),
    scheme(Scheme),
    %abolish(set/2),
    findall(L, ( member(C, Scheme),
                 findall(X-Y-C, member(X-Y-C,Map), L) ), Ss),
    ( % Find step pairs:
        [S,_,B|_] = Ss,
        append(_, [Y,_,E], Ss),
        steps([S,B], [Steps0]),
        steps(Ss, Steps),
        steps([Y,E], [Steps2]),
        [H|Steps3] = Steps,
        append(Steps4, [T], Steps3),
        append(Steps0, H, H2),
        append(Steps2, [T], T2),
        [H2|Steps4] = Steps5,
        append(Steps5, T2, Steps6) ),
    %writeln(Steps6), nl,
    %writeln(Ss),
    path(Steps6, Ss, Path),
    writeln(Path),
    trace,
    true.
    %start(S, Map),
    %goal(G, Map),
    %all_shortest_paths(S, G, Map, P),
    %length(P, N).

solution2(_).
