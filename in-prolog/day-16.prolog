
day16.
input(File) :-
    source_file(day16, F),
    relative_file_name(File, F, '../input/day-16.input').

read-input(StreamAlias, Result) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, [], Result),
    close(StreamAlias).

get-result(StreamIn, Acc, Result) :-
    peek_char(StreamIn, end_of_file), !,
    reverse(Acc, Result).
get-result(StreamIn, Acc, Result) :-
    read_line_to_string(StreamIn, S),
    split_string(S, "alve  has flow rate=; tunnels lead to valves ,",
                 "Valve ", I0),
    findall(R, (member(X, I0),
                X \= "",
                (number_string(N, X),
                 R = N
                ;\+ number_string(_, X),
                 R = X)), [A,N|R]),
    format("~w~n",[R]),
    assertz( valve(A,N) ),
    foreach( member(X,R),
             assertz( tunnel(A,X) ) ),
    get-result(StreamIn, [[A,N|R]|Acc], Result).

:- dynamic valve/2, tunnel/2.

clear_database :-
    abolish(valve/2),
    abolish(tunnel/2).

% Result: Path = [ Element 1,
%                  Element 2 (:) Score 2,
%                  Element 3 (:) Score 3, . . . ]
steps(F, T, Release, Path) :-
    findall(I0:N0:S0:L0, ( step(F,T,S0:L0),
                        length(L0,N0),
                        I0 is S0 / N0 ), L),
    sort(L, R2),
    reverse(R2, [_:_:Release:Path|_]).

step(F, F, _) :- !,
    fail.
step(F, T, [F,T]) :-
    tunnel(F, T), !.
step(F, T, R) :-
    step(F, [], T, R).

step(T, Acc, T, S:R) :- !,
    reverse([T|Acc], [H0|R0]),
    findall(X:S0, ( member(X,R0),
                    valve(X,S0)), R2),
    findall(S0, member(_:S0,R2), S2),
    sum_list(S2, S),
    R = [H0|R2].
step(F, Acc, T, R) :-
    tunnel(F, G),
    \+ member(G, [F|Acc]),
    step(G, [F|Acc], T, R).


process(P, M, R) :-
    process(P, M, 0, [], R).

process(_P, M, M, Acc, R) :- !,
    sum_list(Acc, R).
process(P, M, N, Acc, R) :-
    M > N, !,
    ( [L|_] = P,
      tunnel(L, L2),
      L == 0 % incomplete. . .
    ),
    N2 is N + 1,
    process(P2, M, N2, Acc2, R).

solution(I) :-
    Minutes = 30,
    Path = ["AA"],
    clear_database,
    read-input(input, I),
    true.
%% Then,
% ?- foreach((valve(X,_),valve(Y,_),X\=Y,steps(X,Y,R,P)),
%            format("~w ~~> ~w: ~d ~p~n",[X,Y,R,P])).
% AA ~> BB: 35 ["AA","DD":20,"CC":2,"BB":13]
% AA ~> CC: 22 ["AA","DD":20,"CC":2]
% AA ~> DD: 20 ["AA","DD":20]
% AA ~> EE: 23 ["AA","DD":20,"EE":3]
% AA ~> FF: 38 ["AA","BB":13,"CC":2,"DD":20,"EE":3,"FF":0]
% AA ~> GG: 38 ["AA","BB":13,"CC":2,"DD":20,"EE":3,"FF":0,"GG":0]
% AA ~> HH: 60 ["AA","BB":13,"CC":2,"DD":20,"EE":3,"FF":0,"GG":0,"HH":22]
% AA ~> II: 0 ["AA","II":0]

%% And now my understanding is that
%% when trying to go through the maze, on each step, I can go by
%% either passing a node by `tunnel/2` or then opening its valve
%% by `valve/2`.

solution2(_).
