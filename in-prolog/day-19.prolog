
day19.
input(File) :-
    source_file(day19, F),
    relative_file_name(File, F, '../input/day-19.input').

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
    split_string(S, "Blueprint : Each ore robot costs  ore. Each clay robot costs  ore. Each obsidian robot costs  ore and  clay. Each geode robot costs  ore and  obsidian.", "", S0),
    findall(N, (member(X,S0),
                number_string(N,X)), [A,B,C,D,E,F,G]),
    get-result(StreamIn, [A-B-C-(D,E)-(F,G)|Acc], Result).

% (A,B): spec.
% (C,D): observation
% E: built count
% (F,G): observation update
build_m((A,B), (C,D), E, (F,G)) :-
    min_list([C div A, D div B], E),
    E > 0, !,
    F is C - E * A,
    G is D - E * B.
build_m(A, B, C, D) :-
    number(A), number(B),
    C is B div A,
    C > 0, !,
    D is B - C * A.
build_m(_, X, 0, X).

% A: ore cost for ore
% B: ore cost for clay
% C: ore cost for obsidian
% D: clay cost for obsidian
% E: ore cost for geode
% F: obsidian cost for geode
% ~0: machine count
% ~2: mine count
execute(Minutes, A, B, (C,D), (E,F), Qty) :-
    execute(Minutes, A, B, (C,D), (E,F), (ore(1,0),clay(0,0),obsidian(0,0),geode(0,0)), Qty).

execute(Minutes, A, B, (C,D), (E,F), (ore(W0,W2),clay(X0,X2),obsidian(Y0,Y2),geode(Z0,Z2)), Qty) :-
    Minutes > 0, !,
    (
        build_m((E,F), (W2,Y2), D_z0, (W3,Y3)),
        build_m((C,D), (W3,X2), D_y0, (W4,X3)),
        build_m(B, W4, D_x0, W5),
        build_m(A, W5, D_w0, W6)
    ),
    M2 is Minutes - 1,
    W0_n is W0 + D_w0,
    W2_n is W0 + W6,
    X0_n is X0 + D_x0,
    X2_n is X0 + X3,
    Y0_n is Y0 + D_y0,
    Y2_n is Y0 + Y3,
    Z0_n is Z0 + D_z0,
    Z2_n is Z0 + Z2,
    trace,
    execute(M2, A, B, (C,D), (E,F), (ore(W0_n,W2_n),clay(X0_n,X2_n),obsidian(Y0_n,Y2_n),geode(Z0_n,Z2_n)), Qty).
execute(0, _, _, _, _, (ore(_,_),clay(_,_),obsidian(_,_),geode(_,Qty)), Qty).

solution(R) :-
    read-input(input, I),
    Minutes is 24,
    findall((Id*Qty), ( member(Id-A-B-(C,D)-(E,F), I),
                        execute(Minutes, A, B, (C,D), (E,F), Qty) ), R0),
    writeln(I),
    writeln(R0),
    sum_list(R0, R).
%% Then it's a greedy machine-building process such as:
% ?- solution(R).
%    Call: (19) execute(23, 4, 2,  (3, 14),  (2, 7),  (ore(1, 1), clay(0, 0), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (20) execute(22, 4, 2,  (3, 14),  (2, 7),  (ore(1, 2), clay(0, 0), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (21) execute(21, 4, 2,  (3, 14),  (2, 7),  (ore(1, 1), clay(1, 0), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (22) execute(20, 4, 2,  (3, 14),  (2, 7),  (ore(1, 2), clay(1, 1), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (23) execute(19, 4, 2,  (3, 14),  (2, 7),  (ore(1, 1), clay(2, 2), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (24) execute(18, 4, 2,  (3, 14),  (2, 7),  (ore(1, 2), clay(2, 4), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (25) execute(17, 4, 2,  (3, 14),  (2, 7),  (ore(1, 1), clay(3, 6), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (26) execute(16, 4, 2,  (3, 14),  (2, 7),  (ore(1, 2), clay(3, 9), obsidian(0, 0), geode(0, 0)), _7646) ? leap
%    Call: (27) execute(15, 4, 2,  (3, 14),  (2, 7),  (ore(1, 1), clay(4, 12), obsidian(0, 0), geode(0, 0)), _7646) ?
