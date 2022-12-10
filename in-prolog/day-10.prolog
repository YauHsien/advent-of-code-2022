
day10.
input(File) :-
    source_file(day10, F),
    relative_file_name(File, F, '../input/day-10.input').

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
    ( S == "noop", !,
      get-result(StreamIn, [S|Acc], Result)
    ; split_string(S, " ", "", [C,N0]),
      number_string(N, N0),
      get-result(StreamIn, [C-N|Acc], Result)
    ).

loop(_, [], _, _, Result, Result).
loop(Nth0, ["noop"|Input], Reg, Goal, Acc, Result) :-
    Nth is Nth0 + 1,
    cycle(Nth, Reg, Acc, Goal, Acc2),
    loop(Nth, Input, Reg, Goal, Acc2, Result).
loop(Nth0, ["addx"-N|Input], Reg, Goal, Acc, Result) :-
    Nth is Nth0 + 1,
    Nth2 is Nth0 + 2,
    cycle(Nth, Reg, Acc, Goal, Acc0),
    cycle(Nth2, Reg, Acc0, Goal, Acc2),
    Reg2 is Reg + N,
    loop(Nth2, Input, Reg2, Goal, Acc2, Result).

cycle(Nth, Reg, Acc, Goal, Result) :-
    apply(Goal, [Nth, Reg, Acc, Result]).

filter(Nth, Reg, Acc, [Nth*Reg|Acc]) :-
    member(Nth, [20,60,100,140,180,220]), !.
filter(_, _, Result, Result).

solution(S) :-
    read-input(input, I),
    loop(0, I, 1, filter, [], R),
    writeln(R),
    sum_list(R, S).

solution2(_).
