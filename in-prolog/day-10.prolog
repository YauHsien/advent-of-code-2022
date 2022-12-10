
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
loop(Nth, ["noop"|Input], Reg, Collection, Acc, Result) :-
    Nth2 is Nth + 1,
    ( member(Nth2, Collection), !,
      Acc2 = [Nth2*Reg|Acc]
    ; Acc2 = Acc
    ),
    loop(Nth2, Input, Reg, Collection, Acc2, Result).
loop(Nth, ["addx"-N|Input], Reg, Collection, Acc, Result) :-
    Nth0 is Nth + 1,
    Nth2 is Nth + 2,
    Reg2 is Reg + N,
    ( member(Nth0, Collection), !,
      Acc2 = [Nth0*Reg|Acc]
    ; member(Nth2, Collection), !,
      Acc2 = [Nth2*Reg|Acc]
    ; Acc2 = Acc
    ),
    loop(Nth2, Input, Reg2, Collection, Acc2, Result).

solution(S) :-
    read-input(input, I),
    loop(0, I, 1, [20, 60, 100, 140, 180, 220], [], R),
    writeln(R),
    sum_list(R, S).

solution2(_).
