
day04.
input(File) :-
    source_file(day04, F),
    relative_file_name(File, F, '../input/day-04.input').

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
    split_string(S, "-,", "", R),
    findall(N, (member(X,R), number_string(N,X)), [A,B,C,D]),
    get-result(StreamIn, [A-B,C-D|Acc], Result).

fullcon([], []).
fullcon([A-B,C-D|L], [(A-B,C-D)|R]) :-
    between(A,B,C), between(A,B,D), !,
    fullcon(L, R).
fullcon([A-B,C-D|L], [(A-B,C-D)|R]) :-
    between(C,D,A), between(C,D,B), !,
    fullcon(L, R).
fullcon([_,_|L], R) :-
    fullcon(L, R).

solution(N) :-
    read-input(input, I),
    fullcon(I, R),
    length(R, N).

overlap([], []).
overlap([A-B,C-D|L], [(A-B,C-D)|R]) :-
    ( between(A, B, C), !
    ; between(A, B, D), !
    ; between(C, D, A), !
    ; between(C, D, B)
    ),
    overlap(L, R).
overlap([_,_|L], R) :-
    overlap(L, R).

solution2(N) :-
    read-input(input, I),
    overlap(I, R),
    length(R, N).
