
day13.
input(File) :-
    source_file(day13, F),
    relative_file_name(File, F, '../input/day-13.input').

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
    get-result(StreamIn, [S|Acc], Result).

solution(I) :-
    read-input(input, I),
    true.

solution2(_).
