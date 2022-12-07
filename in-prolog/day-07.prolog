
day07.
:- dynamic dir/1.
:- dynamic path_size/2.

input(File) :-
    source_file(day07, F),
    relative_file_name(File, F, '../input/day-07.input').

read-input(StreamAlias) :-
    abolish(dir/1),
    abolish(path_size/2),
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, []),
    close(StreamAlias).

get-result(StreamIn, _Acc) :-
    peek_char(StreamIn, end_of_file), !.
get-result(StreamIn, Acc) :-
    read_line_to_string(StreamIn, S),
    split_string(S, " ", "", C),
    go_with(Acc, C, Acc2),
    get-result(StreamIn, Acc2).

go_with(Path_r, ["$","ls"], Path_r).
go_with(Path_r, ["dir",Folder], Path_r) :-
    assertz(dir(Folder)).
go_with([_|Path_r], ["$","cd",".."], Path_r) :- !.
go_with(Path_r, ["$","cd",Folder], [Folder|Path_r]).
go_with(Path_r, [Size,File], Path_r) :-
    number_string(N, Size),
    !,
    reverse([File|Path_r], Path),
    assertz(path_size(Path,N)).

total_size(Folder, Size) :-
    findall( X,
             ( path_size(Path, X),
               member(Folder, Path)
             ),
             Sizes),
    sum_list(Sizes, Size).

solution(S) :-
    Limit = 100000,
    read-input(input),
    findall( X,
             ( dir(Folder),
               total_size(Folder, X),
               X =< Limit
             ),
             Sizes),
    sum_list(Sizes, S).

solution2(_).
