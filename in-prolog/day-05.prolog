
day05.
input(File) :-
    source_file(day05, F),
    relative_file_name(File, F, '../input/day-05.input').

read-input(StreamAlias, (Stacks,Commands)) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-stacks(StreamAlias, [], Stacks),
    get-commands(StreamAlias, [], Commands),
    close(StreamAlias).

get-stacks(StreamIn, Acc, Result) :-
    peek_char(StreamIn, '\n'), !,
    read_line_to_string(StreamIn, _),
    build-stacks(Acc, Result).
get-stacks(StreamIn, Acc, Result) :-
    read_line_to_string(StreamIn, S),
    get-stacks(StreamIn, [S|Acc], Result).

get-commands(StreamIn, Acc, Result) :-
    peek_char(StreamIn, end_of_file), !,
    reverse(Acc, Result).
get-commands(StreamIn, Acc, Result) :-
    read_line_to_string(StreamIn, S),
    get-commands(StreamIn, [S|Acc], Result).

build-stacks(Input, Stacks) :-
    findall(Ts,
            ( member(Line,Input),
              stacks(Line, Ts) ),
            Stacks).

% stack-count(Line, Count) :-
%     string_length(Line, N),
%     Count is round(N / 3 * 0.25).

stacks("", []) :- !.
stacks(S, [""|Stacks]) :-
    string_concat("   ", S2, S), !,
    after_gap(S2, S0),
    stacks(S0, Stacks).
stacks(S, [T|Stacks]) :-
    string_concat(X, S2, S),
    string_length(X, 3), !,
    term(X, T),
    after_gap(S2, S0),
    stacks(S0, Stacks).

after_gap("", "") :- !.
after_gap(S, R) :-
    string_concat(" ", R, S).

term(X, X).
    %trace,
    %write(X).

solution(R) :-
    read-input(input, R),
    true.

solution2(R) :-
    read-input(input, R),
    true.
