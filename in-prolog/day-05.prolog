
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
    split_string(S, "move from to", "", S2),
    term(S2, S3),
    get-commands(StreamIn, [S3|Acc], Result).

build-stacks(Input, Stacks) :-
    findall(Ts,
            ( member(Line,Input),
              layers(Line, Ts) ),
            [Labels|Layers]),
    stacks(Labels, Layers, Stacks).

stacks(Labels, Layers, Stacks) :-
    findall( X-[],
             ( member(S,Labels),
               term_string(X, S)
             ),
             Stacks0),
    build_stacks(Stacks0, Layers, Stacks).

build_stacks(Stacks, [], Stacks).
build_stacks(Acc, [Layers0|Layers], Stacks) :-
    push_stacks(Acc, Layers0, Stacks0),
    build_stacks(Stacks0, Layers, Stacks).

push_stacks([], [], []).
push_stacks([B-L|Stacks0], [""|Layers], [B-L|Stacks]) :- !,
    push_stacks(Stacks0, Layers, Stacks).
push_stacks([B-L|Stacks0], [Y|Layers], [B-[Y|L]|Stacks]) :-
    push_stacks(Stacks0, Layers, Stacks).

layers("", []) :- !.
layers(S, [""|Stacks]) :-
    string_concat("   ", S2, S), !,
    after_gap(S2, S0),
    layers(S0, Stacks).
layers(S, [X|Stacks]) :-
    string_concat(X, S2, S),
    string_length(X, 3), !,
    after_gap(S2, S0),
    layers(S0, Stacks).


after_gap("", "") :- !.
after_gap(S, R) :-
    string_concat(" ", R, S).

term([_,_,_,_,_,N0,_,_,_,_,_,From0,_,_,_,To0], N-From/To) :-
    number_string(N, N0),
    number_string(From, From0),
    number_string(To, To0).

solution((R,S)) :-
    read-input(input, (R,S)),
    foreach(member(X-Y,R), writeln(X-Y)),
    true.
%% Print:
%  1 -[[N],[Q],[L],[S],[C],[Z],[P],[T]]
%  2 -[[G],[C],[H],[V],[T],[P],[L]]
%  3 -[[F],[Z],[C],[D]]
%  4 -[[C],[V],[M],[L],[D],[T],[W],[G]]
%  5 -[[C],[W],[P]]
%  6 -[[Z],[S],[T],[C],[D],[J],[F],[P]]
%  7 -[[D],[B],[G],[W],[V]]
%  8 -[[W],[H],[Q],[S],[J],[N]]
%  9 -[[V],[L],[S],[F],[Q],[C],[R]]

solution2(R) :-
    read-input(input, R),
    true.
