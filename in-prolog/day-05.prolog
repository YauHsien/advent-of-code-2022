%% --- Day 5: Supply Stacks ---
%% The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.
%%
%% The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.
%%
%% The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
%%
%% They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:
%%
%%     [D]
%% [N] [C]
%% [Z] [M] [P]
%%  1   2   3
%%
%% move 1 from 2 to 1
%% move 3 from 1 to 3
%% move 2 from 2 to 1
%% move 1 from 1 to 2
%% In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.
%%
%% Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:
%%
%% [D]
%% [N] [C]
%% [Z] [M] [P]
%%  1   2   3
%% In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:
%%
%%         [Z]
%%         [N]
%%     [C] [D]
%%     [M] [P]
%%  1   2   3
%% Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:
%%
%%         [Z]
%%         [N]
%% [M]     [D]
%% [C]     [P]
%%  1   2   3
%% Finally, one crate is moved from stack 1 to stack 2:
%%
%%         [Z]
%%         [N]
%%         [D]
%% [C] [M] [P]
%%  1   2   3
%% The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.
%%
%% After the rearrangement procedure completes, what crate ends up on top of each stack?
%%
%% Your puzzle answer was SVFDLGLWV.
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

operate_stacks(Stacks, [], Stacks).
operate_stacks(Stacks0, [C|Commands], Stacks) :-
    operate_stack(Stacks0, C, Stacks2),
    operate_stacks(Stacks2, Commands, Stacks).

operate_stack(Stacks0, N-From/To, Stacks) :-
    findall(Lf, member(From-Lf,Stacks0), [Lf]),
    findall(Lt, member(To-Lt,Stacks0), [Lt]),
    findall(X-L, (member(X-L,Stacks0),X=\=From,X=\=To), L),
    move(N, Lf, Lt, Lf2, Lt2),
    Stacks = [From-Lf2,To-Lt2|L].

move(N, From0, To0, From, To) :-
    append(Part, From, From0),
    length(Part, N), !,
    reverse(Part, Part2),
    append(Part2, To0, To).

solution(Top) :-
    read-input(input, (Stacks,Commands)),
    operate_stacks(Stacks, Commands, Stacks0),
    keysort(Stacks0, Stacks2),
    findall( A,
             ( member(_-[H|_],Stacks2),
               split_string(H, "[]", [], [_,X,_]),
               string_chars(X, [A])
             ),
             Top0 ),
    string_chars(Top, Top0), !.
%% (By query `?- solution(Top)`)
%% Your puzzle answer was `Top`.

%% --- Part Two ---
%% As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.
%%
%% Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
%%
%% The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup holder, and the ability to pick up and move multiple crates at once.
%%
%% Again considering the example above, the crates begin in the same configuration:
%%
%%     [D]
%% [N] [C]
%% [Z] [M] [P]
%%  1   2   3
%% Moving a single crate from stack 2 to stack 1 behaves the same as before:
%%
%% [D]
%% [N] [C]
%% [Z] [M] [P]
%%  1   2   3
%% However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates stay in the same order, resulting in this new configuration:
%%
%%         [D]
%%         [N]
%%     [C] [Z]
%%     [M] [P]
%%  1   2   3
%% Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:
%%
%%         [D]
%%         [N]
%% [C]     [Z]
%% [M]     [P]
%%  1   2   3
%% Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate C that gets moved:
%%
%%         [D]
%%         [N]
%%         [Z]
%% [M] [C] [P]
%%  1   2   3
%% In this example, the CrateMover 9001 has put the crates in a totally different order: MCD.
%%
%% Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be ready to unload the final supplies. After the rearrangement procedure completes, what crate ends up on top of each stack?
operate_stacks2(Stacks, [], Stacks).
operate_stacks2(Stacks0, [C|Commands], Stacks) :-
    operate_stack2(Stacks0, C, Stacks2),
    operate_stacks2(Stacks2, Commands, Stacks).

operate_stack2(Stacks0, N-From/To, Stacks) :-
    findall(Lf, member(From-Lf,Stacks0), [Lf]),
    findall(Lt, member(To-Lt,Stacks0), [Lt]),
    findall(X-L, (member(X-L,Stacks0),X=\=From,X=\=To), L),
    move2(N, Lf, Lt, Lf2, Lt2),
    Stacks = [From-Lf2,To-Lt2|L].

move2(N, From0, To0, From, To) :-
    append(Part, From, From0),
    length(Part, N), !,
    append(Part, To0, To).

solution2(Top) :-
    read-input(input, (Stacks,Commands)),
    operate_stacks2(Stacks, Commands, Stacks0),
    keysort(Stacks0, Stacks2),
    findall( A,
             ( member(_-[H|_],Stacks2),
               split_string(H, "[]", [], [_,X,_]),
               string_chars(X, [A])
             ),
             Top0 ),
    string_chars(Top, Top0), !.
%% (By query `?- solution2(Top2)`)
%% Your puzzle answer was `Top2`.

