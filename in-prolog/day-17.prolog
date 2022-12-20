
day17.
input(File) :-
    source_file(day17, F),
    relative_file_name(File, F, '../input/day-17.input').

read-input(StreamAlias, Result) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, Result),
    close(StreamAlias).

get-result(StreamIn, Result) :-
    read_line_to_string(StreamIn, S),
    findall(P, (string_concat(_, Ss, S),
                string_concat(P, _, Ss),
                string_length(P, 1)), Result).

field(Width, Field) :-
    findall([], between(1,Width,_), Field).

field_height(F, H) :-
    findall(N0, member([(N0,_)|_],F), L0),
    ( sort(L0, []), !,
      H = 0
    ; sort(L0, [H0|_]),
      H is -H0
    ).

%space(Width, Height, Space) :-
%    findall(S, (between(1,Width,_),
%                findall([], between(1,Height,_), S)), Space).

block_for(Field, Gap, Left, Shape_name, Block) :-
    member(Shape_name, [-,+,j,l,o]), !,
    field_height(Field, H),
    length(Field, W),
    H0 is - H - Gap,
    shape(H0, W, Left, Shape_name, Block).

shape(H_base, Width, Left, -, Shape) :-
    findall([], between(1,Left,_), S0),
    L2 is Left + 5,
    findall([], between(L2,Width,_), S2),
    H is H_base - 1,
    Block = [[(H,H)],[(H,H)],[(H,H)],[(H,H)]],
    append([S0,Block,S2], Shape).

shape(H_base, Width, Left, +, Shape) :-
    findall([], between(1,Left,_), S0),
    L2 is Left + 4,
    findall([], between(L2,Width,_), S2),
    H is H_base - 3,
    H0 is H + 1,
    H2 is H + 2,
    Block = [[(H0,H0)],[(H,H2)],[(H0,H0)]],
    append([S0,Block,S2], Shape).

shape(H_base, Width, Left, j, Shape) :-
    findall([], between(1,Left,_), S0),
    L2 is Left + 4,
    findall([], between(L2,Width,_), S2),
    H is H_base - 3,
    H0 is H + 2,
    Block = [[(H0,H0)],[(H0,H0)],[(H,H0)]],
    append([S0,Block,S2], Shape).

shape(H_base, Width, Left, l, Shape) :-
    findall([], between(1,Left,_), S0),
    L2 is Left + 2,
    findall([], between(L2,Width,_), S2),
    H is H_base - 4,
    H0 is H + 3,
    Block = [[(H,H0)]],
    append([S0,Block,S2], Shape).

shape(H_base, Width, Left, o, Shape) :-
    findall([], between(1,Left,_), S0),
    L2 is Left + 3,
    findall([], between(L2,Width,_), S2),
    H is H_base - 2,
    H0 is H + 1,
    Block = [[(H,H0)],[(H,H0)]],
    append([S0,Block,S2], Shape).

move_right(B, [[]|R]) :-
    append(R, [[]], B), !.
move_right(B, B).

move_left([[]|B], R) :-
    append(B, [[]], R), !.
move_left(B, B).

move_down(B, R) :-
    findall(L2, (member(L, B),
                 findall((X2,Y2), (member((X,Y),L),
                                   X2 is X + 1,
                                   Y2 is Y + 1), L2)), R).

merge_block(Block, Field, Field2) :-
    length(Block, W),
    length(Field, W), !,
    findall(L4, (between(1, W, I),
                 I0 is I - 1,
                 nth0(I0, Block, L0),
                 nth0(I0, Field, L2),
                 append(L0, L2, L4)), Field2).

not_overlapped(Block, Field) :-
    length(Block, W),
    length(Field, W), !,
    forall((between(1, W, I),
            nth0(I, Block, L0),
            nth0(I, Field, L2)),
           forall(member(X, L0),
                  \+ overlapped(X, L2))).

overlapped((_,0), _) :- !.
overlapped(X, L) :-
    is_list(L), !,
    member(Y, L),
    overlapped(X, Y), !.
overlapped((X,Y), (W,_)) :-
    between(X, Y, W), !.
overlapped((_,Y), (W,Z)) :-
    between(W, Z, Y).

solution(H) :-
    read-input(input, R),
    length(R, L),
    Width = 7,
    Gap = 3,
    Left = 2,
    I = 0,
    B = [-,+,j,l,o],
    J = 0,
    After = 2022,
    field(Width, Field),
    play_blocks(Field, Gap, Left, R, L, I, B, J, After, Field2),
    %write_field(Field2),
    field_height(Field2, H).

play_blocks(Field, _Gap, _Left, _Commands, _Com_len, _I, _Blocks, J, After, Field) :-
    J > After, !.
play_blocks(Field, Gap, Left, Commands, Com_len, I, Blocks, J, After, Field2) :-
    length(Blocks, L),
    J0 is J mod L,
    nth0(J0, Blocks, Shape_name),
    block_for(Field, Gap, Left, Shape_name, Block),
    play_block(Block, Field, Commands, Com_len, I, I0, Field0),
    J2 is J + 1,
    ( field_height(Field0, H0),
      write(after), write(:), write(J2), write(,), write(height), write(:), write(H0), write(,), write(Shape_name), nl ),
    play_blocks(Field0, Gap, Left, Commands, Com_len, I0, Blocks, J2, After, Field2).

play_block(Block, Field, Commands, Com_len, I, I2, Field2) :-
    %write_block(Block), nl,
    %write_field(Field),
    %trace,
    nth0(I, Commands, Command),
    I0 is (I+1) mod Com_len,
    %write(Command),
    ( ">" == Command,
      move_right(Block, B0)
    ; "<" == Command,
      move_left(Block, B0)
    ),
    ( not_overlapped(B0, Field), !,
      B4 = B0
    ; B4 = Block
    ),
    move_down(B4, B2),
    ( not_overlapped(B2, Field), !,
      play_block(B2, Field, Commands, Com_len, I0, I2, Field2)
    ; I2 = I0,
      merge_block(B4, Field, Field2)
    ).

write_block(Block) :-
    foreach(member(L,Block), writeln(L)).

write_field(Field) :-
    foreach(member(L,Field), writeln(L)).
