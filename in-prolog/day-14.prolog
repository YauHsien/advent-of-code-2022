
day14.
input(File) :-
    source_file(day14, F),
    relative_file_name(File, F, '../input/day-14.input').
output(File) :-
    source_file(day14, F),
    relative_file_name(File, F, 'day-14.output').

read-input(StreamAlias, Result) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, [], Result),
    close(StreamAlias).

get-result(StreamIn, Acc, (X0-X2,Y0-Y2,Vs,Hs)) :-
    peek_char(StreamIn, end_of_file), !,
    sort(Acc, L0),
    ( findall([X,Z], member((X-_,Z-_),L0), Xs0),
      append(Xs0, Xs2),
      sort(Xs2, [X0|Xs]),
      append(_, [X2], Xs) ),
    ( findall([Y,Z], member((_-Y,_-Z),L0), Ys0),
      append(Ys0, Ys2),
      sort(Ys2, [Y0|Ys]),
      append(_, [Y2], Ys) ),
    findall((X-Y,X-Z), member((X-Y,X-Z),L0), Vs),
    findall((X-Y,Z-Y), member((X-Y,Z-Y),L0), Hs).
get-result(StreamIn, Acc, Result) :-
    read_line_to_string(StreamIn, S),
    split_string(S, " ", "", S0),
    findall(A-B, ( member(T, S0),
                 T \= "->",
                 split_string(T, ",", "", [A0,B0]),
                 number_string(A, A0),
                 number_string(B, B0) ), L0),
    pairs(L0, L),
    append(Acc, L, Acc2),
    get-result(StreamIn, Acc2, Result).

pairs([_], []).
pairs([A,B|L], [(A2,B2)|R]) :-
    sort([A,B], [A2,B2]),
    pairs([B|L], R).

% My understanding:
% Sand stack is limited by
% 1. Free platform, out of walls;
% 2. Lower wall, when length of a wall is shorter than of a platform;
% 3. Shorter platform, whe length of a wall is longer than of a platform.

dots(WoPs, Dots) :-
    dots(WoPs, [], Dots).

dots([], Dots, Dots).
dots([H|L], Acc, Dots) :-
    findall(X0, ( member((X-Y,X-Z), H),
                  between(Y, Z, D),
                  X0 = X-D
                ; member((X-Y,Z-Y), H),
                  between(Y, Z, D),
                  X0 = D-Y
                ), R),
    append(Acc, R, Acc2),
    dots(L, Acc2, Dots).

show_map(I-J, X-W, _-Z, Dots) :-
    foreach( between(J, Z, Y0),
             ( foreach( between(X, W, X0),
                        ( X0-Y0 = I-J,
                          write(x)
                        ; X0-Y0 \= I-J,
                          member(X0-Y0, Dots),
                          write(#)
                        ; X0-Y0 \= I-J,
                          \+ member(X0-Y0, Dots),
                          write('.')
                        ) )
             , nl
             ) ).

write_map(OutAlias, I-J, X-W, _-Z, Dots) :-
    foreach( between(J, Z, Y0),
             ( foreach( between(X, W, X0),
                        ( X0-Y0 = I-J,
                          put_char(OutAlias, x)
                        ; X0-Y0 \= I-J,
                          member(X0-Y0, Dots),
                          put_char(OutAlias, #)
                        ; X0-Y0 \= I-J,
                          \+ member(X0-Y0, Dots),
                          put_char(OutAlias, '.')
                        ) )
               , put_char(OutAlias, '\n')
             ) ).

solution(X0-X2, Y0-Y2, Walls, Platforms) :-
    Gen = 500-0,
    read-input(input, (X0-X2,Y0-Y2,Walls,Platforms)),
    dots([Walls,Platforms], Dots),
    ( output(FileName),
      OutAlias = output,
      open(FileName, write, _Fd, [alias(OutAlias)]),
      write_map(OutAlias, Gen, X0-X2, Y0-Y2, Dots),
      close(OutAlias) ),
    true.

solution2(_).
