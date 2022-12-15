
day15.
input(File) :-
    source_file(day15, F),
    relative_file_name(File, F, '../input/day-15.input').

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
    Symbols = "Sensor at x=, y=: closest beacon is at x=, y=",
    split_string(S, Symbols, Symbols, [A0,B0,C0,D0]),
    findall(N, (member(X, [A0,B0,C0,D0]),
                number_string(N, X)), [A,B,C,D]),
    get-result(StreamIn, [(A-B,C-D)|Acc], Result).

range_x(L, M, Mx) :-
    findall([X-abs(X-W)-abs(Y-Z), W+abs(X-W)+abs(Y-Z)],
            member((X-Y,W-Z), L),
            L0),
    append(L0, L2),
    min_list(L2, M),
    max_list(L2, Mx).

range_y(L, M, Mx) :-
    findall([Y-abs(X-W)-abs(Y-Z), Z+abs(X-W)+abs(Y-Z)],
            member((X-Y,W-Z), L),
            L0),
    append(L0, L2),
    min_list(L2, M),
    max_list(L2, Mx).

coverage((X-Y,W-Z), Ym, (X0,X2)) :-
    R is abs(X-W) + abs(Y-Z),
    Dy is abs(Y-Ym),
    Dx is R - Dy,
    ( Dx > 0,
      X0 is X - Dx,
      X2 is X + Dx
    ; Dx =< 0,
      X0 = 1,
      X2 = -1
    ).

solution(Y, N) :-
    read-input(input, I),
    range_x(I, Mx, Mxx),
    %range_y(I, My, Mxy),
    findall(R, (member(X,I),
                coverage(X,Y,R)), Ranges),
    ( findall(X, (X3 is Mx - 25_168_531, %% "What!"
                  X4 is Mxx + 25_168_531, %% "Why?"
                  between(X3,X4,X),
                  %( % not in range
                  %    forall( member((X0,X2),Ranges),
                  %            \+ between(X0, X2, X) ) ),
                  ( % in range and no beacon
                      once((member((X0,X2),Ranges),
                            between(X0,X2,X))),
                      \+ member((X-Y,_-_),I),
                      \+ member((_-_,X-Y),I) ),
                  true),
              R0),
      sort(R0, R),
      length(R, N) ).
%% (By query `?- solution(2_000_000, N).`)
%% Your puzzle answer was `N`.

solution2(_).
