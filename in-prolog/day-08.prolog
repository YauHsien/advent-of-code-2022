%% --- Day 8: Treetop Tree House ---
%% The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a tree house.
%% 
%% First, determine whether there is enough tree cover here to keep a tree house hidden. To do this, you need to count the number of trees that are visible from outside the grid when looking directly along a row or column.
%%
%% The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input). For example:
%%
%% 30373
%% 25512
%% 65332
%% 33549
%% 35390
%% Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.
%%
%% A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.
%%
%% All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the interior nine trees to consider:
%%
%% The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
%% The top-middle 5 is visible from the top and right.
%% The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
%% The left-middle 5 is visible, but only from the right.
%% The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
%% The right-middle 3 is visible from the right.
%% In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
%% With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.
%%
%% Consider your map; how many trees are visible from outside the grid?
day08.
input(File) :-
    source_file(day08, F),
    relative_file_name(File, F, '../input/day-08.input').

read-input(StreamAlias, Result) :-
    input(FileName),
    open(FileName, read, _Fd, [alias(StreamAlias)]),
    get-result(StreamAlias, 1-1, [], Result),
    close(StreamAlias).

get-result(StreamIn, _, Result, Result) :-
    peek_char(StreamIn, end_of_file), !.
get-result(StreamIn, _-Y, Acc, Result) :-
    peek_char(StreamIn, '\n'), !,
    get_char(StreamIn, _),
    Y2 is Y + 1,
    X2 is 1,
    get-result(StreamIn, X2-Y2, Acc, Result).
get-result(StreamIn, X-Y, Acc, Result) :-
    get_code(StreamIn, C),
    X2 is X + 1,
    get-result(StreamIn, X2-Y, [C-X-Y|Acc], Result).

area([_-W-L|_], W, L).

visible(Map, X, Y) :-
    member(C-X-Y, Map),
    ( forall( (member(A-X-Y0, Map), Y0 < Y), C > A ), !
    ; forall( (member(B-X0-Y, Map), X0 < X), C > B ), !
    ; forall( (member(D-X2-Y, Map), X < X2), C > D ), !
    ; forall( (member(E-X-Y2, Map), Y < Y2), C > E ), !
    ).

visible_tree(Map, Exterior_trees, Interior_trees) :-
    area(Map, Width, Length),
    findall( T-X-Y,
             ( member(T-X-Y, Map),
               once(( member(X, [1,Width]); member(Y, [1,Length]) ))
             ),
             Exterior_trees),
    W is Width - 1,
    L is Length - 1,
    findall( T-X-Y,
             ( member(T-X-Y, Map),
               between(2, W, X),
               between(2, L, Y),
               visible(Map, X, Y)
             ),
             Interior_trees).

solution(N) :-
    read-input(input, I),
    visible_tree(I, Ts_e, Ts_i),
    length(Ts_e, N1),
    length(Ts_i, N2),
    N is N1 + N2.
%% (By query `?- solution(Count)`)
%% Your puzzle answer was `Count`.

%% --- Part Two ---
%% Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree house: they would like to be able to see a lot of trees.
%%
%% To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)
%%
%% The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has large eaves to keep it dry, so they wouldn't be able to see higher than the tree house anyway.
%%
%% In the example above, consider the middle 5 in the second row:
%%
%% 30373
%% 25512
%% 65332
%% 33549
%% 35390
%% Looking up, its view is not blocked; it can see 1 tree (of height 3).
%% Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
%% Looking right, its view is not blocked; it can see 2 trees.
%% Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view).
%% A tree's scenic score is found by multiplying together its viewing distance in each of the four directions. For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).
%%
%% However, you can do even better: consider the tree of height 5 in the middle of the fourth row:
%%
%% 30373
%% 25512
%% 65332
%% 33549
%% 35390
%% Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
%% Looking left, its view is not blocked; it can see 2 trees.
%% Looking down, its view is also not blocked; it can see 1 tree.
%% Looking right, its view is blocked at 2 trees (by a massive tree of height 9).
%% This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.
%%
%% Consider each tree on your map. What is the highest scenic score possible for any tree?
scenic_score(T, [], Score, Score) :-
    number(T), number(Score), !.
scenic_score(T, [H|_], Acc, Score) :-
    number(T), number(Acc),
    T =< H, !,
    Score is Acc + 1.
scenic_score(T, [_|L], Acc, Score) :-
    number(T), number(Acc), !,
    Acc2 is Acc + 1,
    scenic_score(T, L, Acc2, Score).
scenic_score(Map, X, Y, Score) :-
    is_list(Map), number(X), number(Y), !,
    member(C-X-Y, Map),
    findall(A0, (member(A0-X-Y0,Map),Y0<Y), T),
    scenic_score(C, T, 0, S_t),
    findall(B0, (member(B0-X0-Y,Map),X0<X), L),
    scenic_score(C, L, 0, S_l),
    findall(D0, (member(D0-X2-Y,Map),X<X2), R0),
    reverse(R0, R),
    scenic_score(C, R, 0, S_r),
    findall(E0, (member(E0-X-Y2,Map),Y<Y2), D0),
    reverse(D0, D),
    scenic_score(C, D, 0, S_d),
    Score is S_t * S_l * S_r * S_d.

scenic_score(Map, Score) :-
    area(Map, Width, Length),
    between(1, Width, X),
    between(1, Length, Y),
    scenic_score(Map, X, Y, Score).

max([], Max, Max).
max([H|L], Acc, Max) :-
    H < Acc, !,
    max(L, Acc, Max).
max([H|L], _, Max) :-
    max(L, H, Max).

solution2(N) :-
    read-input(input, I),
    findall(Ss, scenic_score(I, Ss), S),
    max(S, 0, N).
%% (By query `?- solution2(Max)`)
%% Your puzzle answer was `Max`.
