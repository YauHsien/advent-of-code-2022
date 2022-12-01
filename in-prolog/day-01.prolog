%% --- Day 1: Calorie Counting ---
%% Santa's reindeer typically eat regular reindeer food, but they need a lot of magical energy to deliver presents on Christmas. For that, their favorite snack is a special type of star fruit that only grows deep in the jungle. The Elves have brought you on their annual expedition to the grove where the fruit grows.
%%
%% To supply enough magical energy, the expedition needs to retrieve a minimum of fifty stars by December 25th. Although the Elves assure you that the grove has plenty of fruit, you decide to grab any fruit you see along the way, just in case.
%%
%% Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
%%
%% The jungle must be too overgrown and difficult to navigate in vehicles or access from the air; the Elves' expedition traditionally goes on foot. As your boats approach land, the Elves begin taking inventory of their supplies. One important consideration is food - in particular, the number of Calories each Elf is carrying (your puzzle input).
%%
%% The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc. that they've brought with them, one item per line. Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.
%%
%% For example, suppose the Elves finish writing their items' Calories and end up with the following list:
%%
%% 1000
%% 2000
%% 3000
%%
%% 4000
%%
%% 5000
%% 6000
%%
%% 7000
%% 8000
%% 9000
%%
%% 10000
%% This list represents the Calories of the food carried by five Elves:
%%
%% The first Elf is carrying food with 1000, 2000, and 3000 Calories, a total of 6000 Calories.
%% The second Elf is carrying one food item with 4000 Calories.
%% The third Elf is carrying food with 5000 and 6000 Calories, a total of 11000 Calories.
%% The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a total of 24000 Calories.
%% The fifth Elf is carrying one food item with 10000 Calories.
%% In case the Elves get hungry and need extra snacks, they need to know which Elf to ask: they'd like to know how many Calories are being carried by the Elf carrying the most Calories. In the example above, this is 24000 (carried by the fourth Elf).
%%
%% Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
day01.
input(File) :-
    source_file(day01, F),
    relative_file_name(File, F, '../input/day-01.input').

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

max([], Acc, Base, Calories) :-
    greater(Acc, Base, Calories).
max([""|R], Acc, Base, Calories) :-
    greater(Acc, Base, G),
    max(R, [], G, Calories).
max([H|R], Acc, Base, Calories) :-
    max(R, [H|Acc], Base, Calories).

greater(List, Number, Greater) :-
    findall(N,
            (member(S,List), number_chars(N,S)),
            L),
    sum_list(L, N),
    ( N > Number,
      Greater = N
    ; Greater = Number
    ).

solution(Calories) :-
    read-input(input, Input),
    max(Input, [], 0, Calories), !.
%% (By running `?- solution(C).`)
%% Your puzzle answer was `C`.

%% --- Part Two ---
%% By the time you calculate the answer to the Elves' question, they've already realized that the Elf carrying the most Calories of food might eventually run out of snacks.
%%
%% To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups.
%%
%% In the example above, the top three Elves are the fourth Elf (with 24000 Calories), then the third Elf (with 11000 Calories), then the fifth Elf (with 10000 Calories). The sum of the Calories carried by these three elves is 45000.
%%
%% Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?

top3([], Acc, Base, Calories) :-
    greater3(Acc, Base, Calories).
top3([""|R], Acc, Base, Calories) :-
    greater3(Acc, Base, G),
    top3(R, [], G, Calories).
top3([H|R], Acc, Base, Calories) :-
    top3(R, [H|Acc], Base, Calories).

greater3(List, Nums, [A,B,C]) :-
    findall(N,
            (member(S,List), number_chars(N,S)),
            L),
    sum_list(L, N),
    msort([N|Nums], Sr),
    reverse(Sr, [A,B,C|_]).

solution2(Calories) :-
    read-input(input, Input),
    top3(Input, [], [0,0,0], Cs), !,
    sum_list(Cs, Calories).
%% (By running `?- solution2(C2).`)
%% Your puzzle answer was (C2).
