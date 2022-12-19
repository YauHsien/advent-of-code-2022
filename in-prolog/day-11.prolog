%% --- Day 11: Monkey in the Middle ---
%% As you finally start making your way upriver, you realize your pack is much lighter than you remember. Just then, one of the items from your pack goes flying overhead. Monkeys are playing Keep Away with your missing things!
%%
%% To get your stuff back, you need to be able to predict where the monkeys will throw your items. After some careful observation, you realize the monkeys operate based on how worried you are about each item.
%%
%% You take some notes (your puzzle input) on the items each monkey currently has, how worried you are about those items, and how the monkey makes decisions based on your worry level. For example:
%%
%% Monkey 0:
%%   Starting items: 79, 98
%%   Operation: new = old * 19
%%   Test: divisible by 23
%%     If true: throw to monkey 2
%%     If false: throw to monkey 3
%%
%% Monkey 1:
%%   Starting items: 54, 65, 75, 74
%%   Operation: new = old + 6
%%   Test: divisible by 19
%%     If true: throw to monkey 2
%%     If false: throw to monkey 0
%%
%% Monkey 2:
%%   Starting items: 79, 60, 97
%%   Operation: new = old * old
%%   Test: divisible by 13
%%     If true: throw to monkey 1
%%     If false: throw to monkey 3
%%
%% Monkey 3:
%%   Starting items: 74
%%   Operation: new = old + 3
%%   Test: divisible by 17
%%     If true: throw to monkey 0
%%     If false: throw to monkey 1
%% Each monkey has several attributes:
%%
%% Starting items lists your worry level for each item the monkey is currently holding in the order they will be inspected.
%% Operation shows how your worry level changes as that monkey inspects an item. (An operation like new = old * 5 means that your worry level after the monkey inspected the item is five times whatever your worry level was before inspection.)
%% Test shows how the monkey uses your worry level to decide where to throw an item next.
%% If true shows what happens with an item if the Test was true.
%% If false shows what happens with an item if the Test was false.
%% After each monkey inspects an item but before it tests your worry level, your relief that the monkey's inspection didn't damage the item causes your worry level to be divided by three and rounded down to the nearest integer.
%%
%% The monkeys take turns inspecting and throwing items. On a single monkey's turn, it inspects and throws all of the items it is holding one at a time and in the order listed. Monkey 0 goes first, then monkey 1, and so on until each monkey has had one turn. The process of each monkey taking a single turn is called a round.
%%
%% When a monkey throws an item to another monkey, the item goes on the end of the recipient monkey's list. A monkey that starts a round with no items could end up inspecting and throwing many items by the time its turn comes around. If a monkey is holding no items at the start of its turn, its turn ends.
%%
%% In the above example, the first round proceeds as follows:
%%
%% Monkey 0:
%%   Monkey inspects an item with a worry level of 79.
%%     Worry level is multiplied by 19 to 1501.
%%     Monkey gets bored with item. Worry level is divided by 3 to 500.
%%     Current worry level is not divisible by 23.
%%     Item with worry level 500 is thrown to monkey 3.
%%   Monkey inspects an item with a worry level of 98.
%%     Worry level is multiplied by 19 to 1862.
%%     Monkey gets bored with item. Worry level is divided by 3 to 620.
%%     Current worry level is not divisible by 23.
%%     Item with worry level 620 is thrown to monkey 3.
%% Monkey 1:
%%   Monkey inspects an item with a worry level of 54.
%%     Worry level increases by 6 to 60.
%%     Monkey gets bored with item. Worry level is divided by 3 to 20.
%%     Current worry level is not divisible by 19.
%%     Item with worry level 20 is thrown to monkey 0.
%%   Monkey inspects an item with a worry level of 65.
%%     Worry level increases by 6 to 71.
%%     Monkey gets bored with item. Worry level is divided by 3 to 23.
%%     Current worry level is not divisible by 19.
%%     Item with worry level 23 is thrown to monkey 0.
%%   Monkey inspects an item with a worry level of 75.
%%     Worry level increases by 6 to 81.
%%     Monkey gets bored with item. Worry level is divided by 3 to 27.
%%     Current worry level is not divisible by 19.
%%     Item with worry level 27 is thrown to monkey 0.
%%   Monkey inspects an item with a worry level of 74.
%%     Worry level increases by 6 to 80.
%%     Monkey gets bored with item. Worry level is divided by 3 to 26.
%%     Current worry level is not divisible by 19.
%%     Item with worry level 26 is thrown to monkey 0.
%% Monkey 2:
%%   Monkey inspects an item with a worry level of 79.
%%     Worry level is multiplied by itself to 6241.
%%     Monkey gets bored with item. Worry level is divided by 3 to 2080.
%%     Current worry level is divisible by 13.
%%     Item with worry level 2080 is thrown to monkey 1.
%%   Monkey inspects an item with a worry level of 60.
%%     Worry level is multiplied by itself to 3600.
%%     Monkey gets bored with item. Worry level is divided by 3 to 1200.
%%     Current worry level is not divisible by 13.
%%     Item with worry level 1200 is thrown to monkey 3.
%%   Monkey inspects an item with a worry level of 97.
%%     Worry level is multiplied by itself to 9409.
%%     Monkey gets bored with item. Worry level is divided by 3 to 3136.
%%     Current worry level is not divisible by 13.
%%     Item with worry level 3136 is thrown to monkey 3.
%% Monkey 3:
%%   Monkey inspects an item with a worry level of 74.
%%     Worry level increases by 3 to 77.
%%     Monkey gets bored with item. Worry level is divided by 3 to 25.
%%     Current worry level is not divisible by 17.
%%     Item with worry level 25 is thrown to monkey 1.
%%   Monkey inspects an item with a worry level of 500.
%%     Worry level increases by 3 to 503.
%%     Monkey gets bored with item. Worry level is divided by 3 to 167.
%%     Current worry level is not divisible by 17.
%%     Item with worry level 167 is thrown to monkey 1.
%%   Monkey inspects an item with a worry level of 620.
%%     Worry level increases by 3 to 623.
%%     Monkey gets bored with item. Worry level is divided by 3 to 207.
%%     Current worry level is not divisible by 17.
%%     Item with worry level 207 is thrown to monkey 1.
%%   Monkey inspects an item with a worry level of 1200.
%%     Worry level increases by 3 to 1203.
%%     Monkey gets bored with item. Worry level is divided by 3 to 401.
%%     Current worry level is not divisible by 17.
%%     Item with worry level 401 is thrown to monkey 1.
%%   Monkey inspects an item with a worry level of 3136.
%%     Worry level increases by 3 to 3139.
%%     Monkey gets bored with item. Worry level is divided by 3 to 1046.
%%     Current worry level is not divisible by 17.
%%     Item with worry level 1046 is thrown to monkey 1.
%% After round 1, the monkeys are holding items with these worry levels:
%%
%% Monkey 0: 20, 23, 27, 26
%% Monkey 1: 2080, 25, 167, 207, 401, 1046
%% Monkey 2:
%% Monkey 3:
%% Monkeys 2 and 3 aren't holding any items at the end of the round; they both inspected items during the round and threw them all before the round ended.
%%
%% This process continues for a few more rounds:
%%
%% After round 2, the monkeys are holding items with these worry levels:
%% Monkey 0: 695, 10, 71, 135, 350
%% Monkey 1: 43, 49, 58, 55, 362
%% Monkey 2:
%% Monkey 3:
%%
%% After round 3, the monkeys are holding items with these worry levels:
%% Monkey 0: 16, 18, 21, 20, 122
%% Monkey 1: 1468, 22, 150, 286, 739
%% Monkey 2:
%% Monkey 3:
%%
%% After round 4, the monkeys are holding items with these worry levels:
%% Monkey 0: 491, 9, 52, 97, 248, 34
%% Monkey 1: 39, 45, 43, 258
%% Monkey 2:
%% Monkey 3:
%%
%% After round 5, the monkeys are holding items with these worry levels:
%% Monkey 0: 15, 17, 16, 88, 1037
%% Monkey 1: 20, 110, 205, 524, 72
%% Monkey 2:
%% Monkey 3:
%%
%% After round 6, the monkeys are holding items with these worry levels:
%% Monkey 0: 8, 70, 176, 26, 34
%% Monkey 1: 481, 32, 36, 186, 2190
%% Monkey 2:
%% Monkey 3:
%%
%% After round 7, the monkeys are holding items with these worry levels:
%% Monkey 0: 162, 12, 14, 64, 732, 17
%% Monkey 1: 148, 372, 55, 72
%% Monkey 2:
%% Monkey 3:
%%
%% After round 8, the monkeys are holding items with these worry levels:
%% Monkey 0: 51, 126, 20, 26, 136
%% Monkey 1: 343, 26, 30, 1546, 36
%% Monkey 2:
%% Monkey 3:
%%
%% After round 9, the monkeys are holding items with these worry levels:
%% Monkey 0: 116, 10, 12, 517, 14
%% Monkey 1: 108, 267, 43, 55, 288
%% Monkey 2:
%% Monkey 3:
%%
%% After round 10, the monkeys are holding items with these worry levels:
%% Monkey 0: 91, 16, 20, 98
%% Monkey 1: 481, 245, 22, 26, 1092, 30
%% Monkey 2:
%% Monkey 3:
%%
%% ...
%%
%% After round 15, the monkeys are holding items with these worry levels:
%% Monkey 0: 83, 44, 8, 184, 9, 20, 26, 102
%% Monkey 1: 110, 36
%% Monkey 2:
%% Monkey 3:
%%
%% ...
%%
%% After round 20, the monkeys are holding items with these worry levels:
%% Monkey 0: 10, 12, 14, 26, 34
%% Monkey 1: 245, 93, 53, 199, 115
%% Monkey 2:
%% Monkey 3:
%% Chasing all of the monkeys at once is impossible; you're going to have to focus on the two most active monkeys if you want any hope of getting your stuff back. Count the total number of times each monkey inspects items over 20 rounds:
%%
%% Monkey 0 inspected items 101 times.
%% Monkey 1 inspected items 95 times.
%% Monkey 2 inspected items 7 times.
%% Monkey 3 inspected items 105 times.
%% In this example, the two most active monkeys inspected items 101 and 105 times. The level of monkey business in this situation can be found by multiplying these together: 10605.
%%
%% Figure out which monkeys to chase by counting how many items they inspect over 20 rounds. What is the level of monkey business after 20 rounds of stuff-slinging simian shenanigans?
day11.
input(File) :-
    source_file(day11, F),
    relative_file_name(File, F, '../input/day-11.input').

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
    once(case_of(S, Acc, Acc2)),
    get-result(StreamIn, Acc2, Result).

:- dynamic monkey/1,
           hold/2,
           div_op/1,
           operation/3,
           test/2,
           on_true/2,
           on_false/2,
           inspect/2,
           mod/2,
           worry_level/1.

:- foreach(member(N,[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]),
           assertz(div_op(N))).

clear_database :-
    abolish(monkey/1),
    abolish(hold/2),
    abolish(operation/3),
    abolish(test/2),
    abolish(on_true/2),
    abolish(on_false/2),
    abolish(inspect/2),
    abolish(mod/2),
    abolish(worry_level/1).

case_of(S, Acc, [monkey-Nth:[]|Acc]) :-
    string_concat("Monkey ", R, S),
    string_concat(Nth, ":", R),
    assertz(monkey(Nth)),
    assertz(inspect(Nth,0)).
case_of(S, [monkey-Nth:K|Acc], [monkey-Nth:Items|Acc]) :-
    string_concat("  Starting items: ", R, S),
    split_string(R, ", ", "", R2),
    foreach(( member(X0, R2),
              number_string(X, X0)
            ),
            assertz(hold(Nth, X))),
    findall(X, (member(X0,R2),number_string(X,X0)), L),
    append(K, L, Items).
case_of(S, [monkey-Nth:L|Acc], [monkey-Nth:L|Acc]) :-
    string_concat("  Operation: ", R, S),
    split_string(R, " ", " ", ["new","=",Val2,Op,Val3]),
    worry_level(W),
    ( Op = "*", Val2 == Val3, Val2 == "old",
      ( W =< 1, !,
        Q = (operation(Nth,A,(A*A)))
      ; Q = (operation(Nth,A,A2) :- A2 is (A*A) div W)
      )
    ; Op = "+", Val2 == Val3, Val2 == "old",
      ( W =< 1, !,
        Q = (operation(Nth,A,(A*2)))
      ; Q = (operation(Nth,A,A2) :- A2 is (A+A) div W)
      )
    ; Op = "*", Val2 == "old", number_string(M, Val3),
      ( W =< 1, !,
        Q = (operation(Nth,A,(A*M)))
      ; Q = (operation(Nth,A,A2) :- A2 is (A*M) div W)
      )
    ; Op = "+", Val2 == "old", number_string(M, Val3),
      ( W =< 1, !,
        Q = (operation(Nth,A,(A+M)))
      ; Q = (operation(Nth,A,A2) :- A2 is (A+M) div W)
      )
    ),
    assertz(Q).
case_of(S, [monkey-Nth:L|Acc], [monkey-Nth:L|Acc]) :-
    string_concat("  Test: divisible by ", R, S),
    number_string(N, R),
    assertz( mod(Nth, N) ),
    assertz( test(Nth, A) :- A mod N =:= 0 ).
case_of(S, [monkey-Nth:L|Acc], [monkey-Nth:L|Acc]) :-
    string_concat("    If true: throw to monkey ", Mth, S),
    assertz( on_true(Nth, Mth) ).
case_of(S, [monkey-Nth:L|Acc], [monkey-Nth:L|Acc]) :-
    string_concat("    If false: throw to monkey ", Mth, S),
    assertz( on_false(Nth, Mth) ).
case_of("", Acc, Acc).

factorize(N, Fs) :-
    findall(M, div_op(M), Ms),
    factorize(N, Ms, [], Fs).

factorize(1, [], Acc, R) :- !,
    sort(Acc, R).
factorize(N, [], Acc, R) :-
    sort([N^1|Acc], R).
factorize(N, [M|Ms], Acc, Fs) :-
    fact(N, M, 0, F, N2), !,
    factorize(N2, Ms, [F|Acc], Fs).
factorize(N, [_|Ms], Acc, Fs) :-
    factorize(N, Ms, Acc, Fs).

fact(N, M, Acc, F, R) :-
    N mod M =:= 0, !,
    N2 is N div M,
    fact(N2, M, Acc+1, F, R).
fact(N, M, Acc, (M^Acc2), N) :-
    Acc > 0,
    Acc2 is Acc.

square(Factors, Square) :-
    findall(N^S2, (member(N^S,Factors),
                   S2 is S * 2), Square).

double([2^E|Factors], [2^E2|Factors]) :- !,
    E2 is E + 1.
double(Factors, [2^1|Factors]).

multiply(Factors, Ms, R) :-
    findall(M0, (member(M0^_,Factors)
                ;member(M0^_,Ms)), Ms0),
    sort(Ms0, Ms2),
    findall(M0^E0, (member(M0,Ms2),
                    findall(E0, (member(M0^E0,Factors)
                                ;member(M0^E0,Ms)), L0),
                    sum_list(L0, E0)), R).

add(Factors, As, R) :-
    intersection(Factors, As, As3),
    As3 \= [], !,
    findall(X, (member(X,Factors),
                \+ member(X,As3)), As4),
    findall(X, (member(X,As),
                \+ member(X,As3)), As5),
    eval(As4, R0),
    eval(As5, R2),
    R3 is R0 + R2,
    factorize(R3, F0),
    multiply(As3, F0, R5),
    sort(R5, R).
add(Factors, As, R) :-
    eval(Factors, R0),
    eval(As, R2),
    R3 is R0 + R2,
    factorize(R3, F0),
    sort(F0, R).

eval(Factors, R) :-
    eval(Factors, 1, R).

eval([], Acc, R) :- !,
    R is Acc.
eval([A^E|Factors], Acc, R) :-
    eval(Factors, Acc*A^E, R).

cascade_mod((A*1), K, R) :-
    R is A mod K.
cascade_mod((A*M), K, R) :-
    M > 1, !,
    ( % A = B*K+C, M = D*K+E, A*M = (B*K+C)*(D*K+E), (A*M)%K = ((B*K+C)*(D*K+E))%K
        true
    ),
    M2 is M - 1,
    cascade_mod((A*M2), K, R).

mul_list([], 1).
mul_list([X|L], R) :-
    mul_list(L, R0),
    R is R0 * X.

big_op(Nth, A, ( inc(Nth), once(retract(hold(Nth,A))), assertz(hold(Mth,R)) )) :-
    worry_level(W),
    W =< 1, !,
    ( findall(K, mod(_,K), K0),
      mul_list(K0, K) ),
    operation(Nth, A, R0),
    R is R0 mod K,
    test(Nth, R, Mth).
big_op(Nth, A, (inc(Nth),once(retract(hold(Nth,A))),assertz(hold(Mth,R)))) :-
    operation(Nth, A, R),
    %format("(~w) ~w --> ~w (~w)~n", [Nth, A, R0, R]),
    test(Nth, R, Mth).

test(Nth, R, Mth) :-
    ( test(Nth, R), !,
      %format("divisible !~n"),
      on_true(Nth, Mth)
    ; on_false(Nth, Mth)
    ),
    %format("sent to ~w~n", [Mth]),
    true.

pred_inspect(>, monkey-_:inspected_times-A, monkey-_:inspected_times-B) :-
    A>B.
pred_inspect(<, monkey-_:inspected_times-A, monkey-_:inspected_times-B) :-
    A<B.
pred_inspect(=:=, monkey-_:inspected_times-A, monkey-_:inspected_times-B) :-
    A=:=B.

round(0).
round(N) :-
    N > 0,
    findall(Nth, monkey(Nth), Monkeys),
    monkeys_process(Monkeys),
    %( M is 10_000 - N + 1,
    %  member(M, [1,20,1000,2000,3000,4000,5000,6000,7000,8000,9000,10_000]), !,
    %  write(M), write(' -------'), nl,
    %  foreach(( monkey(Nth),
    %            inspect(Nth, A)
    %          ),
    %          writeln(Nth: A) )
    %; true),
    N2 is N - 1,
    round(N2).

monkeys_process([]).
monkeys_process([Nth|Monkeys]) :-
    foreach(( hold(Nth, Thing),
              big_op(Nth, Thing, Goal) ),
            call(once(Goal)) ),
    monkeys_process(Monkeys).

inc(Nth) :-
    inspect(Nth, N),
    retract(inspect(Nth,N)),
    N2 is N + 1,
    assertz(inspect(Nth,N2)).

solution(S) :-
    clear_database,
    assertz(worry_level(3)),
    read-input(input, _Config),
    round(20),
    findall( monkey-Nth:inspected_times-N,
             ( monkey(Nth),
               inspect(Nth, N)
             ),
             Inspection ),
    predsort(pred_inspect, Inspection, S0),
    append(_, [monkey-_:inspected_times-A,
               monkey-_:inspected_times-B], S0),
    S is A * B, !.
%% (By query `?- solution(Product).`)
%% Your puzzle answer was `Product`.

%% --- Part Two ---
%% (In short, count by 10,000 rounds.)
%% (It's hard fight to big, time-consuming computation.)

:- set_prolog_flag(stack_limit, 34_359_738_368).
%:- set_prolog_flag(stack_limit, 137_438_953_472).

solution2(fresh_start, Rounds, S) :-
    clear_database,
    W is 1,
    assertz(worry_level(W)),
    read-input(input, _Config),
    solution2(continue, Rounds, S).
solution2(continue, Rounds, S) :-
    round(Rounds),
    foreach(( monkey(Nth),
              inspect(Nth, N)
            ),
            writeln(Nth: N) ),
    ( findall( monkey-Nth:inspected_times-N,
               ( monkey(Nth),
                 inspect(Nth, N)
               ),
               Inspection ),
      predsort(pred_inspect, Inspection, S0),
      append(_, [monkey-_:inspected_times-A,
                 monkey-_:inspected_times-B], S0) ),
    S is A * B,
    !.

solution2(S) :-
    Rounds = 10_000,
    Unit = 20,
    Times is Rounds div Unit - 1,
    solution2(fresh_start, Unit),
    foreach( between(1, Times, I),
             ( format("- ~d ----~n", [(I+1)*Unit]),
               flush_output,
               time(solution2(continue, Unit))) ),
    findall( monkey-Nth:inspected_times-N,
             ( monkey(Nth),
               inspect(Nth, N)
             ),
             Inspection ),
    predsort(pred_inspect, Inspection, S0),
    append(_, [monkey-_:inspected_times-A,
               monkey-_:inspected_times-B], S0),
    S is A * B,
    !.
