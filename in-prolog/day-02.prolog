%% --- Day 2: Rock Paper Scissors ---
%% The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to the snack storage, a giant Rock Paper Scissors tournament is already in progress.
%%
%% Rock Paper Scissors is a game between two players. Each game contains many rounds; in each round, the players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape. Then, a winner for that round is selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players choose the same shape, the round instead ends in a draw.
%%
%% Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that they say will be sure to help you win. "The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's tent.
%%
%% The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors. Winning every time would be suspicious, so the responses must have been carefully chosen.
%%
%% The winner of the whole tournament is the player with the highest score. Your total score is the sum of your scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
%%
%% Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the score you would get if you were to follow the strategy guide.
%%
%% For example, suppose you were given the following strategy guide:
%%
%% A Y
%% B X
%% C Z
%% This strategy guide predicts and recommends the following:
%%
%% In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because you won).
%% In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
%% The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6.
%% In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6).
%%
%% What would your total score be if everything goes exactly according to your strategy guide?
day02.
input(File) :-
    source_file(day02, F),
    relative_file_name(File, F, '../input/day-02.input').

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

%score(rock, 1).
%score(paper, 2).
%score(scissors, 3).
%score(lost, 0).
%score(draw, 3).
%score(win, 6).
score("A X", 4).
score("A Y", 8).
score("A Z", 3).
score("B X", 1).
score("B Y", 5).
score("B Z", 9).
score("C X", 7).
score("C Y", 2).
score("C Z", 6).

solution(Total) :-
    read-input(input, I),
    findall(X,
            (member(S, I),
             score(S, X)
            ),
            R),
    sum_list(R, Total).
%% (By query `?- solution(Total)`)
%% Your puzzle answer was `Total`.

%% --- Part Two ---
%% The Elf finishes helping with the tent and sneaks back over to you. "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"
%%
%% The total score is still calculated in the same way, but now you need to figure out what shape to choose so the round ends as indicated. The example above now goes like this:
%%
%% In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y), so you also choose Rock. This gives you a score of 1 + 3 = 4.
%% In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
%% In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.
%% Now that you're correctly decrypting the ultra top secret strategy guide, you would get a total score of 12.
%%
%% Following the Elf's instructions for the second column, what would your total score be if everything goes exactly according to your strategy guide?
nscore("A X", 0, 3).
nscore("A Y", 3, 1).
nscore("A Z", 6, 2).
nscore("B X", 0, 1).
nscore("B Y", 3, 2).
nscore("B Z", 6, 3).
nscore("C X", 0, 2).
nscore("C Y", 3, 3).
nscore("C Z", 6, 1).

solution2(Total) :-
    read-input(input, I),
    findall(A+B,
            (member(S, I),
             nscore(S, A, B)
            ),
            R),
    sum_list(R, Total).
%% (By query `?- solution2(Total2)`)
%% Your puzzle answer was `Total2`.


