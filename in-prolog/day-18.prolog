%% --- Day 18: Boiling Boulders ---
%% You and the elephants finally reach fresh air. You've emerged near the base of a large volcano that seems to be actively erupting! Fortunately, the lava seems to be flowing away from you and toward the ocean.
%%
%% Bits of lava are still being ejected toward you, so you're sheltering in the cavern exit a little longer. Outside the cave, you can see the lava landing in a pond and hear it loudly hissing as it solidifies.
%%
%% Depending on the specific compounds in the lava and speed at which it cools, it might be forming obsidian! The cooling rate should be based on the surface area of the lava droplets, so you take a quick scan of a droplet as it flies past you (your puzzle input).
%%
%% Because of how quickly the lava is moving, the scan isn't very good; its resolution is quite low and, as a result, it approximates the shape of the lava droplet with 1x1x1 cubes on a 3D grid, each given as its x,y,z position.
%%
%% To approximate the surface area, count the number of sides of each cube that are not immediately connected to another cube. So, if your scan were only two adjacent cubes like 1,1,1 and 2,1,1, each cube would have a single side covered and five sides exposed, a total surface area of 10 sides.
%%
%% Here's a larger example:
%%
%% 2,2,2
%% 1,2,2
%% 3,2,2
%% 2,1,2
%% 2,3,2
%% 2,2,1
%% 2,2,3
%% 2,2,4
%% 2,2,6
%% 1,2,5
%% 3,2,5
%% 2,1,5
%% 2,3,5
%% In the above example, after counting up all the sides that aren't connected to another cube, the total surface area is 64.
%%
%% What is the surface area of your scanned lava droplet?
day18.
input(File) :-
    source_file(day18, F),
    relative_file_name(File, F, '../input/day-18.input').

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
    split_string(S, ",", "", S0),
    findall(N, (member(X,S0),
                number_string(N,X)), [A,B,C]),
    get-result(StreamIn, [(A,B,C)|Acc], Result).

solution(R) :-
    read-input(input, I),
    ( findall(A0-B0, (( member((X,Y,Z0), I),
                        member((X,Y,Z2), I), abs(Z0-Z2) =:= 1,
                        A = (X,Y,Z0), B = (X,Y,Z2)
                      ; member((X,Y0,Z), I),
                        member((X,Y2,Z), I), abs(Y0-Y2) =:= 1,
                        A = (X,Y0,Z), B = (X,Y2,Z)
                      ; member((X0,Y,Z), I),
                        member((X2,Y,Z), I), abs(X0-X2) =:= 1,
                        A = (X0,Y,Z), B = (X2,Y,Z) ),
                      sort([A,B], [A0,B0])), R0),
      sort(R0, Connected),
      length(Connected, Connected_count) ),
    Covered_count is Connected_count * 2,
    ( length(I, N),
      Exposed_count is N * 6 - Covered_count
    ),
    R = Exposed_count.
%% (By query `?- solution(Count).`)
%% Your puzzle answer was `Count`.

%% --- Part Two ---
%% Something seems off about your calculation. The cooling rate depends on exterior surface area, but your calculation also included the surface area of air pockets trapped in the lava droplet.
%%
%% Instead, consider only cube sides that could be reached by the water and steam as the lava droplet tumbles into the pond. The steam will expand to reach as much as possible, completely displacing any air on the outside of the lava droplet but never expanding diagonally.
%%
%% In the larger example above, exactly one cube of air is trapped within the lava droplet (at 2,2,5), so the exterior surface area of the lava droplet is 58.
%%
%% What is the exterior surface area of your scanned lava droplet?

x_view(I, (I,Y,Z), (Y,Z)).

y_view(I, (X,I,Z), (X,Z)).

z_view(I, (X,Y,I), (X,Y)).

print_slice(Goal_view, L, I, Xm, Xx, Ym, Yx) :-
    findall((X0,Y0), ( member(X, L),
                       call(Goal_view,I,X,(X0,Y0)) ), Ds),
    foreach(between(Ym,Yx,Y0),
            ( foreach( between(Xm,Xx,X0),
                       ( member((X0,Y0), Ds),
                         write(#)
                       ; \+ member((X0,Y0), Ds),
                         write(.) )),
              nl )), !.
% ?- read-input(input, I), J = 15, print_slice(x_view, I, J, 0, 21, 0, 21).

solution2(R) :-
    read-input(input, I),
    ( findall(X, member((X,_,_),I), Xa),
      min_list(Xa, Xm),
      max_list(Xa, Xx) ),
    ( findall(Y, member((_,Y,_),I), Ya),
      min_list(Ya, Ym),
      max_list(Ya, Yx) ),
    ( findall(Z, member((_,_,Z),I), Za),
      min_list(Za, Zm),
      max_list(Za, Zx) ),
    ( findall((X,Y,Z), ( between(Xm, Xx, X),
                   between(Ym, Yx, Y),
                   between(Zm, Zx, Z),
                   \+ member((X,Y,Z), I),
                   X0 is X - 1,
                   X2 is X + 1,
                   Y0 is Y - 1,
                   Y2 is Y + 1,
                   Z0 is Z - 1,
                   Z2 is Z + 1,
                   member((X0,Y,Z), I),
                   member((X2,Y,Z), I),
                   member((X,Y0,Z), I),
                   member((X,Y2,Z), I),
                   member((X,Y,Z0), I),
                   member((X,Y,Z2), I) ), As),
      sort(As, Air_pockets),
      length(Air_pockets, Air_pockets_count)
    ),
    ( solution(Old_exposed_count),
      Exposed_count is Old_exposed_count - 6 * Air_pockets_count ),
    R is Exposed_count.
