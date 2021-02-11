:-use_module(library(clpfd)).
:-["input.pl"].
:-op(920, fy, *).
*_. % for easier debugging

boundaries(0, 15).
touchdown(point(X,Y)):-
    t(X,Y).
orc(point(X,Y)):-
    o(X,Y).


human(point(X,Y)):-
    h(X,Y).


% state(Pos:point,Pass:{1, 0}, Moves:Int)
valid_move(hand_off, S0, S):-
    S0 = state(P0, Pass, Moves),
    S = state(P, Pass, Moves),
    human(P),
    neighbours(P, P0).

valid_move(run, S0, S):-
    S0 = state(P0, Pass, Moves0),
    S = state(P, Pass, Moves),
    neighbours(P, P0),
    \+ human(P),
    Moves #= Moves0 + 1.

valid_move(pass, S0, S):-
    S0 = state(P0, 0, Moves0),
    S = state(P, 1, Moves),
    dif(P0, P),
    human(P),
    point_pass(P0, P),
    Moves #= Moves0 +1.
%?- valid_move(pass, state(point(1,1), 0, 0), S). 
check_orc(Pos):-
    (orc(Pos) -> false; true).

c_successful_move(hand_off, S0, S):-
    valid_move(hand_off, S0, S),
    S = state(Pos, _, _),
    S0 = state(Pos0, _, _),
    check_orc(Pos0),
    check_orc(Pos).

c_successful_move(run, S0, S):-
    valid_move(run, S0, S),
    S = state(Pos, _, _),
    S0 = state(Pos0, _, _),
    check_orc(Pos0),
    check_orc(Pos).
    
c_successful_move(pass, S0, S):-  
    valid_move(pass, S0, S),
    S0 = state(Pos0, _, _),
    S = state(Pos, _, _),
    no_interception(Pos0, Pos).

no_interception(point(X,Y), point(X,Y)):-
    point(X,Y),
    check_orc(point(X,Y)).


no_interception(point(X,Y), point(X1,Y)):- %horizontal case
    X #\=X1,
    point(X,Y),
    check_orc(point(X,Y)),
    Dx*abs(X1-X) #= X1 - X,
    X2 #= X + Dx,
    no_interception(point(X2,Y), point(X1,Y)).

no_interception(point(X,Y), point(X,Y1)):- %vertical case
    Y1 #\= Y,
    point(X,Y),
    check_orc(point(X,Y)),
    Dy*abs(Y1-Y) #= Y1 - Y,
    Y2 #= Y + Dy,
    no_interception(point(X,Y2), point(X,Y1)).

no_interception(point(X,Y), point(X1,Y1)):- %diagonal case
    X1 #\= X,
    Y1 #\= Y,
    point(X,Y),
    check_orc(point(X,Y)),
    Dx*abs(X1-X) #= X1 - X,
    X2 #= X + Dx,
    Dy*abs(Y1-Y) #= Y1 - Y,
    Y2 #= Y + Dy,
    no_interception(point(X2,Y2), point(X1,Y1)).

%?- no_interception(point(3,0), P).




%?- c_successful_move(pass, state(point(3,0), 0, _), S).

    




point(X, Y):-
    boundaries(Min, Max),
    [X, Y] ins Min..Max. 

pos_see(Pos, Known):- %what agenet sees standing in point Pos
  %^
  %o
    findall(orc-X, (neighbours(Pos, X), orc(X)), Orcs),
    findall(human-X, (neighbours(Pos, X), human(X)), Humans),
    findall(touchdown-X, (neighbours(Pos, X), touchdown(X)), Touchdowns),
    append(Orcs, Humans, Orcs_Humans),
    append(Orcs_Humans, Touchdowns, Known).
%?- pos_see(point(2,1), Known).
%@ Known = [human-point(1, 1), touchdown-point(3, 1)].

% state(Pos:point,Pass:{1, 0}, Moves:Int)

neighbours(point(X,Y), point(X1,Y)):-
    point(X,Y),
    point(X1, Y),
    (X - X1 #= -1;
    X - X1 #= 1).
neighbours(point(X,Y), point(X,Y1)):-
    point(X,Y),
    point(X,Y1),
    (Y - Y1 #= -1;
     Y - Y1 #= 1).

horizontal(point(X,_), point(X,_)).
vertical(point(_,Y), point(_,Y)).
ldiagonal(point(X,Y), point(X1,Y1)):- %left diagonal goes from top left corner to bottom right
    X - X1 #= Y1 - Y.                 %cause I said so
rdiagonal(point(X,Y), point(X1,Y1)):-
    X1 - X #= Y1 - Y.

%?-ldiagonal(point(5,5), point(X,Y)), [X,Y] ins 1..20, label([X,Y]).

point_pass(P0, P):-
    P0 = point(X,Y),
    P = point(X1, Y1),
    point(X, Y),
    point(X1, Y1),
    (horizontal(P0, P);
    vertical(P0, P);
    ldiagonal(P0, P);
    rdiagonal(P0, P)).

%?- point_pass(point(X, Y), point(1,1)).



%?- neighbours(X, point(1,0)).
%?- length(Ls, 4),list_has(Ls, a).
%?- neighbours(X, point(0, 0)).

point_point_dist(point(X,Y), point(X1,Y1), D):-
    D #= abs(X-X1) + abs(Y-Y1).
%?-point_point_dist(point(0,0), point(X,Y), 3),labeling([ff],[X,Y]), call(point(X,Y)).

write_path([]).
write_path([Move-S|Hist]):-
    S = state(point(X,Y), _, _),
    format("~w ~w ~w~n", [Move, X, Y]),
    write_path(Hist).

start_random_finish(S, [], S).
start_random_finish(S0, [Cmove-Cstate|Hist], F):-
    findall(Move-S, valid_move(Move, S0, S), Choices),
    length(Choices, L),
    random_between(1, L, Rand),
    nth1(Rand, Choices, Cmove-Cstate),
    c_successful_move(Cmove, S0, Cstate),
    start_random_finish(Cstate, Hist, F).

    

path_path_best(Moves0-Hist0, Moves1-_Hist1, Moves0-Hist0):-
    Moves0 #=< Moves1.
path_path_best(Moves0-_Hist0, Moves1-Hist1, Moves1-Hist1):-
    Moves1 #=< Moves0.

try_n(1, Result):-
    touchdown(T),
    (start_random_finish(state(point(0,0), 0, 0), Hist, state(T, _, Moves))->
	 Result = Moves-Hist;
         Result = 1000-[]).

try_n(N, Best):-
    N #> 1,
    N0 #= N-1,
    try_n(N0, Best0),
    touchdown(T),
    (start_random_finish(state(point(0,0), 0, 0), Hist, state(T, _, Moves))->
	 Best1 = Moves-Hist;
         Best1 = 1000-[]),
    path_path_best(Best0, Best1, Best).
%?- once(try_n(100, Moves-Hist)),format("~w~n", [Moves]), write_path(Hist).
%@ 3
%@ pass 4 4
%@ run 5 4
%@ run 5 5
%@ Moves = 3,
%@ Hist = [pass-state(point(4, 4), 1, 1), run-state(point(5, 4), 1, 2), run-state(point(5, 5), 1, 3)].


%?- random_try_N(Hist, Moves, 100).

%?- touchdown(T), start_random_finish(state(point(0,0), 0, 0), Hist, state(T, _, Moves)).

start_backtracking_finish(S, [], S).
start_backtracking_finish(S0, [Move-S|Hist], F):-
    c_successful_move(Move, S0, S),
    maplist(dif(_-S), Hist),
    start_backtracking_finish(S, Hist, F).

%?- S0 = state(point(0, 0), 0, 0), F = state(T, _, Moves), touchdown(T), start_backtracking_finish(S0, Hist, F), labeling([min(Moves)], [Moves]).

%  djs_ describes the transitions between two states of Dijkstra's algorithm,
%  while djs iterates over these states untill it reaches final state

%?- Check = [point(3,3)], Pos = point(X,Y), point(X,Y), list_lacks(Check, Pos),X=3, label([X,Y]).
djs(Visited-_, _, Visited-_):-
    touchdown(T),
    member(turn(_, T, _), Visited).
djs(Visited-[], 1, Visited-[]).
djs(Visited-[], 0, Final):-
    % try passing to solve map which are impossible to solve otherwize
    Goal = (member(turn(_, From, Price), Visited),
	    To = point(X,Y),
	    point(X,Y),
	    label([X,Y]),
	    \+ member(turn(_, To, Price), Visited),
	    c_successful_move(pass, state(From, 0, _Price0), state(To, 1, Price))),
    findall(turn(From, To, Price), Goal, Passes),
    to_check_updated([], Passes, ToCheck),
    djs(Visited-ToCheck, 1,  Final).

    
djs(Current, Passed, Final):-                
    djs_(Current, Next),
    djs(Next, Passed, Final).

to_check_updated(ToCheck, [], ToCheck).
to_check_updated(ToCheck, Candidates, ToCheck1):-
    append([ToCheck, Candidates], Unsorted),
    sort(3, @=<, Unsorted, ToCheckDups),
    sort(2, @<, ToCheckDups, NoDups),
    sort(3, @=<, NoDups, ToCheck1).

    
djs_(Visited-[Current|ToCheck0], [Current|Visited]-ToCheck1):-
    Current = turn(_, From, Price0),
    findall(turn(From, To, Price), (c_successful_move(_, state(From, 1, Price0), state(To, 1, Price)),
			 \+ member(turn(_, To, _),Visited)), Neighbours),
    to_check_updated(ToCheck0, Neighbours, ToCheck1).

knowledge_path(Knowledge, S, Hist, F):-
    kp(Knowledge, S, Rev, F),
    reverse(Rev, Hist).
    
kp(_Knowledge, S, [], S).
kp(Knowledge, S, [Move-F|Hist], F):-
    F = state(Pos, _, Price),
    F0 = state(Pos0, _, _),
    member(turn(Pos0, Pos, Price), Knowledge),
    valid_move(Move, F0, F),
    kp(Knowledge, S, Hist, F0).

%?- _S0 = [turn(point(0,0), point(0,0), 0)], once(djs([]-_S0, 0, Visited-_)), touchdown(T), knowledge_path(Visited, state(point(0,0), 0, 0), Path, state(T, _, Moves)). 


