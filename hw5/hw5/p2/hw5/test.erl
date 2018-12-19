-module(test).
-export([createSeq/1, getSecond/1, area/1, area2/1, tailFac/1, fac/1, grade/1, grade2/1, countDown/1, countUp/1, countUp2/1, countUpNonTail/1, tailListSum/1]).

%% area function uses function head to pattern match 'tagged tuple'
%% representing shape
area({rect, Width, Height}) ->
    Width * Height;
area({circle, Radius}) ->
    math:pi() * Radius * Radius;
area(_) ->
    0. % default?

%% Same as above using a case expression
area2(Shape) ->
    case Shape of
	{rect, Width, Height} ->
	    Width * Height;
	{circle, Radius} ->
	    math:pi() * Radius * Radius;
	_ ->
	    0
    end.

%% More pattern matching, value is second element of any list
getSecond([]) ->
    fred; % not a keyword, just an atom we made up
getSecond([_]) ->
    george;
getSecond([_, B | _]) ->
    B.


%% Pattern match is trivial, uses guard to select letter grade
grade(N) when N >= 90 ->
    "A";
grade(N) when N >= 80 ->
    "B";
grade(_) -> 
    "F".

%% Same as above using "if" expression
grade2(N) ->
    if N >= 90 ->
	    "A";
       N >= 80 ->
	    "B";
       true  -> "F"
    end.


%% Tail recursion to do iteration
countDown(0) ->
    ok;
countDown(N) ->
    io:format("~p~n", [N]),
    countDown(N - 1).

%% Not tail recursive, builds up call stack
countUpNonTail(0) ->
    ok;
countUpNonTail(N) ->
    countUpNonTail(N - 1),
    if N rem 1000 == 0 ->
	    io:format("~p~n", [N]);
       true  -> 
	    ok
    end.
    
%% Uses tail recursive helper function countUp/2
countUp(N) ->
    countUp(N, N).

countUp(0, _) ->
    true;
countUp(N, Max) ->
    io:format("~p~n", [Max - N]),
    countUp(N - 1, Max).

    
%% Uses tail recursive helper function countUp/2
countUp2(N) ->
    countUp2(0, N).

countUp2(_, 0) ->
    true;
countUp2(N, Max) ->
    io:format("~p~n", [N]),
    countUp2(N + 1, Max - 1).


%% Factorial, not tail recursive
fac(1) -> 1;
fac(N) -> N * fac(N - 1).

 
%% Uses tail recursive helper function tailFac/2
tailFac(N) ->
    tailFac(N, 1).

tailFac(1, Acc) ->
    Acc;
tailFac(N, Acc) ->
    tailFac(N - 1, N * Acc).

%% Sum of list elements, not tail recursive
listSum([]) ->
    0;
listSum([H|T]) ->
    H + listSum(T).

%% Sum of list elements, tail recursive

tailListSum(L) ->
    tailListSum(L, 0).

tailListSum([], Acc) ->
    Acc;
tailListSum([H|T], Acc) ->
    tailListSum(T, Acc + H).

%% Creates a list of consecutive numbers
createSeq(Max) ->
    createSeq(Max, []).

createSeq(0, Seq) ->
    [0|Seq];
createSeq(Max, Seq) ->
    createSeq(Max - 1, [Max|Seq]).