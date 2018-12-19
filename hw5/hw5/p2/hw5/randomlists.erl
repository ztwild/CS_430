%% 2b: Provides two ways to create Erlang lists of random integers, one
%% by prepending with cons and one by appending. Also provides a (naive)
%% comparison of these functions' relative execution times.

-module(randomlists).
-export([make/2, try_it/0, try_it/2, prepend/2, append/2]).

%% @doc Builds a list of randomly generated integers in {1..UB} using the
%% preferred method (either prepend/2 or append/2).
make(Len, Ub) when Len >= 0, Ub >= 1 ->
  prepend(Len, Ub).

%% @doc Builds a list of randomly generated integers in {1..Ub} using cons.
prepend(Len, Ub) ->
  prepend(Len, Ub, []).

prepend(0, _, List) ->
  List;
prepend(Len, Ub, List) ->
  Random = randint(Ub),
  prepend(Len - 1, Ub, [Random | List]).


%% @doc Builds a list of randomly generated integers in {1..Ub} using append.
append(1, Ub) ->
  Random = randint(Ub),
  [ Random ];
append(Len, Ub) -> 
  Random = randint(Ub),
  [ Random | append(Len - 1, Ub) ].



%% @doc Generate a random integer in the set {1..Ub}.
randint(Ub) when Ub >= 1 -> rand:uniform(Ub).

try_it() -> try_it(1000000, 10).

try_it(Len, Ub) when Len >= 0, Ub >= 1 ->
  io:fwrite("timeit(Len=~w, Ub=~w)~n", [Len, Ub]),
  {_, ElapsedAppend } = stopwatch:time_it(fun() -> append(Len, Ub) end),
  {_, ElapsedPrepend } = stopwatch:time_it(fun() -> prepend(Len, Ub) end),
  io:fwrite("Elapsed Time Appending: ~wμs~n", [ElapsedAppend]),
  io:fwrite("Elapsed Time Prepending: ~wμs~n", [ElapsedPrepend]),
  io:fwrite("Ratio of Elapsed Appending to Elapsed Prepending: ~w~n",
            [ElapsedAppend / ElapsedPrepend]).
