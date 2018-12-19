%% Problem 2d

-module(histogram).
-export([
   merge/2,
   make/2,
   make/3,
   make/4,
   worker/3,
   from_samples/1,
   try_it/0,
   try_it/3,
   count_samples/1,
   run/0
 ]).


%% @doc Calls make/3 with the given arguments while timing it. Once this call to
%% make/3 finishes, the following are printed to the terminal: the computed
%% histogram, the time elapsed to compute it, and the total number of samples.
try_it(NumSamples, UpperBound, NumActors) ->
  F = fun() -> make(NumSamples, UpperBound, NumActors) end,
  {Histogram, Elapsed} = stopwatch:time_it(F),
  io:fwrite("~w~n", [Histogram]),
  io:fwrite("Elapsed Time: ~wms~n", [Elapsed]),
  ok.

%% @doc Calls try_it/3 with some default values.
try_it() ->
  try_it(10000000, 10000, 4).


%% @doc Makes a histogram with the given number of samples and upper bound using
%% the given number of workers running concurrently. Essentially, this spawns
%% this number of worker actors and then merges their results as they arrive.
% make(_, _, 0) ->
%   [];
make(NumSamples, UpperBound, 1) ->
  % io:format("1) One Worker\n", []),
  spawn(histogram, worker, [NumSamples, UpperBound, self()]),
  receive
    {make, Histogram} ->
      H = Histogram
  end,
  H;
make(NumSamples, UpperBound, NumWorkers) ->
  S_Half = round(NumSamples / 2),
  W_Half = round(NumWorkers / 2),
  S_Other = NumSamples - S_Half,
  W_Other = NumWorkers - W_Half,
  % io:format("2) Before: ~p, After Half ~p, After Other ~p\n", [NumWorkers, W_Half, W_Other]),
  spawn(histogram, make, [S_Half, UpperBound, W_Half, self()]),
  spawn(histogram, make, [S_Other, UpperBound, W_Other, self()]),
  receive
    {make, Histogram1} ->
      H1 = Histogram1
  end,
  receive
    {make, Histogram2} ->
      H2 = Histogram2
  end,
  merge(H1, H2).


make(NumSamples, UpperBound, 1, Pid) ->
  % io:format("3) One Worker\n", []),
  spawn(histogram, worker, [NumSamples, UpperBound, self()]),
  receive
    {make, Histogram} ->
      H = Histogram
  end,
  Pid ! {make, H};
make(NumSamples, UpperBound, NumWorkers, Pid) ->
  S_Half = round(NumSamples / 2),
  W_Half = round(NumWorkers / 2),
  S_Other = NumSamples - S_Half,
  W_Other = NumWorkers - W_Half,
  % io:format("4) Before: ~p, After Half ~p, After Other ~p\n", [NumWorkers, W_Half, W_Other]),
  spawn(histogram, make, [S_Half, UpperBound, W_Half, self()]),
  spawn(histogram, make, [S_Other, UpperBound, W_Other, self()]),
  receive
    {make, Histogram1} ->
      H1 = Histogram1
  end,
  receive
    {make, Histogram2} ->
      H2 = Histogram2
  end,
  Merge = merge(H1, H2),
  Pid ! {make, Merge}.


%% @doc Makes a histogram.
% Make sure that you use `randomlists:make/2`.
make(NumSamples, UpperBound) ->
  List = randomlists:make(NumSamples, UpperBound),
  
  from_samples(List).


%% @doc A helper function which is meant to be spawned by make/3 as a process
%% to run make/2. It Calls make/2 with the given parameters and then sends the
%% results to ResultsReceiver.

worker(NumSamples, UpperBound, ResultsReceiver) ->
  io:format("In worker\n"),
  Histogram = make(NumSamples, UpperBound),
  % io:format("Histogram finished\n"),
  ResultsReceiver ! {make, Histogram}.
  % io:format("worker finished\n").


%% @doc Creates a histogram from a list of integer samples.
from_samples(Samples) ->
  Sort = lists:sort(Samples),
  Reverse = lists:reverse(Sort),
  from_samples([], [], Reverse).

from_samples(List, [], []) ->
  List;
from_samples(List, Tuple, []) ->
  [ Tuple | List ];
from_samples(List, [], [ H | T ]) ->
  Tuple = { H, 1 },
  from_samples(List, Tuple, T);
from_samples(List, Tuple, [ H | T ]) ->
  { Key, Value } = Tuple,
  if H == Key ->
    NewTuple = { Key, Value + 1 },
    from_samples(List, NewTuple, T);
  true ->
    NewTuple = { H, 1 },
    from_samples([ Tuple | List ], NewTuple, T)
  end.


%% @doc Merges two histograms together.

merge([], []) ->
  [];
merge(Xs, []) ->
  Xs;
merge([], Ys) ->
  Ys;
merge(Xs, Ys) ->
  [ Head_X | Tail_X ] = Xs,
  [ Head_Y | Tail_Y ] = Ys,
  { Key_X, Value_X } = Head_X,
  { Key_Y, Value_Y } = Head_Y,
  if Key_X == Key_Y ->
    Total = Value_X + Value_Y,
    [{Key_X, Total} | merge(Tail_X, Tail_Y)];
  Key_X < Key_Y ->
    [ Head_X | merge(Tail_X, [ Head_Y | Tail_Y ]) ];
  true ->
    [ Head_Y | merge([ Head_X | Tail_X ], Tail_Y ) ]
    end.


%% @doc Counts the total number of samples in the given histogram. This is just
%% the sum of all of the values in all of the 2-tuples.

count_samples([]) ->
  0;
count_samples(Histogram) ->
  [H | T] = Histogram,
  {_, Value} = H,
  count_samples(T, Value).

count_samples([], Count) ->
  Count;
count_samples(Histogram, Count) ->
  [H | T] = Histogram,
  {_, Value} = H,
  count_samples(T, Value + Count).


% Testing
run() ->
  % Histogram = make(5000000, 10000, 2),
  % count_samples(Histogram).

  % H2 = make(5000000, 10000, 1),
  % H3 = merge(H1, H2),
  % count_samples(H3).

  % make(10000000, 10000, 1),
  % io:format("Done\n"),
  % make(10000000, 10000, 4),
  % io:format("Done\n").

  % H = make(10000000, 10000, 2),
  % count_samples(H).

  L1 = randomlists:make(25, 15),
  S1 = from_samples(L1),
  L2 = randomlists:make(25, 15),
  S2 = from_samples(L2),
  merge(S1, S2).
  