%%% 2a: A tail-recursive Erlang fn that computes the Nth Fibonacci number.
-module(fib).
-export([tailFib/1, tailFib/4, fibonacci/1]).

fibonacci(N) ->
  if N == 0 -> 0;
    N == 1 -> 1;
    N > 1 -> fibonacci(N - 1) + fibonacci(N - 2)
  end.

tailFib(N)->
  tailFib(0, 1, 0, N).

tailFib(A, B, C, N)->
  if C < N ->
    tailFib(B, A + B, C + 1, N);
  true -> B
  end.

