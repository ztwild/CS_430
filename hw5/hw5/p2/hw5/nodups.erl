%%% 2c: Implements de-duplication of list elements using Erlang's `foldl/3`.
-module(nodups).
-export([nodups/2, nodups/1, try_it/0]).

% TODO: Implement `nodups/2` as specified.
% nodups(X, []) ->
%   io:format("1) ~p & ~p\n", [X, []]),
%   if is_list(X) ->
%     X;
%   true ->
%     [X]
%   end;
% nodups([], [ A | B ]) -> 
%   io:format("2) ~p & ~p\n", [[], [A|B]]),
%   nodups([A], B);
% nodups([ X | Y ], [ A | B ]) ->
%   io:format("3) ~p & ~p\n", [[X|Y], [A|B]]),
  
%   if X == A ->
%     nodups([ X | Y ], B);
%     true ->
%     [ X | nodups(Y, [ A | B ])]
%     end.

nodups(X, []) ->
  % io:format("1) ~p & ~p\n", [X, []]),
  [X];
nodups(X, [ A | B ]) -> 
  % io:format("2) ~p & ~p\n", [X, [A|B]]),
  if X == A ->
    nodups(X, B);
  true ->
    [ A | nodups(X, B)]
  end.



% TODO: Once `nodups/2` is implemented, uncomment the following functions to
% try it out.

nodups(Xs) -> 
  lists:reverse(lists:foldl(fun nodups/2, [], Xs)).

try_it() ->
 nodups(lists:sort(randomlists:prepend(10, 100))).
