-module(hello).
-export([helloworld/0, helloworld/1, helloworld/2]).

%% Define two different functions helloworld
helloworld() ->
    io:format("Hello, World~n"),
    io:format("Erlang is wacky!~n"),
    42.
 
helloworld(X, Y) ->
    io:format("~5.2f plus ~5.2f is ~5.2f~n", [X, Y, X + Y]).

helloworld(Name) ->
    io:format("Hello, " ++ Name ++ "~n").