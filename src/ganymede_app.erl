-module(ganymede_app).
-behavior(application).
-export([start/2, stop/1]).

-author('Vladimir Zaytsev <vladimir.zaytsev.m@gmail.com>').

start(_Type, StartArgs) ->
    io:format("Ganymede started...~n", []),
    ganymede_sup:start_link().
stop(_State) ->
    ok. 



