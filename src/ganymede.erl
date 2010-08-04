-module(ganymede).
% 
% -export([start/0, start/1, stop/0]).
% -export([ensure_started/1]).
% 
% ensure_started(App) ->
%     case application:start(App) of
%   ok ->
%       ok;
%   {error, {already_started, App}} ->
%       ok
%     end.
% 
% %% @spec start() -> ok
% %% @doc Start the gnymede server.
% start() -> start([]).
%   
% %% @spec start(_Args) -> ok
% %% @doc Start the gnymede server.
% start(_Args) ->
%     ensure_started(crypto),
%     %ensure_started(nitrogen),
%     application:start(ganymede).
% 
% %% @spec stop() -> ok
% %% @doc Stop the ganymede server.
% stop() ->
%     Res = application:stop(ganymede),
%     application:stop(crypto),
%     application:stop(nitrogen),
%     Res.