-module(bloodbath).
-export([start/0, init/1, get_config/0]).

start() ->
    {ok, _Pid} = application:start(bloodbath).

init([]) ->
    {ok, bloodbath_configuration:new()}.

get_config() ->
    bloodbath_configuration:get_config().
