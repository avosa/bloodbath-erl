%%%-------------------------------------------------------------------
%% @doc bloodbath public API
%% @end
%%%-------------------------------------------------------------------

-module(bloodbath_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    bloodbath_sup:start_link().

stop(_State) ->
    ok.
