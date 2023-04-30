-module(bloodbath_threading).
-export([run_async/1]).

run_async(Fun) ->
    spawn(Fun).
