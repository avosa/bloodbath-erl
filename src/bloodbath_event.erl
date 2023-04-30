-module(bloodbath_event).
-export([schedule/1, list/0, find/1, cancel/1]).

schedule(Args) ->
    bloodbath_rest_adapter:perform(post, "/events", Args, bloodbath:get_config()).

list() ->
    bloodbath_rest_adapter:perform(get, "/events", undefined, bloodbath:get_config()).

find(Id) ->
    bloodbath_rest_adapter:perform(get, "/events/" ++ Id, undefined, bloodbath:get_config()).

cancel(Id) ->
    bloodbath_rest_adapter:perform(delete, "/events/" ++ Id, undefined, bloodbath:get_config()).
