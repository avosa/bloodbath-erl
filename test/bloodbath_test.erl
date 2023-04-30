-module(bloodbath_test).
-include_lib("eunit/include/eunit.hrl").
-export([test_schedule/0, test_list/0, test_find/0, test_cancel/0]).

% Replace with your actual API key
-define(API_KEY, "your_api_key_here").

test_schedule() ->
    ?_test(begin
        bloodbath:start(),
        bloodbath_configuration:set_api_key(?API_KEY),
        Event = #{<<"title">> => <<"Test Event">>},
        {ok, Result} = bloodbath_event:schedule(Event),
        ?assertEqual(<<"Test Event">>, maps:get(<<"title">>, Result))
    end).

test_list() ->
    ?_test(begin
        bloodbath:start(),
        bloodbath_configuration:set_api_key(?API_KEY),
        {ok, Result} = bloodbath_event:list(),
        ?assert(is_list(Result))
    end).

test_find() ->
    ?_test(begin
        bloodbath:start(),
        bloodbath_configuration:set_api_key(?API_KEY),
        % You'll need to replace the event ID with a valid one
        EventId = "valid_event_id_here",
        {ok, Result} = bloodbath_event:find(EventId),
        ?assertEqual(EventId, maps:get(<<"id">>, Result))
    end).

test_cancel() ->
    ?_test(begin
        bloodbath:start(),
        bloodbath_configuration:set_api_key(?API_KEY),
        % You'll need to replace the event ID with a valid one
        EventId = "valid_event_id_here",
        {ok, Result} = bloodbath_event:cancel(EventId),
        ?assertEqual(EventId, maps:get(<<"id">>, Result))
    end).
