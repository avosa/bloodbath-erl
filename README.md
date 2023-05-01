# bloodbath

The Bloodbath Erlang library provides convenient access to the Bloodbath API from applications written in the Erlang and Elixir language. This library will eventually be moved [here](https://github.com/bloodbath-io).

## Project Structure

```erlang
bloodbath_erl/
  ├── src/
  │   ├── bloodbath.erl
  │   ├── bloodbath_configuration.erl
  │   ├── bloodbath_event.erl
  │   ├── bloodbath_rest_adapter.erl
  │   ├── bloodbath_threading.erl
  │   └── bloodbath_verbose.erl
  ├── test/
  │   └── bloodbath_test.erl
  ├── rebar.config
  └── README.md
```

## Installation

Bloodbath can be installed as a dependency in your Erlang project by adding the following to your rebar.config file:

```erlang
{deps, [
    {bloodbath, "1.0.0"}
]}.
```
After updating your dependencies, you can run `rebar3 compile` or `mix deps.get` to download and compile the dependencies.

## Usage

Once you have added Bloodbath to your dependencies, you can use it in your application as follows:

```erlang
-module(myapp).
-compile(export_all).

-include_lib("bloodbath/include/bloodbath.hrl").

schedule_event() ->
    ok = application:start(bloodbath),
    Event = #{<<"scheduled_for">> => calendar:universal_time() + {0, 0, 1, 0, 0, 0},
              <<"headers">> => #{},
              <<"method">> => <<"post">>,
              <<"body">> => <<"some body content">>,
              <<"endpoint">> => <<"https://api.acme.com/path">>},
    {ok, Result} = bloodbath_event:schedule(Event),
    io:format("Scheduled event: ~p~n", [Result]).

list_events() ->
    ok = application:start(bloodbath),
    {ok, Result} = bloodbath_event:list(),
    io:format("List of events: ~p~n", [Result]).

find_event(EventId) ->
    ok = application:start(bloodbath),
    {ok, Result} = bloodbath_event:find(EventId),
    io:format("Found event with ID ~p: ~p~n", [EventId, Result]).

cancel_event(EventId) ->
    ok = application:start(bloodbath),
    {ok, Result} = bloodbath_event:cancel(EventId),
    io:format("Cancelled event with ID ~p: ~p~n", [EventId, Result]).
```

You can then use the functions in your application as follows:

```erlang
myapp:schedule_event(),
myapp:list_events(),
myapp:find_event("b7ccff..."),
myapp:cancel_event("b7ccff...").
```

Make sure to replace "b7ccff..." with the actual ID of an event you want to find or cancel.

# Conclusion

Bloodbath is a powerful library that simplifies event scheduling and management. This README should have given you an idea of how to use it in your Erlang application. For more information, see the [official documentation](https://docs.bloodbath.io/).
