-module(bloodbath_verbose).
-export([capture/2]).

capture(Label, Fun) ->
    case bloodbath_configuration:get_verbose() of
        true ->
            Result = Fun(),
            io:format("~s [VERBOSE] ~s: ~p~n", [colored("===", blue), colored(Label, blue), colored(Result, red)]);
        false ->
            Fun()
    end.

colored(Text, Color) ->
    case Color of
        red -> io_lib:format("\033[31m~s\033[0m", [Text]);
        green -> io_lib:format("\033[32m~s\033[0m", [Text]);
        blue -> io_lib:format("\033[34m~s\033[0m", [Text]);
        _ -> Text
    end.
