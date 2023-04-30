-module(bloodbath_configuration).
-export([new/0, get_api_key/1, set_api_key/2, get_api_base/1, set_api_base/2, get_verbose/1, set_verbose/2]).

-record(state, {api_key, api_base, verbose}).

new() ->
    #state{api_key = undefined, api_base = "https://api.bloodbath.io/rest", verbose = false}.

get_api_key(#state{api_key = ApiKey}) ->
    ApiKey.

set_api_key(Config, ApiKey) ->
    Config#state{api_key = ApiKey}.

get_api_base(#state{api_base = ApiBase}) ->
    ApiBase.

set_api_base(Config, ApiBase) ->
    Config#state{api_base = ApiBase}.

get_verbose(#state{verbose = Verbose}) ->
    Verbose.

set_verbose(Config, Verbose) ->
    Config#state{verbose = Verbose}.
