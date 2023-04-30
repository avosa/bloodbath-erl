-module(bloodbath_rest_adapter).
-record(state, {method, endpoint, body, options, config, api_key, api_base}).
-export([perform/4]).

perform(Method, Endpoint, Body, Config) ->
    ApiKey = bloodbath_configuration:get_api_key(Config),
    ApiBase = bloodbath_configuration:get_api_base(Config),
    Url = ApiBase ++ Endpoint,
    ContentType = "application/json",
    Headers = [{"Authorization", "Bearer " ++ ApiKey}, {"Content-Type", ContentType}],
    Options = [],
    case Method of
        get ->
            httpc:request(get, {Url, Headers}, Options, [{body_format, binary}]);
        post ->
            httpc:request(post, {Url, Headers, ContentType, jsx:encode(Body)}, Options, [{body_format, binary}]);
        delete ->
            httpc:request(delete, {Url, Headers}, Options, [{body_format, binary}]);
        _ ->
            {error, unsupported_method}
    end.
