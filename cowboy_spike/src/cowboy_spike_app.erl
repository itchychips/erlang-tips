-module(cowboy_spike_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),
    cowboy_spike_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
