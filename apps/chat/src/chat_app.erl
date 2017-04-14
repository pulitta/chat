%%%-------------------------------------------------------------------
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, chat, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, chat, "static"}},
            {"/websocket", websocket_handler, []}
        ]}
    ]),
    Port = case application:get_env(chat, port) of
        {ok, ConfigPort} -> ConfigPort;
        _ -> 8080
    end,
    {ok, _} = cowboy:start_http(http, 10, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]),
    chat_sup:start_link().

stop(_State) ->
    ok.
