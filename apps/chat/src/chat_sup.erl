%%%-------------------------------------------------------------------
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Procs = [{chat_bot,
                {chat_bot, start_link,
                 [[{chat_bot, []}]]},
                permanent, 5000, worker, [chat_bot]
       }],
    {ok, {{one_for_one, 10, 10}, Procs}}.

