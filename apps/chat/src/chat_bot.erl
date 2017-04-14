%%%-------------------------------------------------------------------
%% @doc chat bot
%% @end
%%%-------------------------------------------------------------------
-module(chat_bot).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {timer}).

-define(TIMER_FACTOR, 1000).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% Chat bot callbacks
%%====================================================================

init(_) ->
    gproc:reg({p,l,room}),
    rand:seed(exs64),
    TimerRef = erlang:send_after(timeout(), self(), timeout),
    {ok, #state{timer = TimerRef}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{timer=TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    Message = chat_utils:encode_message(message, chat_utils:message(<<"ChatBot">>, <<"Hi! I'm bot.">>)),
    gproc:send({p,l,room}, Message),
    NewTimerRef = erlang:send_after(timeout(), self(), timeout),
    {noreply, State#state{timer=NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc make random values for timeout
timeout() ->
    RandomNumber = rand:uniform(100),
    ?TIMER_FACTOR*RandomNumber.

