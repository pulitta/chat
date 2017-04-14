%%%-------------------------------------------------------------------
%% @doc websocket handler.
%% @end
%%%-------------------------------------------------------------------

-module(websocket_handler).
                                
-behaviour(cowboy_websocket_handler).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, 
        {username = undefined :: binary()}).

%%====================================================================
%% Websocket handler callbacks
%%====================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    gproc:reg({p,l,room}),
    {ok, Req, #state{}}.

websocket_handle({text, Text}, Req, #state{username=CurrentUsername} = State) ->
    NewState = case chat_utils:decode_message(Text) of
        {<<"username">>, <<"">>} ->
            Message = chat_utils:encode_message(error, <<"Username is empty">>),
            gproc:send(self(),Message),
            State;
        {<<"username">>, Username} ->
            case lookup_username(Username) of
                true ->
                    Message = chat_utils:encode_message(error, <<"Username already exists">>),
                    gproc:send(self(),Message),
                    State;
                _ ->
                    gproc:set_value({p,l,room}, Username),
                    Message = chat_utils:encode_message(status, ok),
                    gproc:send(self(), Message),
                    State#state{username=Username}
            end;
        {<<"message">>, Message} when (Message =/= <<"">>) and (CurrentUsername =/= undefined) ->
            FullMessage = chat_utils:message(CurrentUsername, Message),
            FullEncodedMessage = chat_utils:encode_message(message, FullMessage),
            gproc:send({p,l,room}, FullEncodedMessage),
            State;
        _ ->
            State
    end,
    {ok, Req, NewState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Msg, Req, State) ->
    {reply, {text, Msg}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc lookup username in values of registered processes.
lookup_username(Username) ->
    RegUsernames = gproc:lookup_values({p,l,room}),
    do_lookup_username(Username, RegUsernames).

do_lookup_username(_, []) -> false;
do_lookup_username(Username, [{_,Username}|_]) -> true;
do_lookup_username(Username, [_|Other]) -> do_lookup_username(Username, Other).