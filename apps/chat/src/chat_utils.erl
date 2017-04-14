%%%-------------------------------------------------------------------
%% @doc chat utils
%% @end
%%%-------------------------------------------------------------------
-module(chat_utils).
-export ([encode_message/2,
          decode_message/1,
          message/2]).

%% @doc encode message to json format
encode_message(Type, Message) ->
    jsonx:encode([{type, Type}, {data, Message}]).

%% @doc decode message from json format
decode_message(Data) ->
    {DecodedData} = jsonx:decode(Data),
    Type = proplists:get_value(<<"type">>, DecodedData),
    Message = proplists:get_value(<<"data">>, DecodedData),
    {Type, Message}. 

%% @doc make message for sening to client, format: [Timestamp] Username> Message
message(Username, Message) ->
    Timestamp = list_to_binary(timestamp()),
    <<  <<"[">>/binary, 
        Timestamp/binary, 
        <<"] ">>/binary, 
        Username/binary, 
        <<"> ">>/binary, 
        Message/binary
    >>.

%% @doc get current datetime
timestamp() ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]).