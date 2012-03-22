-module(ts_myclient).

-include("ts_profile.hrl").
-include("ts_myclient.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/2,
         session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0]).


%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, ack_type = parse|no_ack|local, persistent = true|false} 
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
	[].

%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#myclient_request{type=echo, data=Data},#state_rcv{session=S}) ->
    % for some reason <<?ECHO:8, Data>> does not work
    {list_to_binary(binary_to_list(<<?ECHO:8>>) ++ Data),S};
get_message(#myclient_request{type=compute, arith=sub, data=Data},#state_rcv{session=S}) ->
    [A, B] = Data,
    {<<?SUB:8, A:8, B:8>>,S};
get_message(#myclient_request{type=compute, arith=add, data=Data},#state_rcv{session=S}) ->
    [A, B] = Data,
    {<<?ADD:8, A:8, B:8>>,S}.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:	Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize=0}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize= 0}) ->
    parse(Data, State#state_rcv{datasize= size(Data)});
%% we don't actually do anything
parse(Data, State=#state_rcv{acc = [], dyndata=DynData}) ->
    ?LOGF("~p:parse(~p, #state_rcv{acc=[], dyndata=~p})~n",[?MODULE, Data, DynData],?NOTICE),
    case size(Data) of
	1 ->
	    ts_mon:add({count, one_byte});
	_ ->
	    ts_mon:add({count, multi_bytes})
    end,
    {State#state_rcv{ack_done = false},[],false};
%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc=Acc, datasize=DataSize}) ->
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary,Data/binary >>, State#state_rcv{acc=[], datasize=NewSize}).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
	ts_config_myclient:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #myclient_request
%%----------------------------------------------------------------------
add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#myclient_request{}.

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
	#dyndata{proto=#myclient_dyndata{}}.



