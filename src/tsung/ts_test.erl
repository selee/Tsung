-module(ts_test).

-include("ts_profile.hrl").
-include("ts_test.hrl").

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
%% Returns: ok, ack_type = parse|no_ack|local, persistent = true|false} 
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


modify_packet(Data) -> 
		T = time(),
		lists:sublist(Data, lists:len(Data)-2)++[T / 256,T rem 256].

get_timestamps(Data) -> 
		T= time(), 
		[O2, O1|_] = lists:reverse(Data), 
		O= O1*256 + O2, 
		[T,O,O-T].


%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#test_request{data=Data},#state_rcv{session=S}) ->
    {list_to_binary(modify_packet(hd(Data))),S}.

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
 %   ?LOGF("~p:parse(~p, #state_rcv{acc=[], dyndata=~p})~n",[?MODULE, Data, DynData],?NOTICE),
		?LOGF("~p,~p,~p,~n",get_timestamps(binary_to_list(Data)),?NOTICE),
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
	ts_config_test:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #test_request
%%----------------------------------------------------------------------
add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#test_request{}.

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
	#dyndata{proto=#test_dyndata{}}.



