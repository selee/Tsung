%%%-------------------------------------------------------------------
%%% File    : myserver.erl
%%% Author  : tty <tty.erlang@gmail.com>
%%% Description : 
%%%   Simple echo and arithmatic server. Listens on
%%%   port 5678. Must call myserver:start_link/0 followed by
%%%   myserver:server to start listening socket.
%%% Usage : erl -s myserver start_link
%%%         1> myserver:server().
%%% Created : 11 Mar 2007 by tty <tty.erlang@gmail.com>
%%%-------------------------------------------------------------------
-module(myserver).

-behaviour(gen_server).

-define(ECHO, 0).
-define(ADD, 1).
-define(SUB, 2).

%% API
-export([start_link/0, server/0, stop/0, accept/1, test/1, test/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: server() -> ok
%% Description: Starts listening to port 5678
%%--------------------------------------------------------------------
server() ->
    gen_server:cast(?MODULE, server).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%% Description: Closes listening port and stops myserver
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: test(sub|add, Arg1, Arg2)
%% Description: Calls myserver for add/sub operation.
%%--------------------------------------------------------------------
test(sub, Arg1, Arg2) ->
    {ok, Sock} = 
	gen_tcp:connect("localhost",
			5678,
			[binary, 
			 {packet, 0},
			 {active, true}]),
    ok = gen_tcp:send(Sock, binary_to_list(<<?SUB:8, Arg1, Arg2>>)),
    receive
	{tcp, Sock, Msg0} ->
	    <<R>> = Msg0,
	    error_logger:info_msg("client some msgs ~p~n", [R])
    after 30000 ->
	    error_logger:info_msg("client timeout~n")
    end,
    ok = gen_tcp:close(Sock);
test(add, Arg1, Arg2) ->
    {ok, Sock} = 
	gen_tcp:connect("localhost",
			5678,
			[binary, 
			 {packet, 0},
			 {active, true}]),
    ok = gen_tcp:send(Sock, binary_to_list(<<?ADD:8, Arg1, Arg2>>)),
    receive
	{tcp, Sock, Msg0} ->
	    <<R>> = Msg0,
	    error_logger:info_msg("client some msgs ~p~n", [R])
    after 30000 ->
	    error_logger:info_msg("client timeout~n")
    end,
    ok = gen_tcp:close(Sock).

%%--------------------------------------------------------------------
%% Function: test(Msg)
%% Description: Calls myserver echo service.
%%--------------------------------------------------------------------
test(Msg) ->
    {ok, Sock} = 
	gen_tcp:connect("localhost",
			5678,
			[binary, 
			 {packet, 0},
			 {active, true}]),
    ok = gen_tcp:send(Sock, binary_to_list(<<?ECHO:8>>) ++ Msg),
    receive
	{tcp, Sock, Msg0} ->
	    error_logger:info_msg("client some msgs ~p~n", [Msg0])
    after 30000 ->
	    error_logger:info_msg("client timeout~n")
    end,
    ok = gen_tcp:close(Sock).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, 
                                        {active, true}]),
    {ok, #state{sock = LSock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(server, #state{sock = LSock} = State) ->
    Reply = case gen_tcp:accept(LSock, 1000) of
		{ok, Sock} ->
		    error_logger:info_msg("Server accepted socket~n"),
		    Pid = proc_lib:spawn(?MODULE, accept, [Sock]),
		    ok = gen_tcp:controlling_process(Sock, Pid),
		    ?MODULE:server(),
		    {noreply, State};
		{error, timeout} ->
		    ?MODULE:server(),
		    {noreply, State};
		{error, closed} ->
		    {stop, normal, State}
	    end,
    Reply;
handle_cast(_Msg, State) ->
    {noreply, State}.

process_message(Sock, <<?SUB:8, Msg/binary>>) -> 
    <<A:8,B:8>> = Msg,
    gen_tcp:send(Sock, [A-B]);
process_message(Sock, <<?ADD:8, Msg/binary>>) -> 
    <<A:8,B:8>> = Msg,
    gen_tcp:send(Sock, [A+B]);
process_message(Sock, <<?ECHO:8, Msg/binary>>) -> 
    gen_tcp:send(Sock, binary_to_list(Msg)).

accept(Sock) ->
    receive
	{tcp, Sock, Msg} ->
	    process_message(Sock, Msg),
	    ok = gen_tcp:close(Sock);
	 E ->
	    error_logger:info_msg("Server Got unknown msg ~p~n", [E]),
	    ok = gen_tcp:close(Sock)
    after 30000 ->
	    error_logger:info_msg("Server socket timeout~n"),
	    ok = gen_tcp:close(Sock)
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{sock = Sock}) ->
    gen_tcp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
