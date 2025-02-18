-module(srv_listener).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("common/include/common.hrl").

-record(state, {sock, transport, protocol}).

-export([start_link/3, init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Ref, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Transport, Opts], []).

init([Ref, Transport, _Opts]) ->
    {ok, [], {continue, {handshake, Ref, Transport}}}.

handle_continue({handshake, Ref, Transport}, []) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}, binary]),
    State = #state{sock = Socket, transport = Transport, protocol = protocol:new()},
    logger:set_process_metadata(#{token => peername(State)}),
    {noreply, State}.

handle_call(_Request, _From, #state{} = State) ->
    {reply, unimlp, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_method(#auth{login = Login} = Auth, #state{} = State) ->
    maybe
      {_, ok}           ?= {auth_check,  srv_auth_db:authenticate(Auth)},
      {_, ok}           ?= {connection_check, srv_session_db:try_write_session(Login, self())},
      send(State, protocol:encode(#auth_sucess{}))
    else
        {auth_check, _} -> send(State, #auth_error{status = <<"auth_error">>});
        {connection_check, _} -> send(State, #auth_error{status = <<"already_connected">>})
    end,
    State;
handle_method(#message{} = Message, #state{} = State) ->
    Connections = srv_session_db:query_connections(),
    lists:foreach(fun (C) -> C ! {send, Message} end, Connections),
    State.

handle_info({tcp, Socket, Data}, #state{sock = Socket, protocol = Protocol} = State) ->
    ?LOG_DEBUG("inbound data: ~p", [Data]),
    NextState = case protocol:decode_all(Protocol, Data) of
        {ok, {NewProtocol, Packets}} ->
            ?LOG_INFO("inbound packets: ~p", [Packets]),
            lists:foldl(fun handle_method/2, State#state{protocol = NewProtocol}, Packets);
        {error, {Reason, Packets}} ->
            ?LOG_INFO("can not parse due to ~p, got only ~p", [{error, Reason}, Packets]),
            State#state{protocol = protocol:new()}
    end,
    setopts(NextState, [{active, once}]),
    {noreply, NextState};
handle_info({tcp_closed, Socket}, #state{sock = Socket} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, #state{sock = Socket} = State) ->
    {stop, Reason, State};
handle_info(_Info, #state{} = State) ->
    {noreply, State}.

setopts(#state{sock = Socket, transport = Transport}, Opts) ->
    Transport:setopts(Socket, Opts).

peername(#state{sock = Socket, transport = Transport}) ->
    {ok, {Addr, Port}} = Transport:peername(Socket),
    Symbolic = lists:map(fun integer_to_list/1, tuple_to_list(Addr)),
    lists:flatten([
        lists:join($., Symbolic), ":", integer_to_list(Port)
    ]).

send(#state{sock = Socket, transport = Transport}, Buffer) ->
    Transport:send(Socket, Buffer).
