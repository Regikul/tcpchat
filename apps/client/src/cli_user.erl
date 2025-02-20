-module(cli_user).

-behaviour(gen_server).

-include_lib("common/include/common.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/4]).
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([send_message/2]).

send_message(Server, Message) ->
    gen_server:cast(Server, {send_message, Message}).

-record(state, {socket, protocol, login}).

start_link(Host, Port, ?NE_BINARY_PAT = Login, ?NE_BINARY_PAT = Password) ->
    gen_server:start_link(?MODULE, [{Host, Port, Login, Password}], []).

init([{Host, Port, Login, Password}]) ->
    logger:set_process_metadata(#{token => "chat_user:" ++ binary_to_list(Login)}),
    {ok, {}, {continue, {Host, Port, Login, Password}}}.

handle_continue({Host, Port, Login, Password}, {}) ->
    case gen_tcp:connect(Host, Port, [binary, {active, once}]) of
        {ok, Socket} ->
            AuthPacket = protocol:encode(#auth{login = Login, passw = Password}),
            ok = gen_tcp:send(Socket, AuthPacket),
            {noreply, #state{socket = Socket, protocol = protocol:new(), login = Login}};
        {error, Reason} ->
            {stop, {error, Reason}, {Host, Port, Login, Password}}
        end.

handle_info({tcp, Socket, Data}, #state{socket = Socket, protocol = Protocol} = State) ->
    inet:setopts(Socket, [{active, once}]),
    case protocol:decode_all(Protocol, Data) of
        {ok, {NewProtocol, Packets}} -> lists:foldl(fun handle_packet/2, State#state{protocol = NewProtocol}, Packets);
        {error, Reason} -> {stop, {protocol_failure, Reason}, State}
    end;
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    ?LOG_ERROR("client goes down due to ~p", [{error, Reason}]),
    {stop, Reason, State};
handle_info(Info, #state{} = State) ->
    ?LOG_ERROR("unknown info ~p", Info),
    {noreply, State}.

handle_packet(#auth_sucess{}, #state{} = State) ->
    {noreply, State};
handle_packet(#auth_error{status = Reason}, #state{} = State) ->
    {stop, {auth_error, Reason}, State};
handle_packet(#message{from = From, txt = Text}, #state{} = State) ->
    ?LOG_INFO("~s: ~s", [From, Text]),
    {noreply, State}.

handle_call(Request, From, #state{} = State) ->
    ?LOG_ERROR("~p sends unknown request ~p", [From, Request]),
    {reply, unimpl, State}.

handle_cast({send_message, Message}, #state{login = Login, socket = Socket} = State) ->
    MessagePacket = protocol:encode(#message{txt = Message}),
    case gen_tcp:send(Socket, MessagePacket) of
        ok ->
            ?LOG_INFO("~s: ~s", [Login, Message]),
            {noreply, State};
        {error, Reason} = Error ->
            ?LOG_ERROR("can not send message due to ~p", [Error]),
            {stop, {error, Reason}, State}
        end;
handle_cast(Request, #state{} = State) ->
    ?LOG_ERROR("unknown request ~p", [Request]),
    {noreply, State}.