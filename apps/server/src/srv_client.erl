-module(srv_client).

-behaviour(gen_server).

-define(GRACESHUTDOWN, timer:minutes(30)).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {user, connection, monitor}).

start_link(User, ConnectionPid) ->
    gen_server:start_link(?MODULE, [{User, ConnectionPid}], []).

init({User, Connection}) ->
    Monitor = erlang:monitor(process, Connection),
    {ok, #state{user = User, connection = Connection, monitor = Monitor}}.

handle_call(_Request, _From, #state{} = State) ->
    {noreply, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info(_Info, #state{} = State) ->
    {noreply, State}.
