-module(srv_session_db).

-behaviour(gen_server).

-include_lib("common/include/common.hrl").

%%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([query_connections/0, try_write_session/2]).

-define(SERVER, ?MODULE).
-define(TAB, session_db).

-record(state, {tab}).
-record(session, {user, connection}).

try_write_session(Login, Pid) ->
    gen_server:call(?SERVER, {try_write_conn, Login, Pid}).

query_connections() ->
    Self = self(),
    ets:foldl(session_fold(Self), [], ?TAB).

session_fold(Exclude) ->
    fun (#session{connection = Pid}, Acc) ->
        case Pid of
            Exclude -> Acc;
            _Else -> [Pid | Acc]
        end
    end.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init(_) ->
    ?LOG_INFO("Starting session_db"),
    Tab = ets:new(?TAB, [named_table, set, protected, {keypos, #session.user}]),
    {ok, #state{tab = Tab}}.

handle_call({try_write_conn, User, Connection}, _From, #state{tab = Tab} = State) ->
    case ets:insert_new(Tab, #session{user = User, connection = Connection}) of
        true ->
            {reply, ok, State};
        false ->
            {reply, {error, already_connected}, State}
    end;
handle_call(Request, From, #state{} = State) ->
    ?LOG_ERROR("~p sends unknown call: ~p", [From, Request]),
    {reply, unimpl, State}.

handle_cast(Request, #state{} = State) ->
    ?LOG_ERROR("unknown cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, #state{} = State) ->
    ?LOG_ERROR("unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
