-module(srv_auth_db).

-behaviour(gen_server).

-include_lib("common/include/common.hrl").

%%% gen_server callbacks
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-export([register/2, authenticate/1]).

-define(SERVER, ?MODULE).
-define(TAB, auth_db).

-record(state, {tab}).

register(Login, Password) ->
  gen_server:cast(?SERVER, {register, #auth{login = Login, passw = Password}}).

authenticate(#auth{login = Login, passw = Password}) ->
  case ets:lookup(?TAB, Login) of
    [#auth{passw = Password}] -> ok;
    [] -> {error, unknown}
  end.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init(_) ->
  ?LOG_INFO("Starting auth_db"),
  Tab = ets:new(?TAB, [named_table, set, protected, {keypos, #auth.login}]),
  lists:foreach(fun (#auth{} = Auth) -> ets:insert(?TAB, Auth) end, [
    #auth{login = <<"alice">>, passw = <<"alice">>},
    #auth{login = <<"bob">>, passw = <<"bob">>}
  ]),
  {ok, #state{tab = Tab}}.

handle_call(Request, From, #state{} = State) ->
  ?LOG_ERROR("~p sends unknown call: ~p", [From, Request]),
  {reply, unimpl, State}.

handle_cast({register, #auth{login = ?NE_BINARY_PAT, passw = ?NE_BINARY_PAT} = Auth}, #state{tab = Tab} = State) ->
  ets:insert(Tab, Auth),
  {noreply, State};
handle_cast(Request, #state{} = State) ->
  ?LOG_ERROR("unknown cast: ~p", [Request]),
  {noreply, State}.

handle_info(Info, #state{} = State) ->
  ?LOG_ERROR("unknown info: ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
