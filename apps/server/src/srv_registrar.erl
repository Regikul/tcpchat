-module(srv_registrar).

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

-export([register/2, authorize/1]).

-define(SERVER, ?MODULE).

-record(state, {tab}).
-record(registrar, {
    login :: binary(),
    passw :: binary(),
    connection :: pid() | undefined
}).

register(Login, Password) ->
    gen_server:cast(?SERVER, {register, #auth{login = Login, passw = Password}}).

authorize(#auth{} = Auth) ->
    Self = self(),
    gen_server:call(?SERVER, {authz, Auth, Self}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init(_) ->
    ?LOG_INFO("Starting registrar"),
    Tab = ets:new(registrar, [named_table, set, protected, {keypos, #registrar.login}]),
    {ok, #state{tab = Tab}}.


handle_call({authz, #auth{login = Login, passw = Password}, Pid}, _From, #state{tab = Tab} = State) ->
  Reply = maybe
    [#registrar{passw = Password, connection = undefined} = Object] ?= ets:lookup(Tab, Login),
    ets:insert(Tab, Object#registrar{connection = Pid}), ok
  else
    [] -> {error, auth_error};
    [#registrar{passw = Passw}] when Passw =/= Password -> {error, auth_error};
    [#registrar{connection = Pid}] when is_pid(Pid) -> {error, already_connected}
  end,
  {reply, Reply, State};
handle_call(_Request, _From, #state{} = State) ->
  {reply, unimpl, State}.

handle_cast({register, #auth{login = Login, passw = Password}}, #state{tab = Tab} = State) ->
  ets:insert(Tab, #registrar{login = Login, passw = Password}),
  {noreply, State};
handle_cast(_Request, #state{} = State) ->
  {noreply, State}.

handle_info(_Info, #state{} = State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
