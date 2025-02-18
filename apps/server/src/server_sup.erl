%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-include_lib("common/include/common.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        ?WORKER(srv_auth_db, []),
        ?WORKER(srv_session_db, []),
        ranch:child_spec(chat_listener_pool, ranch_tcp, #{socket_opts => [{port, 5555}]}, srv_listener, [])
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
