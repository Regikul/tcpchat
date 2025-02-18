-module(srv_client_sup).

-behaivour(supervisor).

-include_lib("common/include/common.hrl").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        ?WORKER(srv_client, [])
    ],
    {ok, {SupFlags, ChildSpecs}}.
