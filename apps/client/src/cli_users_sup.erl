-module(cli_users_sup).

-behaviour(supervisor).

-include_lib("common/include/common.hrl").

-export([start_link/0, init/1]).
-export([start_user/2]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_) ->
    Host = os:getenv("TCPCHAT_HOST", "localhost"),
    Port = list_to_integer(os:getenv("TCPCHAT_PORT", "5555")),
    SupFlags = #{strategy => simple_one_for_one,
                    intensity => 10,
                    period => 1},
    ChildSpecs = [
        ?TEMPOR(cli_user, [Host, Port])
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_user(Login, Password) ->
    supervisor:start_child(?SERVER, [Login, Password]).
