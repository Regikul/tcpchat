-ifndef(_COMMON_HRL).
-define(_COMMON_HRL, true).

-include_lib("kernel/include/logger.hrl").

-record(auth, {
    login :: binary(),
    passw :: binary()
}).
-record(auth_sucess, {}).
-record(auth_error, {
    status :: binary() %%  <<"already_connected">> | <<"auth_error">>
}).
-record(message, {
    from :: binary() | undefined,
    txt :: binary()
}).

-type packet() :: #auth{} | #auth_sucess{} | #auth_error{} | #message{}.

-define(WORKER(Name, Args), #{id => Name, start => {Name, start_link, Args}, restart => transient, shutdown => 5, type => worker, modules => [Name]}).
-define(SUPERV(Name), #{id => Name, start => {Name, start_link, []}, restart => transient, shutdown => infinity, type => supervisor, modules => [Name]}).

-define(NE_BINARY_PAT, <<_:8, _/binary>>).

-endif.