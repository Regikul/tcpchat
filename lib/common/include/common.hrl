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

-define(CHILD(Type, Name, Args, Restart), #{id => Name, start => {Name, start_link, Args}, restart => Restart, shutdown => infinity, type => Type, modules => [Name]}).
-define(SUPERV(Name), ?CHILD(supervisor, Name, [], transient)).
-define(WORKER(Name, Args), ?CHILD(worker, Name, Args, transient)).
-define(TEMPOR(Name, Args), ?CHILD(worker, Name, Args, temporary)).

-define(NE_BINARY_PAT, <<_:8, _/binary>>).

-endif.