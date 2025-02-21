TCPChat
=====

An OTP application

Configuration
----

You can define `TCPCHAT_HOST` (client only) and `TCPCHAT_PORT` (both server and client) env vars to control socket's listen/connect behaivour.

Build
-----

    $ rebar3 release -n back
    $ rebar3 release -n front

Run
----

Client node does not register itself on epmd, so you can run as many instances as you need to.

    $ _build/default/rel/back/bin/back console
    $ _build/default/rel/front/bin/front console

Usage
----

Server will start with 2 predefined users:

* Alice (login: alice, passw: alice)
* Bob (login: bob, passw: bob)

You can create additional user records by calling `srv_auth_db:register/2`:

```erlang
(tcpchat@HOME-PC)1> srv_auth_db:register(<<"freddy">>, <<"foo">>).
ok
```

Later you can start client node and initiate connection as Freddy:

```erlang
(nonode@nohost)1> {ok, Fred} = cli_users_sup:start_user(<<"freddy">>, <<"foo">>).
{ok,<0.270.0>}
 [system] <0.271.0> supervisor_bridge:report_progress/4:239 [info] Supervisor: {local,inet_gethost_native_sup}. Started: pid=<0.272.0>,mfa={inet_gethost_native,init,[[]]}.
 [system] <0.256.0> supervisor:report_progress/3:2145 [debug] Supervisor: {local,kernel_safe_sup}. Started: id=inet_gethost_native_sup,pid=<0.271.0>.
 ```

 On the server side:
 ```erlang
 [127.0.0.1:49948] <0.504.0> srv_listener:handle_info/2:54 [debug] inbound data: <<"auth:\nfreddy\nfoo\n\n">>
[127.0.0.1:49948] <0.504.0> srv_listener:handle_info/2:57 [info] inbound packets: [{auth,<<"freddy">>,<<"foo">>}]
 ```

 Pls note: here instead of `system` logger reports `127.0.0.1:49948`, it helps to distinguish clients.

 Let's start Alice:
```erlang
(nonode@nohost)2> {ok, Alice} = cli_users_sup:start_user(<<"alice">>, <<"alice">>).
{ok,<0.274.0>}
```
And send some messages:
```erlang
(nonode@nohost)3> cli_user:send_message(Alice, <<"hello">>).
[chat_user:alice] <0.274.0> cli_user:handle_cast/2:66 [info] alice: hello
ok
[chat_user:freddy] <0.270.0> cli_user:handle_packet/2:55 [info] alice: hello
(nonode@nohost)4> cli_user:send_message(Fred, <<"hello, Alice">>).
ok
[chat_user:freddy] <0.270.0> cli_user:handle_cast/2:66 [info] freddy: hello, Alice
[chat_user:alice] <0.274.0> cli_user:handle_packet/2:55 [info] freddy: hello, Alice
```

Try to enter chat as someone who are not registered will end up with `auth_error` error:
```erlang
(nonode@nohost)5> {ok, Jesus} = cli_users_sup:start_user(<<"jesus">>, <<"christ">>).
{ok,<0.278.0>}
[chat_user:jesus] <0.278.0> gen_server:error_info/8:2646 [error] Generic server <0.278.0> terminating. Reason: {auth_error,<<"auth_error">>}. Last message: {tcp,#Port<0.8>,<<"auth_error:\nauth_error\n\n">>}. State: {state,#Port<0.8>,{protocol,<<>>},<<"jesus">>}.
[chat_user:jesus] <0.278.0> proc_lib:crash_report/4:948 [error] crasher: initial call: cli_user:init/1, pid: <0.278.0>, registered_name: [], exit: {{auth_error,<<"auth_error">>},[{gen_server,handle_common_reply,8,[{file,"gen_server.erl"},{line,2476}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,329}]}]}, ancestors: [cli_users_sup,client_sup,<0.262.0>], message_queue_len: 0, messages: [], links: [<0.264.0>,#Port<0.8>], dictionary: [{'$logger_metadata$',#{token => "chat_user:jesus"}}], trap_exit: false, status: running, heap_size: 2586, stack_size: 29, reductions: 3512; neighbours:
 [system] <0.264.0> supervisor:do_restart/3:1303 [error] Supervisor: {local,cli_users_sup}. Context: child_terminated. Reason: {auth_error,<<"auth_error">>}. Offender: id=cli_user,pid=<0.278.0>.
```

Try to enter chat as someone who are already logged will end up with `already_connected` error:
```erlang
(nonode@nohost)6> {ok, Alice2} = cli_users_sup:start_user(<<"alice">>, <<"alice">>).
{ok,<0.280.0>}
[chat_user:alice] <0.280.0> gen_server:error_info/8:2646 [error] Generic server <0.280.0> terminating. Reason: {auth_error,<<"already_connected">>}. Last message: {tcp,#Port<0.9>,<<"auth_error:\nalready_connected\n\n">>}. State: {state,#Port<0.9>,{protocol,<<>>},<<"alice">>}.
[chat_user:alice] <0.280.0> proc_lib:crash_report/4:948 [error] crasher: initial call: cli_user:init/1, pid: <0.280.0>, registered_name: [], exit: {{auth_error,<<"already_connected">>},[{gen_server,handle_common_reply,8,[{file,"gen_server.erl"},{line,2476}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,329}]}]}, ancestors: [cli_users_sup,client_sup,<0.262.0>], message_queue_len: 0, messages: [], links: [<0.264.0>,#Port<0.9>], dictionary: [{'$logger_metadata$',#{token => "chat_user:alice"}}], trap_exit: false, status: running, heap_size: 2586, stack_size: 29, reductions: 3637; neighbours:
 [system] <0.264.0> supervisor:do_restart/3:1303 [error] Supervisor: {local,cli_users_sup}. Context: child_terminated. Reason: {auth_error,<<"already_connected">>}. Offender: id=cli_user,pid=<0.280.0>.
```
Pid `Alice2` will not be available after error from server.