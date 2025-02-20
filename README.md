tcpchat
=====

An OTP application

Configuration
----

You can define `TCPCHAT_HOST` (client only) and `TCPCHAT_PORT` (both server and client) env vars to control socket listen/connect behaivour.

Build
-----

    $ rebar3 release -n back
    $ rebar3 release -n front

Run
----

    $ _build/default/rel/back/bin/back console
    $ _build/default/rel/front/bin/front console
