-module(protocol).

-include("common.hrl").

-define(DELIMETER1, <<"\n">>).
-define(DELIMETER2, <<"\r\n">>).

-export([new/0, decode_all/2, encode/1]).

-record(protocol, {buffer}).
-type t() :: #protocol{}.

-export_type([t/0]).

-spec new() -> t().
new() ->
    #protocol{buffer = <<>>}.

-spec decode_all(t(), binary()) -> {ok, {t(), Packets}} | {error, {Reason, Packets}}
    when
        Packets :: [packet()],
        Reason :: incomplete | unknown | {failed_at, atom()}.
decode_all(#protocol{buffer = Buffer}, ?NE_BINARY_PAT = Append) ->
    decode_loop(#protocol{buffer = <<Buffer/binary, Append/binary>>}, []).

decode_loop(#protocol{buffer = Buffer} = P, Acc) when is_list(Acc) ->
    case decode(Buffer) of
        {ok, {Packet, Rest}} -> decode_loop(#protocol{buffer = Rest}, [Packet | Acc]);
        {error, incomplete} -> {ok, {#protocol{buffer = Buffer}, lists:reverse(Acc)}};
        {error, {{failed_at, Step}, Acc}} when Step =/= eop -> {ok, {P, lists:reverse(Acc)}};
        {error, Reason} -> {error, {Reason, lists:reverse(Acc)}}
    end.

-spec decode(binary()) -> {ok, {packet(), binary()}} | {error, atom()}.
decode(Input) ->
    case split(Input) of
        [_] -> {error, incomplete};
        [_Method, <<>>] -> {error, incomplete};
        [Method, Rest] -> decode(Method, Rest)
    end.

decode(<<"auth:">>, Rest) ->
    maybe
        {_, [?NE_BINARY_PAT = Login, MaybePassw]}     ?= {login, split(Rest)},
        {_, [?NE_BINARY_PAT = Passw, MaybeFinalizer]} ?= {passw, split(MaybePassw)},
        {_, [<<>>, NextBuffer]}                       ?= {eop, split(MaybeFinalizer)},
        {ok, {#auth{login = Login, passw = Passw}, NextBuffer}}
    else
        {Step, _} -> {error, {'failed_at', Step}}
    end;
decode(<<"msg:">>, Rest) ->
    maybe
        {_, [?NE_BINARY_PAT = Text, MaybeFrom]} ?= {text, split(Rest)},
        {From, MaybeFinalizer} = case split(MaybeFrom) of
            [?NE_BINARY_PAT = User, Continue] -> {User, Continue};
            [<<>>, _NextBuffer] -> {undefined, MaybeFrom}
        end,
        {_, [<<>>, NextBuffer]} ?= {eop, split(MaybeFinalizer)},
        {ok, {#message{from = From, txt = Text}, NextBuffer}}
    else
        {Step, _} -> {error, {'failed_at', Step}}
    end;
decode(_Method, _) ->
    {error, 'unknown'}.

encode(#auth{login = Login, passw = Password}) ->
    <<"auth:\n", Login/binary, "\n", Password/binary, "\n\n">>;
encode(#message{from = From, txt = Text}) ->
    <<"msg:\n", From/binary, "\n", Text/binary, "\n\n">>.

pattern() -> binary:compile_pattern([?DELIMETER1, ?DELIMETER2]).
split(Subject) -> binary:split(Subject, pattern()).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_data() -> <<>>.
auth_data(Delimeter) ->
    <<
    "auth:", Delimeter/binary,
    "login", Delimeter/binary,
    "password", Delimeter/binary, Delimeter/binary
    >>.

message_data(Delimeter) ->
    <<
    "msg:", Delimeter/binary,
    "Hello there!", Delimeter/binary, Delimeter/binary
    >>.

signed_message_data(Delimeter) ->
    <<
    "msg:", Delimeter/binary,
    "This is a signed message", Delimeter/binary,
    "Alice", Delimeter/binary, Delimeter/binary
    >>.

parse_empty_biffer_test() ->
    ?assertEqual({error, incomplete}, decode(empty_data())).

parse_bad_buffer_test() ->
    ?assertEqual({error, incomplete}, decode(<<"this is not a packet">>)).

parse_unknown_packet_test() ->
    ?assertEqual({error, unknown}, decode(<<"undefiend:", ?DELIMETER1/binary, "field", ?DELIMETER1/binary, ?DELIMETER1/binary>>)).

parse_auth_data_test() ->
    Auth = #auth{login = <<"login">>, passw = <<"password">>},
    ?assertEqual({ok, {Auth, <<>>}}, decode(auth_data(?DELIMETER1))),
    ?assertEqual({ok, {Auth, <<>>}}, decode(auth_data(?DELIMETER2))).

parse_message_data_test() ->
    Message = #message{txt = <<"Hello there!">>},
    ?assertEqual({ok, {Message, <<>>}}, decode(message_data(?DELIMETER1))),
    ?assertEqual({ok, {Message, <<>>}}, decode(message_data(?DELIMETER2))).

parse_signed_message_test() ->
    Message = #message{from = <<"Alice">>, txt = <<"This is a signed message">>},
    ?assertEqual({ok, {Message, <<>>}}, decode(signed_message_data(?DELIMETER1))),
    ?assertEqual({ok, {Message, <<>>}}, decode(signed_message_data(?DELIMETER2))).

keep_trailing_buffer() ->
    TrailingData = <<"foo">>,
    AuthData = auth_data(?DELIMETER1),
    MessageData = message_data(?DELIMETER1),
    SignedMessageData = signed_message_data(?DELIMETER1),

    ?assertMatch({ok, {#auth{}, TrailingData}}, decode(<<AuthData/binary, TrailingData/binary>>)),
    ?assertMatch({ok, {#message{}, TrailingData}}, decode(<<MessageData/binary, TrailingData/binary>>)),
    ?assertMatch({ok, {#message{}, TrailingData}}, decode(<<SignedMessageData/binary, TrailingData/binary>>)).

decode_all_single_packet_test() ->
    ?assertMatch({ok, {_, [#auth{}]}}, decode_all(new(), auth_data(?DELIMETER1))).

decode_all_two_packets_test() ->
    Auth = auth_data(?DELIMETER1),
    Message = message_data(?DELIMETER1),

    ?assertMatch({ok, {_, [#auth{}, #message{}]}}, decode_all(new(), <<Auth/binary, Message/binary>>)).

decode_all_keep_partial_data_test() ->
    Auth = auth_data(?DELIMETER1),
    TrailingData = <<"this is not a valid data">>,

    ?assertMatch({ok, {#protocol{buffer = TrailingData}, [#auth{}]}}, decode_all(new(), <<Auth/binary, TrailingData/binary>>)).

decode_all_skip_bad_data_test() ->
    Auth = auth_data(?DELIMETER1),
    BadData = <<"msg:\n\n">>,

    ?assertMatch({error, {{failed_at, text}, [#auth{}]}}, decode_all(new(), <<Auth/binary, BadData/binary>>)).

decode_all_chan_calls_test() ->
    P = new(),

    {ok, {P1, []}} = decode_all(P, <<"auth:\n">>),
    {ok, {P2, []}} = decode_all(P1, <<"alice\n">>),
    {ok, {P3, []}} = decode_all(P2, <<"password\n">>),
    {ok, {P4, []}} = decode_all(P3, <<"\n">>).

-endif.