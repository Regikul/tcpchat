-module(protocol).

-include("common.hrl").

-define(DELIMETER1, <<"\n">>).
-define(DELIMETER2, <<"\r\n">>).
-define(EOP, <<>>). %% End-Of-Packet

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

-dialyzer({no_match, decode_loop/2}).
decode_loop(#protocol{buffer = Buffer} = P, Acc) when is_list(Acc) ->
    case decode(Buffer) of
        {ok, {Packet, Rest}} -> decode_loop(#protocol{buffer = Rest}, [Packet | Acc]);
        {error, incomplete} -> {ok, {#protocol{buffer = Buffer}, lists:reverse(Acc)}};
        {error, {failed_at, Step}} when Step =/= eop -> {ok, {P, lists:reverse(Acc)}};
        {error, Reason} -> {error, {Reason, lists:reverse(Acc)}}
    end.

-spec decode(binary()) -> {ok, {packet(), binary()}} | {error, atom()}.
decode(Input) ->
    case split(Input) of
        [_] -> {error, incomplete};
        [_Method, <<>>] -> {error, incomplete};
        [Method, Rest] -> decode(Method, Rest)
    end.

-spec decode(binary(), binary()) -> {ok, {packet(), binary()}} | {error, {failed_at, atom()} | malformed | incomplete | unknown}.
decode(<<"auth:">>, Rest) ->
    maybe
        {_, [?NE_BINARY_PAT = Login, MaybePassw]}     ?= {login, split(Rest)},
        {_, [?NE_BINARY_PAT = Passw, MaybeFinalizer]} ?= {passw, split(MaybePassw)},
        {_, [?EOP, NextBuffer]}                       ?= {eop, split(MaybeFinalizer)},
        {ok, {#auth{login = Login, passw = Passw}, NextBuffer}}
    else
        {Step, [?EOP, ?EOP]} when Step =/= eop -> {error, malformed};
        {_Step = eop, [?EOP]} -> {error, incomplete};
        {Step, _} -> {error, {'failed_at', Step}}
    end;
decode(<<"msg:">>, Rest) ->
    maybe
        {_, [?NE_BINARY_PAT = Text, MaybeFrom]} ?= {text, split(Rest)},
        {From, MaybeFinalizer} = case split(MaybeFrom) of
            [?NE_BINARY_PAT = User, Continue] -> {User, Continue};
            [?EOP, _NextBuffer] -> {undefined, MaybeFrom};
            [?EOP] -> {undefined, <<>>}
        end,
        {_, [?EOP, NextBuffer]} ?= {eop, split(MaybeFinalizer)},
        {ok, {#message{from = From, txt = Text}, NextBuffer}}
    else
        {Step, [?EOP, ?EOP]} when Step =/= eop -> {error, malformed};
        {_Step = eop, [?EOP]} -> {error, incomplete};
        {Step, _} -> {error, {'failed_at', Step}}
    end;
decode(<<"auth_sucess:">>, Rest) ->
    case split(Rest) of
        [?EOP, NextBuffer] -> {ok, {#auth_sucess{}, NextBuffer}};
        _Else -> {error, {'failed_at', eop}}
    end;
decode(<<"auth_error:">>, Rest) ->
    maybe
        {_, [?NE_BINARY_PAT = Reason, MaybeFinalizer]} ?= {reason, split(Rest)},
        {_, true} ?= {reason_text, lists:member(Reason, [<<"already_connected">>, <<"auth_error">>])},
        {_, [?EOP, NextBuffer]} ?= {eop, split(MaybeFinalizer)},
        {ok, {#auth_error{status = Reason}, NextBuffer}}
    else
        {Step, [?EOP, ?EOP]} when Step =/= eop -> {error, malformed};
        {_Step = eop, [?EOP]} -> {error, incomplete};
        {Step, _} -> {error, {'failed_at', Step}}
    end;
decode(_Method, _) ->
    {error, unknown}.

-spec encode(packet()) -> binary().
encode(#auth{login = Login, passw = Password}) ->
    <<"auth:\n", Login/binary, "\n", Password/binary, "\n\n">>;
encode(#auth_sucess{}) ->
    <<"auth_sucess:\n\n">>;
encode(#auth_error{status = Status}) when Status =:= <<"already_connected">>; Status =:= <<"auth_error">> ->
    <<"auth_error:\n", Status/binary, "\n\n">>;
encode(#message{from = undefined, txt = Text}) ->
    <<"msg:\n", Text/binary, "\n\n">>;
encode(#message{from = From, txt = Text}) ->
    <<"msg:\n", Text/binary, "\n", From/binary, "\n\n">>.

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

parse_bad_packet_test() ->
    ?assertEqual({error, malformed}, decode(<<"msg:\n\n">>)).

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

symmetric_protocol_auth_test() ->
    Packet = #auth{login = <<"alice">>, passw = <<"passw">>},
    Data = protocol:encode(Packet),
    ?assertEqual({ok, {Packet, <<>>}}, decode(Data)).

symmetric_protocol_message_test() ->
    Packet = #message{txt = <<"hello">>},
    Data = protocol:encode(Packet),
    ?assertEqual({ok, {Packet, <<>>}}, decode(Data)).

symmetric_protocol_signed_message_test() ->
    Packet = #message{from = <<"Alice">>, txt = <<"hello">>},
    Data = protocol:encode(Packet),
    ?assertEqual({ok, {Packet, <<>>}}, decode(Data)).

symmetric_protocol_auth_success_test() ->
    Packet = #auth_sucess{},
    Data = protocol:encode(Packet),
    ?assertEqual({ok, {Packet, <<>>}}, decode(Data)).

symmetric_protocol_auth_error_test() ->
    Packet = #auth_error{status = <<"auth_error">>},
    Data = protocol:encode(Packet),
    ?assertEqual({ok, {Packet, <<>>}}, decode(Data)).

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

    ?assertMatch({error, {malformed, [#auth{}]}}, decode_all(new(), <<Auth/binary, BadData/binary>>)).

decode_partial_buffer_test() ->
    P = new(),
    ?assertEqual({ok, {#protocol{buffer = <<"auth:\n">>}, []}}, decode_all(P, <<"auth:\n">>)).

decode_incremental_buffer_test() ->
    P = new(),
    {ok, {#protocol{buffer = <<"auth:\n">>} = P1, []}} = decode_all(P, <<"auth:\n">>),

    ?assertEqual({ok, {#protocol{buffer = <<"auth:\nalice\n">>}, []}}, decode_all(P1, <<"alice\n">>)).

decode_all_chain_calls_auth_test() ->
    Token1 = <<"auth:\n">>,
    Token2 = <<"alice:\n">>,
    Token3 = <<"password:\n">>,
    Token4 = <<"\n">>,

    Buffer1 = Token1,
    Buffer2 = <<Token1/binary, Token2/binary>>,
    Buffer3 = <<Token1/binary, Token2/binary, Token3/binary>>,

    ?assertEqual({ok, {#protocol{buffer = Buffer1}, []}}, decode_all(new(), Token1)),
    ?assertEqual({ok, {#protocol{buffer = Buffer2}, []}}, decode_all(#protocol{buffer = Buffer1}, Token2)),
    ?assertEqual({ok, {#protocol{buffer = Buffer3}, []}}, decode_all(#protocol{buffer = Buffer2}, Token3)),
    ?assertMatch({ok, {#protocol{buffer = <<>>}, [#auth{}]}}, decode_all(#protocol{buffer = Buffer3}, Token4)).

decode_all_chain_calls_auth_success_test() ->
    Token1 = <<"auth_sucess:\n">>,
    Token2 = <<"\n">>,

    ?assertEqual({ok, {#protocol{buffer = Token1}, []}}, decode_all(new(), Token1)),
    ?assertMatch({ok, {#protocol{buffer = <<>>}, [#auth_sucess{}]}}, decode_all(#protocol{buffer = Token1}, Token2)).

decode_all_chain_calls_auth_error_test() ->
    Token1 = <<"auth_error:\n">>,
    Token2 = <<"auth_error\n">>,
    Token3 = <<"\n">>,

    Buffer1 = Token1,
    Buffer2 = <<Token1/binary, Token2/binary>>,

    ?assertEqual({ok, {#protocol{buffer = Buffer1}, []}}, decode_all(new(), Token1)),
    ?assertEqual({ok, {#protocol{buffer = Buffer2}, []}}, decode_all(#protocol{buffer = Buffer1}, Token2)),
    ?assertMatch({ok, {#protocol{buffer = <<>>}, [#auth_error{}]}}, decode_all(#protocol{buffer = Buffer2}, Token3)).

decode_all_chain_calls_msg_test() ->
    Token1 = <<"msg:\n">>,
    Token2 = <<"Hi there!\n">>,
    Token3 = <<"\n">>,

    Buffer1 = Token1,
    Buffer2 = <<Token1/binary, Token2/binary>>,

    ?assertEqual({ok, {#protocol{buffer = Buffer1}, []}}, decode_all(new(), Token1)),
    ?assertEqual({ok, {#protocol{buffer = Buffer2}, []}}, decode_all(#protocol{buffer = Buffer1}, Token2)),
    ?assertMatch({ok, {#protocol{buffer = <<>>}, [#message{from = undefined, txt = ?NE_BINARY_PAT}]}}, decode_all(#protocol{buffer = Buffer2}, Token3)).

decode_all_chain_calls_signed_msg_test() ->
    Token1 = <<"msg:\n">>,
    Token2 = <<"Hi there\n">>,
    Token3 = <<"alice\n">>,
    Token4 = <<"\n">>,

    Buffer1 = Token1,
    Buffer2 = <<Token1/binary, Token2/binary>>,
    Buffer3 = <<Token1/binary, Token2/binary, Token3/binary>>,

    ?assertEqual({ok, {#protocol{buffer = Buffer1}, []}}, decode_all(new(), Token1)),
    ?assertEqual({ok, {#protocol{buffer = Buffer2}, []}}, decode_all(#protocol{buffer = Buffer1}, Token2)),
    ?assertEqual({ok, {#protocol{buffer = Buffer3}, []}}, decode_all(#protocol{buffer = Buffer2}, Token3)),
    ?assertMatch({ok, {#protocol{buffer = <<>>}, [#message{from = ?NE_BINARY_PAT, txt = ?NE_BINARY_PAT}]}}, decode_all(#protocol{buffer = Buffer3}, Token4)).

-endif.