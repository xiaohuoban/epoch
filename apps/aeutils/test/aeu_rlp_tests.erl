%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Tests for Recursive Length Prefix
%%% @end
%%%-------------------------------------------------------------------

-module(aeu_rlp_tests).

-include_lib("eunit/include/eunit.hrl").

rlp_one_byte_test() ->
    <<42>> = aeu_rlp:encode(<<42>>).

rlp_another_one_byte_test() ->
    B = 127,
    <<B>> = aeu_rlp:encode(<<B>>).

rlp_two_bytes_test() ->
    B = 128,
    X = <<B>>,
    <<129, B>> = aeu_rlp:encode(X).

rlp_many_bytes_test() ->
    L = 55,
    X = list_to_binary(lists:seq(1,L)),
    <<183, X/binary>> = aeu_rlp:encode(X).

rlp_some_more_bytes_test() ->
    L = 56,
    X = list_to_binary(lists:duplicate(L, 42)),
    <<184, 56, X/binary>> = aeu_rlp:encode(X).

rlp_many_more_bytes_test() ->
    L = 256,
    X = list_to_binary(lists:duplicate(L, 42)),
    <<185, 1, 0, X/binary>> = aeu_rlp:encode(X).

rlp_just_one_more_bytes_test() ->
    L = 257,
    X = list_to_binary(lists:duplicate(L, 42)),
    <<185, 1, 1, X/binary>> = aeu_rlp:encode(X).

rlp_onebytelist_test() ->
    L = 1,
    X = lists:duplicate(L, 42),
    <<193, 42>> = aeu_rlp:encode(X).

rlp_shortbytelist_test() ->
    L = 55,
    X = lists:duplicate(L, 42),
    <<247, 42, B/binary>> = aeu_rlp:encode(X),
    54 = byte_size(B).

rlp_longbytelist_test() ->
    L = 56,
    X = lists:duplicate(L, <<42>>),
    <<248, 56, 42, B/binary>> = aeu_rlp:encode(X),
    55 = byte_size(B).

rlp_integer_test() ->
    X = 1,
    <<1>> = aeu_rlp:encode(X).

rlp_twobyteinteger_test() ->
    X = 256,
    <<130,1,0>> = aeu_rlp:encode(X).

rlp_largeinteger_test() ->
    X = 1 bsl 255,
    <<160,128,0, B/binary>> = aeu_rlp:encode(X),
    30 = byte_size(B).

rlp_verylargeinteger_test() ->
    X = 1 bsl 2555,
    <<185,1, 64, 8, 0, B/binary>> = aeu_rlp:encode(X),
    318 = byte_size(B).

rlp_listofints_test() ->
    X = [1,256,1 bsl 255, 1 bsl 2555],
    <<249, 1, 104, 1, 130, 1, 0, 160, 128, B/binary>> = aeu_rlp:encode(X),
    354 = byte_size(B).

rlp_decode_one_byte_test() ->
    <<42>> = aeu_rlp:decode(aeu_rlp:encode(<<42>>)).

rlp_decode_another_one_byte_test() ->
    B = 127,
    <<B>> = aeu_rlp:decode(aeu_rlp:encode(<<B>>)).

rlp_decode_two_bytes_test() ->
    B = 128,
    X = <<B>>,
    X = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_many_bytes_test() ->
    L = 55,
    X = list_to_binary(lists:seq(1,L)),
    X = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_some_more_bytes_test() ->
    L = 56,
    X = list_to_binary(lists:duplicate(L, 42)),
    X = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_many_more_bytes_test() ->
    L = 256,
    X = list_to_binary(lists:duplicate(L, 42)),
    X = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_just_one_more_bytes_test() ->
    L = 257,
    X = list_to_binary(lists:duplicate(L, 42)),
    X = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_onebytelist_test() ->
    L = 1,
    X = lists:duplicate(L, [<<42>>]),
    ?assertEqual(X, aeu_rlp:decode(aeu_rlp:encode(X))).

rlp_decode_shortbytelist_test() ->
    L = 55,
    X = lists:duplicate(L, <<42>>),
    ?assertEqual(X, aeu_rlp:decode(aeu_rlp:encode(X))).

rlp_decode_longbytelist_test() ->
    L = 56,
    X = lists:duplicate(L, <<42>>),
    X = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_integer_test() ->
    X = 1,
    <<X/unsigned-integer>> = aeu_rlp:decode(aeu_rlp:encode(X)).

rlp_decode_twobyteinteger_test() ->
    X = 256,
    R = aeu_rlp:decode(aeu_rlp:encode(X)),
    S = byte_size(R) * 8,
    <<X:S/unsigned-integer>> = R.

rlp_decode_largeinteger_test() ->
    X = 1 bsl 255,
    R = aeu_rlp:decode(aeu_rlp:encode(X)),
    S = byte_size(R) * 8,
    <<X:S/unsigned-integer>> = R.

rlp_decode_verylargeinteger_test() ->
    X = 1 bsl 2555,
    R = aeu_rlp:decode(aeu_rlp:encode(X)),
    S = byte_size(R) * 8,
    <<X:S/unsigned-integer>> = R.


%% TODO: see decode comment in aeu_rlp.erl
%% rlp_decode_listofints_test() ->
%%     X = [1,256,1 bsl 255, 1 bsl 2555],
%%     X = aeu_rlp:decode(aeu_rlp:encode(X)).

