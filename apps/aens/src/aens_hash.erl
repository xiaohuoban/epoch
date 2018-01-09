%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System hashing functions
%%% @end
%%%=============================================================================

-module(aens_hash).

%% API
-export([name_hash/1,
         commitment_hash/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type name_hash() :: binary().
-type commitment_hash() :: binary().

-export_type([name_hash/0,
              commitment_hash/0]).

-define(NAME_HASH_BYTES, 32).
-define(COMMITMENT_HASH_BYTES, 32).

%%%===================================================================
%%% API
%%%===================================================================

-spec commitment_hash(binary(), integer()) -> commitment_hash().
commitment_hash(Name, Nonce) ->
    NameHash = hash(Name),
    NonceBin = int_to_bin(Nonce),
    hash(<<NameHash/binary, NonceBin/binary>>).

-spec name_hash(aens_names:name()) -> name_hash().
name_hash(Name) ->
    Labels = binary:split(Name, <<".">>, [global]),
    hash_labels(lists:reverse(Labels)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

hash_labels([]) ->
    empty_hash();
hash_labels([Label | Rest]) ->
    LabelHash = hash(Label),
    RestHash = hash_labels(Rest),
    hash(<<RestHash/binary, LabelHash/binary>>).

empty_hash() ->
    <<0:?NAME_HASH_BYTES/unit:8>>.

hash(Bin) ->
    crypto:hash(sha256, Bin).

int_to_bin(Int) ->
    <<Int:?COMMITMENT_HASH_BYTES/integer-unit:8>>.
