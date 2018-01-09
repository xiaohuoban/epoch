%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Resolve registered names
%%% @end
%%%-------------------------------------------------------------------

-module(aens).

%% API
-export([resolve/2]).

-define(LABEL_SEPARATOR, <<".">>).

resolve(Type, Binary) ->
    %% TODO: add validation
    case binary:match(Binary, ?LABEL_SEPARATOR) of
        nomatch ->
            aec_base58c:safe_decode(Type, Binary);
        {_,_} ->
            StateTrees = aec_conductor:top_state_trees(),
            NameTree = aec_trees:ns(StateTrees),
            NameHash = aens_hash:name_hash(Binary),
            Name = aens_state_tree:get_name(NameHash, NameTree),
            Pointers = aens_names:pointers(Name),
            proplists:get_value(Type, Pointers, error_invalid_name)
    end.
