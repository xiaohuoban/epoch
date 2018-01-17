%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle Patricia Trees
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeu_mp_trees).

-export([ new/0
        , delete/3
        , get/3
        , pp/2
        , put/4
        , root_hash/1
        ]).

-type tree_node() :: null() | leaf() | branch() | extension().

-type null()      :: <<>>.
-type branch()    :: {tree_node(), tree_node(), tree_node(), tree_node(),
                      tree_node(), tree_node(), tree_node(), tree_node(),
                      tree_node(), tree_node(), tree_node(), tree_node(),
                      tree_node(), tree_node(), tree_node(), tree_node(),
                      value()}.

-type leaf()      :: {encoded_path(), value()}.
-type extension() :: {encoded_path(), hash()}.

-type encoded_path() :: binary().
-type value()        :: binary(). %% TODO: 'Something that can be rlp encoded'
-type hash()         :: <<_:256>> | <<>>.

-record(mpt, { hash = <<>>
             }).

%%-define(debug_on, true).
-ifdef(debug_on).
-define(debug(___FMT___, ___ARGS___), io:format(___FMT___, ___ARGS___)).
-else.
-define(debug(___FMT___, ___ARGS___), ok).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #mpt{}.

get(Key, #mpt{hash = Hash}, DB) ->
    ?debug("\n", []),
    int_get(Key, decode_node(Hash, DB), DB).

put(Key, <<>>, #mpt{} = Mpt, DB) when is_bitstring(Key), Key =/= <<>> ->
    delete(Key, Mpt, DB);
put(Key, Val, #mpt{hash = Hash} = Mpt, DB) when is_bitstring(Key), Key =/= <<>> ->
    try int_put(Key, Val, decode_node(Hash, DB), DB) of
        {NewTree, DB1} ->
            {NewHash, DB2} = force_encoded_node_to_hash(NewTree, DB1),
            {Mpt#mpt{ hash = NewHash}, DB2}
    catch throw:unchanged -> {Mpt, DB}
    end.

delete(Key, #mpt{hash = Hash} = Mpt, DB) when is_bitstring(Key) ->
    try int_delete(Key, decode_node(Hash, DB), DB) of
        {<<>>, DB1} ->
            {Mpt#mpt{ hash = <<>>
                    },
             DB1};
        {NewTree, DB1} ->
            {NewHash, DB2} = force_encoded_node_to_hash(NewTree, DB1),
            {Mpt#mpt{ hash = NewHash}, DB2}
    catch throw:unchanged -> {Mpt, DB}
    end.

root_hash(#mpt{hash = H}) -> H.

pp(#mpt{hash = Hash}, DB) ->
    io:format("Root: ~s\n", [hexstring(Hash)]),
    [io:format("~s\n", [S])
     || S <- lists:flatten([pp_tree(decode_node(Hash, DB), DB)])],
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Get

int_get(_Path, <<>>,_DB) ->
    <<>>;
int_get(<<>>, {branch, Branch},_DB) when tuple_size(Branch) =:= 17 ->
    element(17, Branch);
int_get(Path, {branch, Branch}, DB) when tuple_size(Branch) =:= 17 ->
    <<Next:4, Rest/bits>> = Path,
    NextNode = decode_node(element(Next + 1, Branch), DB),
    int_get(Rest, NextNode, DB);
int_get(Path, {Type, NodePath, NodeVal}, DB) when Type =:= ext; Type =:= leaf ->
    S = bit_size(NodePath),
    case Path of
        NodePath when Type =:= leaf ->
            NodeVal;
        <<NodePath:S/bits, _/bits>> when Type =:= leaf ->
            <<>>;
        <<NodePath:S/bits, Rest/bits>> when Type =:= ext ->
            int_get(Rest, decode_node(NodeVal, DB), DB);
        _ ->
            <<>>
    end.

%%%===================================================================
%%% Put

int_put(Key, Val, <<>>, DB) ->
    leaf(Key, Val, DB);
int_put(Key1, Val1, {leaf, Key2, Val2}, DB) ->
    {Common, K1, K2} = find_common_path(Key1, Key2),
    case {K1, K2} of
        {<<>>, <<>>} when Val1 =:= Val2 ->
            %% The same key and the same value.
            throw(unchanged);
        {<<>>, <<>>} ->
            %% The same key. Just update the value.
            leaf(Key1, Val1, DB);
        {<<>>, <<K:4, Rest/bits>>} ->
            %% We need a branch before the old leaf
            {L, DB1} = leaf(Rest, Val2, DB),
            {Branch, DB2} = new_branch([{K, L}], Val1, DB1),
            maybe_extension(Common, Branch, DB2);
        {<<K:4, Rest/bits>>, <<>>} ->
            %% We need a branch before the new leaf
            {L, DB1} = leaf(Rest, Val1, DB),
            {Branch, DB2} = new_branch([{K, L}], Val2, DB1),
            maybe_extension(Common, Branch, DB2);
        {<<Next1:4, Rest1/bits>>, <<Next2:4, Rest2/bits>>} ->
            %% We need a branch before both leaves
            {L1, DB1} = leaf(Rest1, Val1, DB),
            {L2, DB2} = leaf(Rest2, Val2, DB1),
            {Branch, DB3} = new_branch([{Next1, L1}, {Next2, L2}], <<>>, DB2),
            maybe_extension(Common, Branch, DB3)
    end;
int_put(<<>> =_Key, Val, {branch, Branch}, DB) ->
    %% Update the value of the branch
    case element(17, Branch) =:= Val of
        true  -> throw(unchanged);
        false -> branch(setelement(17, Branch, Val), DB)
    end;
int_put(<<Next:4, Rest/bits>>, Val, {branch, Branch}, DB) ->
    NextNode = decode_node(element(Next + 1, Branch), DB),
    {NewNode, DB1} = int_put(Rest, Val, NextNode, DB),
    Branch1 = setelement(Next + 1, Branch, NewNode),
    branch(Branch1, DB1);
int_put(Key1, Val, {ext, Key2, Hash} = Ext, DB) ->
    {Common, K1, K2} = find_common_path(Key1, Key2),
    case {K1, K2} of
        {<<>>, <<>>} ->
            %% The same preamble
            {NewNode, DB1} = int_put(<<>>, Val, decode_node(Hash, DB), DB),
            extension(Common, NewNode, DB1);
        {_, <<>>} ->
            %% The whole extension is passed
            {NewNode, DB1} = int_put(K1, Val, decode_node(Hash, DB), DB),
            extension(Common, NewNode, DB1);
        {<<>>, <<Next2:4, Rest2/bits>>} ->
            %% We need to branch out
            {NewNode2, DB1} = update_extension_path(Ext, Rest2, DB),
            {Branch, DB2} = new_branch([{Next2, NewNode2}], Val, DB1),
            maybe_extension(Common, Branch, DB2);
        {<<Next1:4, Rest1/bits>>, <<Next2:4, Rest2/bits>>} ->
            {NewNode1, DB1} = leaf(Rest1, Val, DB),
            {NewNode2, DB2} = update_extension_path(Ext, Rest2, DB1),
            BranchList = [{Next1, NewNode1}, {Next2, NewNode2}],
            {Branch, DB3} = new_branch(BranchList, <<>>, DB2),
            maybe_extension(Common, Branch, DB3)
    end.

find_common_path(B1, B2) ->
    Offset = find_common_path_1(B1, B2),
    <<Common:Offset/bits, Left1/bits>> = B1,
    <<_     :Offset/bits, Left2/bits>> = B2,
    {Common, Left1, Left2}.

find_common_path_1(<<X:4, Rest1/bits>>,
                   <<X:4, Rest2/bits>>) ->
    4 + find_common_path_1(Rest1, Rest2);
find_common_path_1(_, _) ->
    0.

%%%===================================================================
%%% Delete

int_delete(_Key, <<>>,_DB) ->
    throw(unchanged);
int_delete(Key1, {leaf, Key2, _} = _Node, DB) ->
    ?debug("~w: ~s ~s\n", [?LINE, hexstring(Key1), pp_node(_Node, DB)]),
    case Key1 =:= Key2 of
        true  -> {<<>>, DB};
        false -> throw(unchanged)
    end;
int_delete(Key1, {ext, Key2, Hash} = _Node, DB) ->
    ?debug("~w: ~s ~s\n", [?LINE, hexstring(Key1), pp_node(_Node, DB)]),
    S = bit_size(Key2),
    case Key1 of
        <<Key2:S/bits, Rest/bits>> ->
            case int_delete(Rest, decode_node(Hash, DB), DB) of
                {<<>>, DB1}    -> {<<>>, DB1};
                {NewNode, DB1} ->
                    case decode_node(NewNode, DB1) of
                        {leaf, Path, Val} -> leaf(<<Key2/bits, Path/bits>>, Val, DB);
                        {ext, Path, Hash} -> extension(<<Key2/bits, Path/bits>>, Hash, DB);
                        _ -> extension(Key2, NewNode, DB1)
                    end
            end;
        _ ->
            throw(unchanged)
    end;
int_delete(Key, {branch, Branch} = _Node, DB) ->
    ?debug("~w: ~s ~s\n", [?LINE, hexstring(Key), pp_node(_Node, DB)]),
    case Key of
        <<>> when element(17, Branch) =:= <<>> -> throw(unchanged);
        <<>> -> try_reduce_branch(setelement(17, Branch, <<>>), DB);
        <<Next:4, Rest/bits>> ->
            case int_delete(Rest, decode_node(element(Next + 1, Branch), DB), DB) of
                {<<>>, DB1} ->
                    try_reduce_branch(setelement(Next + 1, Branch, <<>>), DB1);
                {NewNode, DB1} ->
                    branch(setelement(Next + 1, Branch, NewNode), DB1)
            end
    end.

try_reduce_branch(Branch, DB) ->
    case get_singleton_branch(Branch) of
        error     -> branch(Branch, DB);
        none      -> error({empty_branch, Branch});
        {17, Val} -> leaf(<<>>, Val, DB);
        {Element, NextNode} ->
            case decode_node(NextNode, DB) of
                {leaf, Path, Val} ->
                    Path1 = <<(Element-1):4, Path/bits>>,
                    leaf(Path1, Val, DB);
                {ext, Path, Val} ->
                    Path1 = <<(Element-1):4, Path/bits>>,
                    extension(Path1, Val, DB);
                {branch,_B} ->
                    extension(<<(Element-1):4>>, NextNode, DB)
            end
    end.


get_singleton_branch(Branch) ->
    get_singleton_branch(Branch, 1, none).

get_singleton_branch(_Branch, N, Res) when N > 17 -> Res;
get_singleton_branch(Branch, N, Acc) when element(N, Branch) =:= <<>> ->
    %% Nothing in this branch.
    get_singleton_branch(Branch, N + 1, Acc);
get_singleton_branch(Branch, N, Acc) ->
    case Acc =:= none of
        true ->
            %% This is the first value we see.
            Val = element(N, Branch),
            get_singleton_branch(Branch, N + 1, {N, Val});
        false ->
            %% At least two values.
            error
    end.

%%%===================================================================
%%% Prettyprinter

pp_node(Node, DB) ->
    pp_node(Node, DB, false).

pp_tree(Node, DB) ->
    pp_node(Node, DB, true).

pp_node(<<>>,_DB,_Children) -> <<"<<>>">>;
pp_node({leaf, Path, Val}, DB,_Children) ->
    {Node, _} = leaf(Path, Val, DB),
    Hash = node_hash(Node),
    S = io_lib:format("~s: {leaf, ~s, ~w}", [hexstring(Hash), hexstring(Path), Val]),
    iolist_to_binary(S);
pp_node({ext, Path, ChildHash}, DB, Children) ->
    {Node, _} = extension(Path, ChildHash, DB),
    Hash = node_hash(Node),
    S = io_lib:format("~s: {ext, ~s, ~s}", [hexstring(Hash), hexstring(Path), hexstring(ChildHash)]),
    [iolist_to_binary(S)
     | if Children -> pp_node(decode_node(ChildHash, DB), DB, Children);
          true -> []
       end];
pp_node({branch, Branch}, DB, Children) ->
    {Node, _} = branch(Branch, DB),
    Hash = node_hash(Node),
    BranchElements = [pp_branch_element(X) || X <- lists:droplast(tuple_to_list(Branch))],
    BranchString = "[" ++ string:join(BranchElements, ",") ++ "]",
    Val = element(17, Branch),
    S = io_lib:format("~s: {branch, ~s, ~p}", [hexstring(Hash), BranchString, Val]),
    [iolist_to_binary(S)
     | if Children ->
               [pp_node(decode_node(X, DB), DB)
                || X <- lists:droplast(tuple_to_list(Branch)), X =/= <<>>];
          true -> []
       end].

pp_branch_element(<<>>) -> "<<>>";
pp_branch_element(Bin) when byte_size(Bin) =:= 32 -> [hexstring(Bin)];
pp_branch_element(Bin) when byte_size(Bin) <   32 -> ["*", hexstring(node_hash(Bin))].

hexstring(Bin) ->
    [hexchar(X) || <<X:4>> <= Bin].

hexchar(X) when X > -1, X < 10 -> $0 + X;
hexchar(X) when X > -1, X < 16 -> $A + X - 10.

%%%===================================================================
%%% Nodes

node_hash(<<>>) -> error(no_hash_for_nil);
node_hash(Bin) when byte_size(Bin) < 32   -> crypto:hash(sha256, Bin);
node_hash(Bin) when byte_size(Bin) =:= 32 -> Bin.

get_encoded_node(<<>> = X,_DB) -> X;
get_encoded_node(Bin,_DB) when byte_size(Bin) < 32 -> Bin;
get_encoded_node(Hash, DB) when byte_size(Hash) =:= 32 ->
    Node = db_get(Hash, DB),
    Rlp  = aeu_rlp:encode(Node),
    case byte_size(Rlp) < 32 of
        true  -> Rlp;
        false -> Hash
    end.

decode_node(<<>> = X,_DB) -> X;
decode_node(Bin,_DB) when byte_size(Bin) < 32 ->
    case aeu_rlp:decode(Bin) of %% TODO: Decode with tuples
        [CPath, Val] ->
            {Type, Path} = compact_decode(CPath),
            {Type, Path, Val};
        [_|_] = Branch ->
            {branch, list_to_tuple(Branch)}
    end;
decode_node(Hash, DB) when byte_size(Hash) =:= 32 ->
    case db_get(Hash, DB) of
        [CPath, Val] ->
            {Type, Path} = compact_decode(CPath),
            {Type, Path, Val};
        [_|_] = Branch ->
            {branch, list_to_tuple(Branch)}
    end.
encode_node(<<>>, DB) ->
    error(nil_has_no_encoding);
encode_node(Node, DB) ->
    Rlp = aeu_rlp:encode(Node),
    case byte_size(Rlp) < 32 of
        true  -> {Rlp, DB};
        false ->
            Hash = crypto:hash(sha256, Rlp),
            {Hash, db_put(Hash, Node, DB)}
    end.

force_encoded_node_to_hash(<<>>,_DB) ->
    error(cannot_store_nil);
force_encoded_node_to_hash(Hash, DB) when byte_size(Hash) =:= 32 ->
    {Hash, DB};
force_encoded_node_to_hash(Rlp, DB) when byte_size(Rlp) < 32 ->
    Node = aeu_rlp:decode(Rlp),
    Hash = node_hash(Rlp),
    {Hash, db_put(Hash, Node, DB)}.

%%%===================================================================
%%% Leaf

leaf(Path, Val, DB) when is_binary(Val) ->
    encode_node([encode_path_leaf(Path), Val], DB).

maybe_extension(<<>>, Node, DB) ->
    {Node, DB};
maybe_extension(Path, Node, DB) when Path =/= <<>> ->
    extension(Path, Node, DB).

%%%===================================================================
%%% Extension

extension(Path, Node, DB) when Path =/= <<>> ->
    %% Extensions must have a hash as value.
    {Hash, DB1} = force_encoded_node_to_hash(Node, DB),
    encode_node([encode_path_ext(Path), Hash], DB1).

update_extension_path({ext,_OldPath, Hash}, NewPath, DB) ->
    case NewPath =:= <<>> of
        true ->
            %% Fall through to the refered value
            {get_encoded_node(Hash, DB), DB};
        false ->
            encode_node([encode_path_ext(NewPath), Hash], DB)
    end.

%%%===================================================================
%%% Branch

branch(Branch, DB) ->
    List = tuple_to_list(Branch),
    case lists:all(fun is_binary/1, List) of
        true  -> encode_node(List, DB); %% TODO: Encode with tuples
        false -> error({illegal_branch, Branch})
    end.

new_branch(List, Val, DB) ->
    {true, List} = {length(List) =:= length(lists:ukeysort(1, List)), List},
    true = lists:all(fun({X, _}) -> X < 17 end, List),
    Branch = lists:foldl(fun({X, Node}, Acc) -> setelement(X + 1, Acc, Node) end,
                         { <<>>, <<>>, <<>>, <<>>
                         , <<>>, <<>>, <<>>, <<>>
                         , <<>>, <<>>, <<>>, <<>>
                         , <<>>, <<>>, <<>>, <<>>
                         , Val}, List),
    branch(Branch, DB).

%%%===================================================================
%%% DB interface (currently dict)
%% TODO: Make a local cache, and a proper db interface.

db_get(Hash, DB) ->
  dict:fetch(Hash, DB).

db_put(Hash, Val, DB) ->
  dict:store(Hash, Val, DB).


%%%===================================================================
%%% Compact encoding of hex sequence with optional terminator
%%%
%%% From: https://github.com/ethereum/wiki/wiki/Patricia-Tree
%%%
%%% The flagging of both odd vs. even remaining partial path length and
%%% leaf vs. extension node as described above reside in the first nibble
%%% of the partial path of any 2-item node. They result in the following:
%%%
%%% hex char    bits    |    node type partial     path length
%%% ----------------------------------------------------------
%%%    0        0000    |       extension              even
%%%    1        0001    |       extension              odd
%%%    2        0010    |   terminating (leaf)         even
%%%    3        0011    |   terminating (leaf)         odd
%%%
%%% For even remaining path length (0 or 2), another 0 "padding"
%%% nibble will always follow.

-define(EVEN_EXT,  0).
-define(ODD_EXT,   1).
-define(EVEN_LEAF, 2).
-define(ODD_LEAF , 3).

encode_path_leaf(Path) ->
    case bit_size(Path) rem 8 of
        0 -> <<?EVEN_LEAF:4, 0:4, Path/bits>>;
        4 -> <<?ODD_LEAF:4, Path/bits>>
    end.

encode_path_ext(Path) ->
    case bit_size(Path) rem 8 of
        0 -> <<?EVEN_EXT:4, 0:4, Path/bits>>;
        4 -> <<?ODD_EXT:4, Path/bits>>
    end.

compact_decode(<<?EVEN_EXT :4, 0:4, Left/bits>>) -> {ext, Left};
compact_decode(<<?ODD_EXT  :4,      Left/bits>>) -> {ext, Left};
compact_decode(<<?EVEN_LEAF:4, 0:4, Left/bits>>) -> {leaf, Left};
compact_decode(<<?ODD_LEAF :4,      Left/bits>>) -> {leaf, Left}.
