%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    Unit tests for Merkle Patricia Trees
%%% @end
%%%=============================================================================
-module(aeu_mp_trees_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    [ {"Put", fun test_put/0}
    , {"Lookup", fun test_lookup/0}
    , {"Delete one", fun test_delete_one_by_one/0}
    , {"Delete all", fun test_delete_all/0}
    ].

hash_test_() ->
    [ {"Reversed put", fun test_reversed_put/0}
    , {"Put and delete", fun test_put_and_delete/0}
    ].

%%%=============================================================================
%%% Basic tests

test_put() ->
    {_Tree,_DB,_Vals} = gen_mp_tree({23, 123534, 345345}, 1000),
    ok.

test_lookup() ->
    {Tree, DB, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    [?assertEqual(Y, aeu_mp_trees:get(X, Tree, DB)) || {X, Y} <- Vals],
    ok.

test_delete_one_by_one() ->
    {Tree, DB, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    [begin
         {T, DB1} = aeu_mp_trees:delete(X, Tree, DB),
         ?assertEqual(<<>>, aeu_mp_trees:get(X, T, DB1))
     end
     || {X,_Y} <- Vals],
    ok.

test_delete_all() ->
    {Tree, DB, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    {Tree1,_DB1}     = test_delete_all(Vals, Tree, DB),
    ?assertEqual(aeu_mp_trees:root_hash(aeu_mp_trees:new()),
                 aeu_mp_trees:root_hash(Tree1)).

test_delete_all([{X, Y}|Left], Tree, DB) ->
    ?assertEqual(Y, aeu_mp_trees:get(X, Tree, DB)),
    {Tree1, DB1} = aeu_mp_trees:delete(X, Tree, DB),
    ?assertEqual(<<>>, aeu_mp_trees:get(X, Tree1, DB1)),
    ?assertNotEqual(aeu_mp_trees:root_hash(Tree),
                    aeu_mp_trees:root_hash(Tree1)),
    test_delete_all(Left, Tree1, DB1);
test_delete_all([], Tree, DB) ->
    {Tree, DB}.

%%%=============================================================================
%%% Hash tests

test_reversed_put() ->
    rand:seed(exs1024s, {143, 14132, 4163}),
    Vals = gen_vals(1000),
    T0  = aeu_mp_trees:new(),
    DB0 = dict:new(),
    {T1,_DB1} = insert_vals(Vals, T0, DB0),
    {T2,_DB2} = insert_vals(lists:reverse(Vals), T0, DB0),
    ?assertEqual(aeu_mp_trees:root_hash(T1),
                 aeu_mp_trees:root_hash(T2)).

test_put_and_delete() ->
    %% From an existing tree, add and delete some nodes and see that
    %% we arrive at the same hash again.
    {T0, DB0, _} = gen_mp_tree({345, 2345, 1234}, 1000),
    %%Vals = gen_vals(1000),
    Vals = gen_vals(1000),
    ?assertEqual([], [X || {X, _} <- Vals, aeu_mp_trees:get(X, T0, DB0) =/= <<>>]),
    {T1, DB1} = insert_vals(Vals, T0, DB0),
    {T2,_DB2} = delete_vals(Vals, T1, DB1),
    ?assertEqual(aeu_mp_trees:root_hash(T0), aeu_mp_trees:root_hash(T2)),

    {T3,_DB3} = delete_vals(lists:reverse(Vals), T1, DB1),
    ?assertEqual(aeu_mp_trees:root_hash(T0), aeu_mp_trees:root_hash(T3)),
    ok.

%%%=============================================================================
%%% Test utils

gen_mp_tree(Seed, NofNodes) ->
    rand:seed(exs1024s, Seed),
    Vals = gen_vals(NofNodes),
    ?assertEqual(length(Vals), length(lists:ukeysort(1, Vals))),
    {Tree, DB} = insert_vals(Vals, aeu_mp_trees:new(), dict:new()),
    {Tree, DB, Vals}.

gen_vals(NofNodes) ->
    [{random_hexstring(65), random_hexstring(8)}
     || _ <- lists:seq(1, NofNodes)].

insert_vals([{X, Y}|Left], T, DB) ->
    {T1, DB1} = aeu_mp_trees:put(X, Y, T, DB),
    insert_vals(Left, T1, DB1);
insert_vals([], T, DB) ->
    {T, DB}.

delete_vals([{X,_Y}|Left], T, DB) ->
    {T1, DB1} = aeu_mp_trees:delete(X, T, DB),
    delete_vals(Left, T1, DB1);
delete_vals([], T, DB) ->
    {T, DB}.

random_hexstring(N) when N >= 1 ->
    << <<(rand:uniform(15)):4>> || _ <- lists:seq(1, N) >>.
