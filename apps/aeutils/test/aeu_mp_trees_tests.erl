%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    Unit tests for Merkle Patricia Trees
%%% @end
%%%=============================================================================
-module(aeu_mp_trees_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {"Basic tests",
     [ {"Put", fun test_put/0}
     , {"Lookup", fun test_lookup/0}
     , {"Delete one", fun test_delete_one_by_one/0}
     , {"Delete all", fun test_delete_all/0}
     ]
    }.

test_put() ->
    {_Tree,_DB,_Vals} = gen_mp_tree({23, 123534, 345345}, 1000),
    ok.

test_lookup() ->
    {Tree, DB, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    %% aeu_mp_trees:pp(Tree, DB),
    [?assertEqual(Y, aeu_mp_trees:get(X, Tree, DB)) || {X, Y} <- Vals],
    ok.

test_delete_one_by_one() ->
    {Tree, DB, Vals} = gen_mp_tree({123, 1234, 1234123}, 29),
    [begin
         {T, DB1} = aeu_mp_trees:delete(X, Tree, DB),
         ?assertEqual(<<>>, aeu_mp_trees:get(X, T, DB1))
     end
     || {X,_Y} <- Vals],
    ok.

test_delete_all() ->
    {Tree, DB, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    {Tree1,_DB1} = test_delete_all(Vals, Tree, DB),
    ?assertEqual(aeu_mp_trees:root_hash(aeu_mp_trees:new()),
                 aeu_mp_trees:root_hash(Tree1)).


test_delete_all([{X, Y}|Left], Tree, DB) ->
    ?debugFmt("Deleting ~s", [hexstring(X)]),
    ?assertEqual(Y, aeu_mp_trees:get(X, Tree, DB)),
    {Tree1, DB1} = Acc = aeu_mp_trees:delete(X, Tree, DB),
    ?assertEqual(<<>>, aeu_mp_trees:get(X, Tree1, DB1)),
    ?assertNotEqual(aeu_mp_trees:root_hash(Tree),
                    aeu_mp_trees:root_hash(Tree1)),
    test_delete_all(Left, Tree1, DB1);
test_delete_all([], Tree, DB) ->
    {Tree, DB}.

gen_mp_tree(Seed, NofNodes) ->
    rand:seed(exs1024s, Seed),
    Vals = [{random_hexstring(65), random_hexstring(8)}
            || _ <- lists:seq(1, NofNodes)],
    ?assertEqual(length(Vals), length(lists:ukeysort(1, Vals))),
    {Tree, DB} = lists:foldl(fun({X, Y}, {T, D}) ->
                                     aeu_mp_trees:put(X, Y, T, D)
                             end,
                             {aeu_mp_trees:new(), dict:new()},
                             Vals),
    {Tree, DB, Vals}.

random_hexstring(N) when N >= 1 ->
    << <<(rand:uniform(15)):4>> || _ <- lists:seq(1, N) >>.

hexstring(Bin) ->
    [hexchar(X) || <<X:4>> <= Bin].

hexchar(X) when X > -1, X < 10 -> $0 + X;
hexchar(X) when X > -1, X < 16 -> $A + X - 10.
