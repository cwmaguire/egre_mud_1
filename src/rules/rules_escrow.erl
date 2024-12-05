%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_escrow).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud_rules.hrl").

attempt({#{}, Props, {Buyer, buy, Item, from, _Seller, for, _Cost, with, Self}, _})
  when Self == self() ->
    Log = [{?EVENT, buy},
           {?SOURCE, Buyer},
           {?TARGET, Item},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {unreserve, for, Self}, _})
  when Self == self() ->
    Log = [{?EVENT, unreserve},
           {?SOURCE, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.


succeed({Props, {Buyer, buy, Item, from, Seller, for, Cost, with, Self}, _}) ->
    Log = [{?EVENT, buy},
           {?SOURCE, Buyer},
           {?TARGET, Item},
           ?RULES_MOD],
    egre:attempt(Buyer, {Buyer, reserve, Cost, for, Self}),
    egre:attempt(Seller, {Seller, reserve, Item, for, Self}),
    egre:attempt(Item, {Item, reserve, self, for, Self}),
    {Props, Log};
succeed({Props, {Character, reserve, Cost, for, _Self}, _})
  when is_integer(Cost) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Character},
           {?TARGET, Cost},
           ?RULES_MOD],
    Props2 = [{reserved, {buyer, success}} | Props],
    maybe_resolve(Props2, Log);
succeed({Props, {Item, reserve, self, for, _Self}, _}) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Item},
           {?TARGET, Item},
           ?RULES_MOD],
    Props2 = [{reserved, {item, success}} | Props],
    maybe_resolve(Props2, Log);
succeed({Props, {Seller, reserve, Item, for, _Self}, _}) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Seller},
           {?TARGET, Item},
           ?RULES_MOD],
    Props2 = [{reserved, {seller, success}} | Props],
    maybe_resolve(Props2, Log);
    %{Props2, Log};

succeed({Props, {unreserve, for, Self}, _}) ->
    Log = [{?EVENT, unreserve},
           {?SOURCE, Self},
           ?RULES_MOD],
    {stop, finished, Props, Log};
succeed({Props, {Buyer, spend, _, because, _Self}, _}) ->
    Log = [{?EVENT, spend},
           {?SOURCE, self()},
           {?TARGET, Buyer},
           ?RULES_MOD],

    IsMoneyAccrued = proplists:get_value(is_money_accrued, Props, false),
    IsItemMoved = proplists:get_value(is_item_moved, Props, false),
    case IsMoneyAccrued andalso IsItemMoved of
        true ->
            {stop, finished, [{is_money_spent, true} | Props], Log};
        false ->
            {[{is_money_spent, true} | Props], Log}
    end;
succeed({Props, {Seller, accrue, _Cost, because, _Self}, _}) ->
    Log = [{?EVENT, accrue},
           {?SOURCE, self()},
           {?TARGET, Seller},
           ?RULES_MOD],

    IsMoneySpent = proplists:get_value(is_money_spent, Props, false),
    IsItemMoved = proplists:get_value(is_item_moved, Props, false),
    case IsMoneySpent andalso IsItemMoved of
        true ->
            {stop, finished, [{is_money_accrued, true} | Props], Log};
        false ->
            {[{is_money_accrued, true} | Props], Log}
    end;
succeed({Props, {Item, move, from, _Seller, to, _Buyer, because, _Self}, _}) ->
    Log = [{?EVENT, accrue},
           {?SOURCE, self()},
           {?TARGET, Item},
           ?RULES_MOD],

    IsMoneySpent = proplists:get_value(is_money_spent, Props, false),
    IsMoneyAccrued = proplists:get_value(is_money_accrued, Props, false),
    case IsMoneySpent andalso IsMoneyAccrued of
        true ->
            {stop, finished, [{is_item_moved, true} | Props], Log};
        false ->
            {[{is_is_item_moved, true} | Props], Log}
    end;
succeed(_) ->
    undefined.

fail({Props, _Reason, {Something, reserve, Target, for, _}, _}) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Something},
           {?TARGET, Target},
           ?RULES_MOD],
    Buyer = proplists:get_value(buyer, Props),
    Seller = proplists:get_value(seller, Props),
    Item = proplists:get_value(item, Props),

    Props2 =
        case Something of
            Buyer ->
                Character = Something,
                Cost = integer_to_binary(Target),
                egre:attempt(Item, {Item, send, Character, <<"Cannot afford cost of ", Cost/binary, " for ">>}),
                [{reserved, {buyer, fail}} | Props];
            Seller ->
                [{reserved, {seller, fail}} | Props];
            Item ->
                [{reserved, {item, fail}} | Props]
        end,

    maybe_resolve(Props2, Log);
fail(_) ->
    undefined.

maybe_resolve(Props, LogProps) ->
    case lists:sort(proplists:lookup_all(reserved, Props)) of
        [{reserved, {buyer, success}},
         {reserved, {item, success}},
         {reserved, {seller, success}}] ->
            close(Props);
        [{reserved, {buyer, _}},
         {reserved, {item, _}},
         {reserved, {seller, _}}] ->
            cancel(Props);
        _ ->
            ok
    end,
    {Props, LogProps}.

close(Props) ->
    Buyer = proplists:get_value(buyer, Props),
    Seller = proplists:get_value(seller, Props),
    Item = proplists:get_value(item, Props),
    Cost = proplists:get_value(cost, Props),

    egre:attempt(Buyer, {Buyer, spend, Cost, because, self()}),
    egre:attempt(Seller, {Seller, accrue, Cost, because, self()}),
    egre:attempt(Item, {Item, move, from, Seller, to, Buyer, because, self()}).

cancel(Props) ->
    case [X || {reserved, {X, success}} <- Props] of
        [Type | _] ->
            AnyParty = proplists:get_value(Type, Props),
            egre:attempt(AnyParty, {unreserve, for, self()});
        _ ->
            ok
    end.
