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
            close(Props),
            {stop, normal, Props, LogProps};
        [{reserved, {buyer, _}},
         {reserved, {item, _}},
         {reserved, {seller, _}}] ->
            cancel(Props),
            {stop, normal, Props, LogProps};
        _ ->
            {Props, LogProps}
    end.

close(Props) ->
    Buyer = proplists:get_value(buyer, Props),
    Seller = proplists:get_value(seller, Props),
    Item = proplists:get_value(item, Props),
    Cost = proplists:get_value(cost, Props),

    egre:attempt(Buyer, {Buyer, spend, Cost, because, self()}, false),
    egre:attempt(Seller, {Seller, accrue, Cost, because, self()}, false),
    egre:attempt(Item, {Item, move, from, Seller, to, Buyer, because, self()}, false).

cancel(Props) ->
    [cancel(X, Props) || {reserve, {X, success}} <- Props].

cancel(Type, Props) ->
    Party = proplists:get_value(Type, Props),
    egre:attempt(Party, {unreserve, for, self()}).
