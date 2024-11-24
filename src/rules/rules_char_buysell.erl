%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_buysell).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud_rules.hrl").


attempt({#{}, Props, {Self, buy, ItemName}, _}) when is_binary(ItemName) ->
    Log = [{?EVENT, buy},
           {?SOURCE, Self},
           {?TARGET, ItemName},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, buy, Item}, _}) when is_pid(Item) ->
    Log = [{?EVENT, buy},
           {?SOURCE, Self},
           {?TARGET, Item},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Character, buy, Item}, _})
  when Character /= self(),
       is_pid(Item) ->
    Log = [{?EVENT, buy},
           {?SOURCE, Character},
           {?TARGET, Item},
           ?RULES_MOD],
    case has_item(Props, Item) andalso (is_item_for_sale(Props, Item)) of
        true ->
            Cost = cost(Props, Item),
            NewEvent = {Character, buy, Item, from, self(), for, Cost},
            #result{result = {resend, Character, NewEvent},
                    subscribe = false,
                    props = Props,
                    log = Log};
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{}, Props, {Character, buy, Item, from, Seller, for, Cost}, _})
  when Character == self() ->
    Log = [{?EVENT, buy},
           {?SOURCE, Character},
           {?TARGET, Item},
           ?RULES_MOD],

    EscrowProperties = [{owner, self()},
                     {buyer, self()},
                     {seller, Seller},
                     {item, Item},
                     {cost, Cost},
                     ?ESCROW_RULES],
    {ok, Escrow} = supervisor:start_child(egre_object_sup, [undefined, EscrowProperties]),
    egre:attempt(Escrow, {Character, buy, Item, from, Seller, for, Cost, with, Escrow}, false),
    ?SUCCEED_NOSUB;
attempt({#{}, Props, {Self, reserve, Item, for, Escrow}, _})
  when Self == self(),
       is_pid(Item) ->
    Log = [{?EVENT, reerve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, reserve, Cost, for, Escrow}, _})
  when Self == self(),
       is_integer(Cost) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;

%% TODO: finish these
attempt({#{}, Props, {Self, spend, Cost, because, Escrow}, _})
attempt({#{}, Props, {Self, accrue, Cost, because, Escrow}, _})
attempt({#{}, Props, {unreserve, for, Escrow}, _})

attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

has_item(Props, Pid) ->
    lists:any(fun({item, Pid_}) when Pid_ == Pid ->
                      true;
                 (_) ->
                      false
              end,
              Props).

is_item_for_sale(Props, Item) ->
    lists:any(fun({cost, #{Item := _}}) -> true;
                 (_) -> false
              end,
              Props).

cost(Props, Item) ->
    #{Item := Cost} = proplists:get_value(cost, Props),
    Cost.
