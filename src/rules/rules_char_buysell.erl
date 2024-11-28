%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_buysell).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud_rules.hrl").


attempt({#{}, Props, {Self, buy, ItemName}, _})
  when is_binary(ItemName),
       Self == self() ->
    Log = [{?EVENT, buy},
           {?SOURCE, Self},
           {?TARGET, ItemName},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, buy, Item}, _})
  when is_pid(Item),
       Self == self() ->
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

    case proplists:get_value(money, Props) of
        NotEnough when NotEnough < Cost ->
            #result{result = {fail, insufficient_funds},
                    subscribe = true,
                    props = Props,
                    log = Log};
        _ ->
            EscrowProperties = [{owner, self()},
                                {buyer, self()},
                                {seller, Seller},
                                {item, Item},
                                {cost, Cost},
                                ?ESCROW_RULES],
            {ok, Escrow} = supervisor:start_child(egre_object_sup, [undefined, EscrowProperties]),
            egre:attempt(Escrow, {Character, buy, Item, from, Seller, for, Cost, with, Escrow}, false),
            ?SUCCEED_NOSUB
    end;
attempt({#{}, Props, {Self, reserve, Item, for, Escrow}, _})
  when Self == self(),
       is_pid(Item) ->
    Log = [{?EVENT, reerve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    HasItem = lists:member({item, Item}, Props),
    IsItemReserved = lists:member({reserve, {Escrow, Item}}, Props),
    case {HasItem, IsItemReserved} of
        {false, _} ->
            ?FAIL_NOSUB(missing_item);
        {_, true} ->
            ?FAIL_NOSUB(item_reserved);
        _ ->
            Props2 = [{reserve, {Escrow, Item}} | Props],
            #result{props = Props2, log = Log}
    end;
attempt({#{}, Props, {Self, reserve, Cost, for, Escrow}, _})
  when Self == self(),
       is_integer(Cost) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    case proplists:get_value(money, Props) of
        NotEnough when NotEnough < Cost ->
            ?FAIL_NOSUB(insufficient_funds);
        Money ->
            Props2 = [{reserve, {Escrow, Cost}} | Props],
            Props3 = lists:keyreplace(money, 1, Props2, {money, Money - Cost}),
            #result{props = Props3,
                    log = Log}
    end;
attempt({#{}, Props, {Self, spend, _Cost, because, Escrow}, _}) 
  when Self == self() ->
    Log = [{?EVENT, spend},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, accrue, _Cost, because, Escrow}, _}) 
  when Self == self() ->
    Log = [{?EVENT, accrue},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {unreserve, for, Escrow}, _}) ->
    Log = [{?EVENT, unreserve},
           {?SOURCE, Escrow},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;

attempt(_) ->
    undefined.

succeed({Props, {Self, buy, ItemName}, _}) when is_binary(ItemName) ->
    Log = [{?EVENT, buy},
           {?SOURCE, Self},
           {?TARGET, ItemName},
           ?RULES_MOD],
    egre:attempt(Self, {send, self(), <<"Item ", ItemName/binary, " doesn't exist">>}),
    {Props, Log};
succeed({Props, {Self, spend, Cost, because, Escrow}, _}) ->
    Log = [{?EVENT, spend},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    Props2 = lists:delete({reserve, {Escrow, Cost}}, Props),
    {Props2, Log};
succeed({Props, {Self, accrue, Cost, because, Escrow}, _}) ->
    Log = [{?EVENT, spend},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    Props2 = lists:filter(fun({reserve, {Escrow_, _Item}})
                                when Escrow_ == Escrow ->
                                  false;
                             (_) ->
                                  true
                          end,
                          Props),
    Money = proplists:get_value(money, Props2),
    Props3 = lists:keyreplace(money, 1, Props2, {money, Money + Cost}),
    {Props3, Log};
succeed({Props, {unreserve, for, Escrow}, _}) ->
    Log = [{?EVENT, unreserve},
           {?SOURCE, Escrow},
           {?TARGET, self()},
           ?RULES_MOD],
    [{reserve, {_, ItemOrCost}}] =
        lists:filter(fun({reserve, {Escrow_, _}})
                           when Escrow_ == Escrow ->
                             true;
                        (_) ->
                             false
                     end,
                     Props),
    Props2 =
        case ItemOrCost of
            Cost when is_integer(Cost) ->
                unreserve_cost(Escrow, Cost, Props);
            Item when is_pid(Item) ->
                unreserve_item(Escrow, Item, Props)
        end,
    {Props2, Log};

succeed(_) ->
    undefined.
fail({Props, insufficient_funds, {Self, buy, Item, from, _Seller, for, Cost}, _}) ->
    Log = [{?EVENT, buy},
           {?SOURCE, Self},
           {?TARGET, Item},
           ?RULES_MOD],
    CostBin = integer_to_binary(Cost),
    egre:attempt(Item, {Item, send, Self, <<"Cannot afford cost of ", CostBin/binary, " for ">>}),
    {Props, Log};
fail({Props, _Reason, {Self, reserve, Cost, for, Escrow}, _})
  when is_integer(Cost) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    Props2 = unreserve_cost(Escrow, Cost, Props),
    {Props2, Log};
fail({Props, _Reason, {Self, reserve, Item, for, Escrow}, _}) ->
    Log = [{?EVENT, reserve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    Props2 = unreserve_item(Escrow, Item, Props),
    {Props2, Log};
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

unreserve_cost(Escrow, Cost, Props) ->
    Props2 = lists:delete({reserve, {Escrow, Cost}}, Props),
    Money = proplists:get_value(money, Props2),
    lists:keyreplace(money, 1, Props2, {money, Money + Cost}).

unreserve_item(Escrow, Item, Props) ->
    lists:delete({reserve, {Escrow, Item}}, Props).
