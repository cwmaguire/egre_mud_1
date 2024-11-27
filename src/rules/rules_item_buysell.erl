%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_item_buysell).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud_rules.hrl").

attempt({#{}, Props, {Self, reserve, self, for, Escrow}, _})
  when Self == self() ->
    Log = [{?EVENT, reerve},
           {?SOURCE, Escrow},
           {?TARGET, Self},
           ?RULES_MOD],
    case lists:member({reserve, Escrow}, Props) of
        true ->
            ?FAIL_NOSUB(already_reserved);
        _ ->
            Props2 = [{reserve, Escrow} | Props],
            #result{props = Props2, log = Log}
    end;
attempt({#{}, Props, {unreserve, for, Escrow}, _}) ->
    Log = [{?EVENT, unreserve},
           {?SOURCE, Escrow},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt({#{}, Props, {Self, move, from, _Seller, to, _Buyer, because, Escrow}, _}) 
  when Self == self() ->
    Log = [{?EVENT, move},
           {?SOURCE, Escrow},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_Self, move, from, Seller, to, Buyer, because, Escrow}, _}) ->
    Log = [{?EVENT, move},
           {?SOURCE, Buyer},
           {?TARGET, Seller},
           ?RULES_MOD],
    Props2 = lists:delete({reserve, Escrow}, Props),
    egre:attempt(self(), {self(), move, from, Seller, to, Buyer}),
    {Props2, Log};
succeed({Props, {unreserve, for, Escrow}, _}) ->
    unreserve(Escrow, Props);
succeed(_) ->
    undefined.

fail({Props, _Reason, {_Self, reserve, self, for, Escrow}, _}) ->
    unreserve(Escrow, Props);
fail(_) ->
    undefined.

unreserve(Escrow, Props) ->
    Log = [{?EVENT, unreserve},
           {?SOURCE, Escrow},
           {?TARGET, self()},
           ?RULES_MOD],
    Props2 = lists:delete({reserve, Escrow}, Props),
    {Props2, Log}.
