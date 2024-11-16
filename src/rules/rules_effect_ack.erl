%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_effect_ack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner}, Props, {Owner, registered, Self, effect}, _})
  when Self == self() ->
    Log = [{?EVENT, registered},
           {?SOURCE, Owner},
           {?TARGET, self()},
           ?RULES_MOD],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {_, init}, _}) ->
    Log = [{?SOURCE, self()},
           {?EVENT, init},
           {?TARGET, self()},
           ?RULES_MOD],
    Owner = proplists:get_value(owner, Props),
    % TODO: time out if we don't get the ack back, meaning we never got
    %       recorded in the character?
    egre:attempt(Owner, {Owner, register, self(), effect}, false),
    {Props, Log};
succeed({Props, {Owner, register, _Self, effect}, _}) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, registered},
           {?TARGET, self()},
           ?RULES_MOD],
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

