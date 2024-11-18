%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_resource_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

%% @doc
%% Captures reservation events reserving this process' character's resource
%% by name and then resends it with this processes pid instead.

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% If something reserves this resource type for this character then we
%% need to inject ourself
attempt({#{owner := Owner,
           type := ResourceType},
         Props,
         {Owner, reserve, Amt,
          'of', ResourceType,
          for, AttackVector,
          for, Purpose,
          Times, times},
         _})
  when is_atom(ResourceType) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, reserve},
           {?TARGET, self()},
           ?RULES_MOD,
           {amount, Amt},
           {resource_type, ResourceType},
           {vector, AttackVector}],
    NewEvent = {Owner, reserve, Amt,
                'of', self(),
                for, AttackVector,
                for, Purpose,
                Times, times},
    ?RESEND_NOSUB(Owner, NewEvent);
attempt({#{owner := Owner},
         Props,
         {Owner, unreserve, ResourceType, for, AttackVector},
         _}) when is_atom(ResourceType) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, unreserver},
           ?RULES_MOD,
           {resource_type, ResourceType},
           {vector, AttackVector}],
    case proplists:get_value(type, Props) of
        ResourceType ->
            NewEvent = {Owner, unreserve, self(), for, AttackVector},
            ?RESEND_SUB(Owner, NewEvent);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
