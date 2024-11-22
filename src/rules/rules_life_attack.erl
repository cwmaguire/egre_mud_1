%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_life_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

%% We have been killed
attempt({#{owner := Owner},
         Props,
         {Source, killed, Owner, with, _AttackVector},
         _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Source},
           {?TARGET, Owner}],
    ?SUCCEED_SUB;

%% We have died
attempt({#{owner := Owner}, Props, Msg = {Owner, die}, _}) ->
    Log = [{stage, attempt},
           {?EVENT, die},
           {object, self()},
           {props, Props},
           {?TARGET, Owner},
           {message, Msg},
           {sub, true}],
    ?SUCCEED_SUB;

%% Something is attack us and we are dead
attempt({#{owner := Owner},
         Props,
         {Attacker, roll, Roll,
          for, hit,
          with, _EffectType,
          on, Defender,
          with,
          attack_source, AttackSource,
          effect, _Effect},
         _})
  when Attacker == Owner; Defender == Owner ->
    Log = [{?EVENT, roll_hit},
           {?SOURCE, Attacker},
           {?TARGET, Owner},
           ?RULES_MOD,
           {roll, Roll},
           {attack_source, AttackSource}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{owner := Owner},
         Props,
         _Msg = {Attacker, attack, Defender},
         _}) when Attacker == Owner; Defender == Owner->
    Log = [{stage, attempt},
           {?EVENT, attack},
           {?SOURCE, Attacker},
           {?TARGET, Defender}],
    case proplists:get_value(is_alive, Props, false) of
        false ->
            ?FAIL_NOSUB(target_is_dead);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{},
         Props,
         _Msg = {Searcher, search, Self},
         _})
  when Self == self() ->
    Log = [{?EVENT, search},
           {?SOURCE, Searcher},
           {?TARGET, Self}],
    case proplists:get_value(is_alive, Props, false) of
        true ->
            ?FAIL_NOSUB(target_is_alive);
        _ ->
            ?SUCCEED_SUB
    end;

%% TODO fail everything when dead, e.g. move, wield, attack, etc.
%% Might be easier to list everything that _doesn't_ fail.

attempt(_) ->
    undefined.

succeed({Props, {Self, init}, _}) ->
    Log = [{?EVENT, init},
           {?SOURCE, Self},
           {?TARGET, Self}],
    {Props, Log};

succeed({Props, {Source, killed, Owner, with, _AttackVector}, _}) ->
    Log = [{?EVENT, killed},
           {?SOURCE, Source},
           {?TARGET, Owner}],
    egre_object:attempt(self(), {Owner, die}),
    {Props, Log};

succeed({Props, {Owner, die}, _}) ->
    Log = [{?EVENT, die},
           {?TARGET, Owner},
           ?RULES_MOD],
    CorpseCleanupMilis = application:get_env(mud, corpse_cleanup_milis, 10 * 60 * 1000),
    egre_object:attempt_after(CorpseCleanupMilis, self(), {Owner, cleanup}),
    Props2 = lists:keystore(is_alive, 1, Props, {is_alive, false}),
    {Props2, Log};

succeed({Props, Msg, _Context}) ->
    ct:pal("~p rules_life_attack received unexpected succeed: ~p",
           [self(), Msg]),
    throw(should_never_happen),
    Props.

fail({Props, _Reason, _Message, _Context}) ->
    throw(should_never_happen),
    Props.
