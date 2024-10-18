%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_life_attack).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include_lib("egre/include/egre.hrl").

%% We have been killed
attempt({#parents{owner = Owner}, Props, Msg = {Source, killed, Owner, with, _AttackVector}}) ->
    log([<<"attempt: ">>, Msg, <<", props: ">>, Props]),
                log([{stage, attempt},
                     {?EVENT, killed},
                     {object, self()},
                     {props, Props},
                     {?SOURCE, Source},
                     {?TARGET, Owner},
                     {message, Msg},
                     {sub, true}]),
    {succeed, _Subscribe = true, Props};

%% We have died
attempt({#parents{owner = Owner}, Props, Msg = {Owner, die}}) ->
    log([{stage, attempt},
         {?EVENT, die},
         {object, self()},
         {props, Props},
         {?TARGET, Owner},
         {message, Msg},
         {sub, true}]),
    {succeed, _Subscribe = true, Props};

%% Something is attack us and we are dead
attempt({#parents{owner = Owner}, Props, Msg = {Attacker, calc, Hit, on, Owner, with, AttackVector}}) ->
    log([{stage, attempt},
         {?EVENT, calc_hit},
         {object, self()},
         {props, Props},
         {?SOURCE, Attacker},
         {?TARGET, Owner},
         {hit, Hit},
         {attack_vector, AttackVector},
         {message, Msg},
         {sub, false}]),
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{owner = Owner},
         Props,
         Msg = {Attacker, calc, Types, damage, Damage, to, Owner, with, AttackVector}}) ->
    log([{stage, attempt},
         {?EVENT, calc_damage},
         {object, self()},
         {props, Props},
         {?SOURCE, Attacker},
         {?TARGET, Owner},
         {damage, Damage},
         {attack_vector, AttackVector},
         {types, Types},
         {message, Msg},
         {sub, false}]),
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{owner = Owner},
         Props,
         _Msg = {Self, attack, Attacker}})
  when Self == self() ->
    log([{stage, attempt},
         {?EVENT, attack},
         {?SOURCE, Attacker},
         {?TARGET, Owner}]),
    case proplists:get_value(is_alive, Props, false) of
        false ->
            {{fail, target_is_dead}, _Subscribe = false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{},
         Props,
         _Msg = {Searcher, search, Self}})
  when Self == self() ->
    log([{?EVENT, search},
         {?SOURCE, Searcher},
         {?TARGET, Self}]),
    case proplists:get_value(is_alive, Props, false) of
        true ->
            {{fail, target_is_alive}, _Subscribe = false, Props};
        _ ->
            {succeed, true, Props}
    end;

%% TODO fail everything when dead, e.g. move, wield, attack, etc.
%% Might be easier to list everything that _doesn't_ fail.

attempt(_) ->
    undefined.

succeed({Props, {Source, killed, Owner, with, AttackVector}}) ->
    log([{stage, succeed},
         {?EVENT, killed},
         {object, self()},
         {props, Props},
         {?SOURCE, Source},
         {?TARGET, Owner},
         {handler, ?MODULE},
         {attack_vector, AttackVector}]),
    egre_object:attempt(self(), {Owner, die}),
    Props;

succeed({Props, {Owner, die}}) ->
    log([{stage, succeed},
         {?EVENT, die},
         {object, self()},
         {props, Props},
         {?TARGET, Owner}]),
    CorpseCleanupMilis = application:get_env(mud, corpse_cleanup_milis, 10 * 60 * 1000),
    egre_object:attempt_after(CorpseCleanupMilis, self(), {Owner, cleanup}),
    lists:keystore(is_alive, 1, Props, {is_alive, false});

succeed({Props, _Msg}) ->
    throw(should_never_happen),
    Props.

fail({Props, _Reason, _Message}) ->
    throw(should_never_happen),
    Props.

log(Terms) ->
    egre_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
