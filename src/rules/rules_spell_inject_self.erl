%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_spell_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{character := Character},
         Props,
         {Character, memorize, SpellName},
         _})
  when is_binary(SpellName) ->
    Log = [{?EVENT, memorize},
           {?SOURCE, Character},
           {?TARGET, SpellName}],
    case is_name(Props, SpellName) of
        true ->
            NewEvent = {Character, memorize, self()},
            ?RESEND_NOSUB(Character, NewEvent);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{character := Character},
         Props,
         {Character, cast, SpellName, on, Target},
         _})
  when is_binary(SpellName) ->
    Log = [{?EVENT, cast},
           {?SOURCE, Character},
           {?TARGET, Target}],
    case is_name(Props, SpellName) of
        true ->
            NewEvent = {Character, cast, self(), on, Target},
            ?RESEND_NOSUB(Character, NewEvent);
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({#{character := Character},
         Props,
         {Character, cast, SpellName},
         _})
  when is_binary(SpellName) ->
    Log = [{?EVENT, cast},
           {?SOURCE, Character}],
    case is_name(Props, SpellName) of
        true ->
            DefaultTarget = default_target(Props),
            NewEvent = {Character, cast, self(), on, DefaultTarget},
            ?RESEND_NOSUB(Character, NewEvent, [{target, DefaultTarget} | Log]);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

is_name(Props, Name) ->
    SpellName = proplists:get_value(name, Props, ""),
    match == re:run(SpellName, Name, [{capture, none}]).

default_target(Props) ->
    case proplists:get_value(default_target, Props) of
        character ->
            proplists:get_value(character, Props);
        _ ->
            undefined
    end.
