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
    case is_name(Props, SpellName) of
        true ->
            Log = [{?EVENT, inject_self},
                   {action, memorize},
                   {name, SpellName}],
            NewEvent = {Character, memorize, self()},
            ?RESEND_NOSUB(Character, NewEvent);
        _ ->
            {succeed, _Subscribe = false, Props}
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
