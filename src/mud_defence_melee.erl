%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_defence_melee).
-behaviour(mud_defence).

-export([should_defend/1]).

should_defend(Props) ->
    is_wielded(Props) andalso is_defence(Props).

is_defence(Props) ->
    true == proplists:get_value(is_defence, Props, false).

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    is_wielded(BodyPart, Props).

is_wielded({BodyPart, BodyPartType}, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    lists:member(BodyPartType, WieldingBodyParts);
is_wielded(_, _) ->
    false.
