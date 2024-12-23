%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_search).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{}, Props, {Player, search, Self}, _}) when Self == self() ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Self}],
    Name = proplists:get_value(name, Props, "MissingName"),
    NewEvent = {Player, search, Self, named, Name, with, body_parts, []},
    #result{result = {resend, Self, NewEvent},
            subscribe = true,
            props = Props,
            log = Log};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

%% TODO
%% Implement search detection: i.e. a character detects a player searching them
%% (and eventually other NPC characters searching them)
fail({Props,
      character_detected_search,
      {Player, search, Self, named, _Name, with, body_parts, _BodyParts},
      _}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Self},
           {reason, character_detected_search}],
    counter_attack(Player, Self, Props),
    {Props, Log};
fail({Props, _Result, {Player, search, Self}, _}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Self}],
    {Props, Log};
fail({Props, _Result, _Msg, _}) ->
    Log = [{?EVENT, search},
           {?TARGET, self()}],
    {Props, Log}.

counter_attack(Player, Self, Props) ->
    case proplists:get_value(is_attacking, Props) of
        true ->
            ok;
        _ ->
            egre_object:attempt(Self, {Self, attack, Player})
    end.
