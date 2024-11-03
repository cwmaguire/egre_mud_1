%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_life_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner,
           is_alive := true},
         Props,
         {Source, attack, _TargetName, is, Owner, 'if', alive, 'not', _DeadChars}}) ->
    Log = [{?SOURCE, Source},
           {?TARGET, Owner},
           {?EVENT, attack}],
    NewMessage = {Source, attack, Owner},
    Result = {resend, Source, NewMessage},
    {Result, false, Props, Log};
attempt({#{owner := Owner},
         Props,
         {Source, attack, TargetName, 'not', DeadChars}}) ->
    Log = [{?SOURCE, Source},
           {?TARGET, Owner},
           {?EVENT, attack}],
    NewMessage = {Source, attack, TargetName, 'not', [Owner | DeadChars]},
    Result = {resend, Source, NewMessage},
    {Result, false, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
