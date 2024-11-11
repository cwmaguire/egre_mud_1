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
         {Source, attack, _TargetName, is, Owner, 'if', alive, 'not', _DeadChars},
         _}) ->
    Log = [{?SOURCE, Source},
           {?TARGET, Owner},
           {?EVENT, attack}],
    NewEvent = {Source, attack, Owner},
    #result{result = {resend, Source, NewEvent},
            subscribe = false,
            props = Props,
            log = Log};
attempt({#{owner := Owner},
         Props,
         {Source, attack, TargetName, 'not', DeadChars},
         _}) ->
    Log = [{?SOURCE, Source},
           {?TARGET, Owner},
           {?EVENT, attack}],
    NewEvent = {Source, attack, TargetName, 'not', [Owner | DeadChars]},
    #result{result = {resend, Source, NewEvent},
            subscribe = false,
            props = Props,
            log = Log};
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
