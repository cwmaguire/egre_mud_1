%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_spell_memorize).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{character := Character},
         Props,
         {Character, memorize, Spell}}) when is_pid(Spell) ->
    Log = [{?SOURCE, Character},
           {?EVENT, memorize},
           {?TARGET, Spell},
           {spell, Spell}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, memorize, Self}}) when Self == self() ->
    Log = [{?EVENT, memorize},
           {?SOURCE, Character},
           {?TARGET, Self},
           {handler, ?MODULE}],
    Props2 = [{is_memorized, true} | proplists:delete(is_memorized, Props)],
    {Props2, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
