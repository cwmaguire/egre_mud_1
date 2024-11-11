%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_effect_create).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{owner := Attack,
           character := Character},
         Props,
         {Character, affect, Target, because, Attack},
         _}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target},
           {attack, Attack}],
    ?SUCCEED_SUB;
attempt(_Other = {_, _, _Msg, _}) ->
    undefined.

succeed({Props, {Character, affect, Target, because, Attack}, _}) ->
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {handler, ?MODULE},
           {attack, Attack}],
    ChildProps = replace_rules_with_child_rules(Props),
    ChildPropsWithTarget = [{target, Target} | ChildProps],
    {ok, Pid} = supervisor:start_child(egre_object_sup, [undefined, ChildPropsWithTarget]),
    egre_object:attempt(Pid, {Pid, affect, Target}),
    {Props, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

replace_rules_with_child_rules(Props) ->
    lists:keystore(rules, 1, Props, proplists:get_value(child_rules, Props)).
