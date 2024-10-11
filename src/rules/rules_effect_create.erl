%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_effect_create).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include_lib("egre/include/egre.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATTEMPT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attempt({#parents{owner = Attack,
                  character = Character},
         Props,
         {Character, affect, Target, because, Attack}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target},
           {attack, Attack}],
    {succeed, true, Props, Log};
attempt(_Other = {_, _, _Msg}) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUCCEED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succeed({Props, {Character, affect, Target, because, Attack}}) ->
    Log = [{?EVENT, attack},
           {?SOURCE, Character},
           {?TARGET, Target},
           {handler, ?MODULE},
           {attack, Attack}],
    ChildProps = replace_handlers_with_child_handlers(Props),
    ChildPropsWithTarget = [{target, Target} | ChildProps],
    {ok, Pid} = supervisor:start_child(egre_object_sup, [undefined, ChildPropsWithTarget]),
    egre_object:attempt(Pid, {Pid, affect, Target}),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

replace_handlers_with_child_handlers(Props) ->
    lists:keystore(handlers, 1, Props, proplists:get_value(child_handlers, Props)).
