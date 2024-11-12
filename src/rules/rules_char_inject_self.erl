%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Source, attack, TargetName}, _})
  when is_binary(TargetName),
       Source /= self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, attack},
           {?TARGET, TargetName}],
    case is_name(Props, TargetName)  of
        true ->
            NewEvent = {Source, attack, TargetName, is, self(), 'if', alive, 'not', []},
            ?RESEND_SUB(Source, NewEvent);
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{}, Props, {Source, attack, TargetName, 'not', DeadChars}, _})
  when is_binary(TargetName),
       Source /= self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, attack},
           {?TARGET, TargetName}],
    case is_name(Props, TargetName) and not lists:member(self(), DeadChars)  of
        true ->
            NewEvent = {Source, attack, TargetName, is, self(), 'if', alive, 'not', DeadChars},
            #result{result = {resend, Source, NewEvent},
                    subscribe = true,
                    props = Props,
                    log = Log};
        _ ->
            ?SUCCEED_NOSUB
    end;
attempt({#{}, Props, {Source, Action, TargetName}, _})
  when is_binary(TargetName) andalso
      (Action == look orelse
       Action == search) ->
    Log = [{?SOURCE, Source},
           {?EVENT, Action}],
    case is_name(Props, TargetName) andalso not attacking_self(Action, Source) of
        true ->
            NewEvent = {Source, Action, self()},
            #result{result = {resend, Source, NewEvent},
                    subscribe = true,
                    props = Props,
                    log = [{?TARGET, self()} | Log]};
        _ ->
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = [{?TARGET, TargetName} | Log]}
    end;
attempt({#{owner := Owner}, Props, {Self, look, _}, _}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, look}],
    NewEvent = {Self, look, Owner},
    ?RESEND_NOSUB(Self, NewEvent);
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

is_name(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}, caseless]).

attacking_self(attack, Self) when Self == self() ->
    true;
attacking_self(_, _) ->
    false.
