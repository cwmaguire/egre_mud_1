%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_body_part_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Source, look, TargetName}, _})
  when is_binary(TargetName) ->
    ct:pal("Running body part inject self handler: ~p ~p ~p", [Source, look, TargetName]),
    Log = [{?SOURCE, Source},
           {?EVENT, look}],
    case is_match(Props, TargetName) of
        true ->
            Log2 = [{?TARGET, self()} | Log],
            NewEvent = {Source, look, self()},
            Result = {resend, Source, NewEvent},
            #result{result = Result,
                    subscribe = false,
                    props = Props,
                    log = Log2};
        _ ->
            Log2 = [{?TARGET, TargetName} | Log],
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = Log2}
    end;
attempt({#{owner := Owner},
         Props,
         {Item, move, from, Owner, to, BodyPartName}, _})
  when is_binary(BodyPartName) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner}],
    case is_match(Props, BodyPartName) of
        true ->
            NewEvent = {Item, move, from, Owner, to, self()},
            Result = {resend, Owner, NewEvent},
            #result{result = Result,
                    subscribe = true,
                    props = Props,
                    log = [{?TARGET, self()} | Log]};
        _ ->
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = [{?TARGET, BodyPartName} | Log]}
    end;
attempt({#{owner := Owner},
         Props,
         {Item, move, from, BodyPartName, to, Owner}, _})
  when is_pid(Item) andalso
       is_binary(BodyPartName) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?TARGET, Owner}],
    case is_match(Props, BodyPartName) of
        true ->
            Log2 = [{?SOURCE, self()} | Log],
            NewEvent = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewEvent},
            {Result, _Subscribe = true, Props, Log2};
        _ ->
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = [{?SOURCE, BodyPartName} | Log]}
    end;
attempt({#{owner := Owner},
         Props,
         {Item, move, from, current_body_part, to, Owner}, _}) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?TARGET, Owner}],
    case [Item_ || {item, {Item_, _Ref}} <- Props, Item_ == Item] of
        [_ | _] ->
            NewEvent = {Item, move, from, self(), to, Owner},
            #result{result = {resend, Owner, NewEvent},
                    subscribe = true,
                    props = Props,
                    log = [{?SOURCE, self()} | Log]};
        _ ->
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = [{?SOURCE, current_body_part} | Log]}
    end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

is_match(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, <<>>), Name, [{capture, none}, caseless]).
