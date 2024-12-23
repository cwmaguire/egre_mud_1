%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_char_look).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Source, look, TargetName}, _})
  when Source =/= self(),
       is_binary(TargetName) ->
    Log = [{?EVENT, look},
           {?SOURCE, Source}],
    SelfName = proplists:get_value(name, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}, caseless]) of
        match ->
            NewEvent = {Source, look, self()},
            #result{result = {resend, Source, NewEvent},
                    subscribe = ignored,
                    props = Props,
                    log = [{?TARGET, self()} | Log]};
        _ ->
            #result{result = succeed,
                    subscribe = false,
                    props = Props,
                    log = [{?TARGET, TargetName} | Log]}
    end;
attempt({#{owner := Room},
         Props,
         {Self, look},
         _})
  when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, look},
           {?TARGET, Room}],
    NewEvent = {Self, look, Room},
    #result{result = {resend, Self, NewEvent},
            subscribe = ignored,
            props = Props,
            log = Log};
attempt({#{},
         Props,
         {Source, look, Self},
         _}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, look},
           {?TARGET, Self}],
    ?SUCCEED_SUB;
attempt({#{owner := OwnerRoom},
         Props,
         _DescFromParent = {Source, describe, OwnerRoom, with, RoomContext}, _}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, OwnerRoom},
           {context, RoomContext}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Source, look, Self}, _}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, look},
           {?TARGET, Self}],
    NoContext = <<>>,
    describe(Source, Props, NoContext, deep),
    {Props, Log};
succeed({Props, {Source, describe, Target, with, Context}, _}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Target},
           {context, Context}],
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context, shallow);
            _ ->
                ok
        end,
    {Props, Log};
succeed(_) ->
    undefined.

fail(_) ->
    undefined.

describe(Source, Props, Context, shallow) ->
    send_description(Source, Props, Context);
describe(Source, Props, Context, deep) ->
    send_description(Source, Props, Context),
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    egre_object:attempt(Source, {Source, describe, self(), with, NewContext}).

send_description(Source, Props, Context) ->
    Description = mud_util:describe(character, Props),
    egre_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.
