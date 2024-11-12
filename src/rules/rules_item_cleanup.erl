%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_item_cleanup).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("mud.hrl").

attempt({#{},
         Props,
         {Character, cleanup,  body_parts, _BodyParts, in, Room},
         _}) ->
    Log = [{?TARGET, Character},
           {?EVENT, cleanup},
           {room, Room}],
    ?SUCCEED_SUB;
attempt(_) ->
    undefined.

succeed({Props, {Character, cleanup, body_parts, BodyParts, in, Room}, _}) ->
    Log = [{?SOURCE, Character},
           {?TARGET, Room},
           {?EVENT, cleanup}],
    Owner = proplists:get_value(owner, Props),
    case lists:member(Owner, [Character | BodyParts]) of
        true -> egre_object:attempt(self(),
                                         {self(), move, from, Owner, to, Room},
                                         _ShouldSubscribe = false);
        _ ->
            ok
    end,
    {Props, Log};

succeed(_) ->
    undefined.

fail(_) ->
    undefined.
