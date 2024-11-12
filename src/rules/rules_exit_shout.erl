%% Copyright 2024, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_exit_shout).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{}, Props, {Char, shouts, _Phrase, in, Room}, _}) ->
    Log = [{?EVENT, shouts},
           {?SOURCE, Char},
           {?TARGET, Room}],
    %% If we are an exit for Room, then send shout to other Room
    case lists:any(fun({{room, _ExitName}, Room_}) when Room_ == Room ->
                           true;
                      (_) ->
                           false
                   end,
                   Props) of
        true ->
            ?SUCCEED_SUB;
        _ ->
            undefined
    end;
attempt(_) ->
    undefined.

succeed({Props, {Char, shouts, Phrase, in, Room}, _}) ->
    Log = [{?SOURCE, Char},
           {?EVENT, shouts},
           {?TARGET, Room}],
    ExitName = room_exit(Room, Props),
    OtherRoom = other_room(Room, Props),
    egre:attempt(self(), {Char, shouts, Phrase, to, OtherRoom, from, ExitName}),
    {Props, Log};
succeed({Props, _, _}) ->
    Props.

fail({Props, _, _, _}) ->
    Props.

other_room(Room, Props) ->
    [{{room, _ExitName}, OtherRoom}] =
        lists:filter(fun({{room, _ExitName}, Room_})
                           when Room_ /= Room ->
                             true;
                        (_) ->
                             false
                     end,
                     Props),
    OtherRoom.

room_exit(Room, Props) ->
    [{{room, ExitName}, _}] =
        lists:filter(fun({{room, _ExitName}, Room_})
                           when Room_ == Room ->
                             true;
                        (_) ->
                             false
                     end,
                     Props),
    ExitName.
