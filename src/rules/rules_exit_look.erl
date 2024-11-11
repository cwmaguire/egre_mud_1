%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_exit_look).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-include("mud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#{owner := Owner},
         Props,
         {Source, describe, Room, with, Context},
         _}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Owner},
           {room, Room},
           {context, Context}],
    ShouldSub = has_room(Props, Room),
    ?SUCCEED_MAYBE_SUB(ShouldSub);
attempt(_) ->
    undefined.

succeed({Props, {Source, describe, Room, with, Context}, _}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Room},
           {room, Room},
           {context, Context}],
    describe(Source, Props, Room, Context),
    {Props, Log};
succeed(_) ->
    undefined.

-spec fail({proplist(), any(), tuple()}) -> {proplist(), proplist()}.
fail(_) ->
    undefined.

has_room(Props, Room) ->
    lists:any(is_exit_room_fun(Room), Props).

is_exit_room_fun(Room) ->
  fun ({{room, _}, Room_}) when Room == Room_ ->
      true;
      (_) ->
      false
  end.

describe(Source, Props, Room, Context) ->
    Exits = other_exits(Props, Room),
    ExitsBin = [atom_to_binary(E, utf8) || E <- Exits],
    ExitsDesc = join(<<",">>, ExitsBin),
    Desc = <<Context/binary,
             "exits ",
             ExitsDesc/binary>>,
    egre_object:attempt(Source, {send, Source, Desc}).

other_exits(Props, Room) ->
    [Dir || {{room, Dir}, NotRoom} <- Props, Room /= NotRoom].

join(Sep, Bins) ->
    join(Sep, Bins, <<>>).

join(_Sep, [], Joined) ->
    Joined;
join(Sep, [Bin | Bins], <<>>) ->
    join(Sep, Bins, <<Bin/binary>>);
join(Sep, [Bin | Bins], Joined) ->
    join(Sep, Bins, <<Joined/binary, Sep, Bin/binary>>).
