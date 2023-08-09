%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_data).

-export([fetch/1]).

fetch(player1) ->
    {mud_player, player1, [{room, room1}, {item, item1}]};
fetch(_) ->
    undefined.
