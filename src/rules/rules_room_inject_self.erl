%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(rules_room_inject_self).
-behaviour(egre_rules).
-compile({parse_transform, egre_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).


% attempt({_Parents,
%          Props,
%          {Char, enter_world, in, Room, with, Conn}})
%   when is_binary(Room) ->
%     case proplists:get_value(name, Props) of
%         Room ->
%             NewEvent = {Char, enter_world, in, self(), with, Conn},
%             {{resend, NewEvent, Char}, false, Props};
%         _ ->
%             {succeed, false, Props}
%     end;
attempt(_) ->
    undefined.

succeed(_) ->
    undefined.

fail(_) ->
    undefined.

%log(Terms) ->
    %egre_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
