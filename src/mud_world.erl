%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_world).

-include("play_world.hrl").

-export([init/0]).
-export([move/1]).
-export([m/1]).
-export([s/0]).
-export([t/0]).

init() ->
    IdPids = [{Id, start(Id, Props)} || {Id, Props} <- ?WORLD],
    _Objs = [egre_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    IdPids.

start(Id, Props) ->
    {ok, Pid} = supervisor:start_child(mud_object_sup, [Id, Props]),
    Pid.

move(IdPids) ->
    Room1 = proplists:get_value(room_5_5, IdPids),
    Room2 = proplists:get_value(room_5_4, IdPids),
    Player1 = proplists:get_value(player, IdPids),
    Procs = {procs, undefined, [], [], []},
    gen_server:cast(Room1, {attempt, {move, Player1, Room1, Room2}, Procs}).

m(Dir) ->
    Player1 = whereis(player1),
    gen_server:cast(Player1, {attempt, {move, Player1, Dir}, {procs, undefined, [], [], []}}).

s() ->
    Ids = [{Id, egre_index:get_pid(Id)} || {Id, _Prop} <- ?WORLD],
    [io:format("~p: ~p~n", [Id, st(Pid)]) || {Id, Pid} <- Ids].

st(Process) ->
    sys:get_state(Process).

t() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, [call]),
    dbg:tpl(mud_object, [{'_',[],[{return_trace}]}]).
