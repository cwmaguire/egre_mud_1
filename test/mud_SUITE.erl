%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_SUITE).
-compile(export_all).

-include("mud_test_worlds.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(WAIT100, receive after 100 -> ok end).
-define(WAIT1000, receive after 1000 -> ok end).

% TODO test updating a skill when a target is killed with a weapon (or when damage is dealt, or both)

%all() -> [player_attack].
%all() ->
    %[player_resource_wait,
     %player_move].
all() ->
    [player_move,
     player_move_fail,
     player_move_exit_locked,
     player_get_item,
     player_drop_item,
     character_owner_add_remove,
     player_attack,
     player_resource_wait,
     attack_with_modifiers,
     one_sided_fight,
     counterattack_behaviour,
     stop_attack_on_move,
     player_wield,
     player_wield_first_available,
     player_wield_missing_body_part,
     player_wield_wrong_body_part,
     player_wield_body_part_is_full,
     player_remove,
     look_player,
     look_room,
     look_item,
     set_character,
     cast_spell,
     decompose,
     search_character].

init_per_testcase(_, Config) ->
    %egre_dbg:add(egre_object),
    %egre_dbg:add(egre_event_log, add_index_details),

    %dbg:tracer(),
    %dbg:tpl(egre_event_log, '_', '_', [{'_', [], [{exception_trace}]}]),

    Port = ct:get_config(port),
    application:load(egremud),
    application:set_env(egremud, port, Port),
    {ok, _Started} = application:ensure_all_started(mud),
    {atomic, ok} = mnesia:clear_table(object),
    {ok, _Pid} = egremud_test_socket:start(),
    TestObject = spawn_link(fun mock_object/0),
    egre_index:put([{pid, TestObject}, {id, test_object}]),
    [{test_object, TestObject} | Config].

end_per_testcase(_, _Config) ->
    ct:pal("~p stopping egre_mud~n", [?MODULE]),
    receive after 1000 -> ok end,
    egremud_test_socket:stop(),
    application:stop(egre_mud).


all_vals(Key, Obj) ->
    Props = case get_props(Obj) of
                undefined ->
                    [];
                Props_ ->
                    Props_
            end,
    proplists:get_all_values(Key, Props).

val(Key, Obj) ->
    case all_vals(Key, Obj) of
        [First | _] ->
            First;
        _ ->
            []
    end.

all(Key, Obj) ->
    proplists:get_all_values(Key, get_props(Obj)).

has(Val, Obj) ->
    false /= lists:keyfind(Val, 2, get_props(Obj)).

get_props(undefined) ->
    [];
get_props(Obj) when is_atom(Obj) ->
    Pid = get_pid(Obj),
    get_props(Pid);
get_props(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            {_RecordName, Props} = sys:get_state(Pid),
            Props;
        false ->
            undefined
    end.

player_move(Config) ->
    %egre_dbg:add(mud, start_obj),
    %egre_dbg:add(egre_object, populate),
    %egre_dbg:add(egre_object, handle_cast_),
    start(?WORLD_1),
    Player = get_pid(player),
    RoomNorth =  get_pid(room_nw),
    RoomSouth =  get_pid(room_s),

    ?assertMatch(RoomNorth, val(owner, Player)),
    attempt(Config, Player, {Player, move, s}),
    ?WAIT100,
    ?assertMatch(RoomSouth, val(owner, Player)).

player_move_fail(Config) ->
    start(?WORLD_1),
    Player = get_pid(player),
    RoomNorth =  get_pid(room_nw),
    ?assertMatch(RoomNorth, val(owner, Player)),
    attempt(Config, Player, {Player, move, non_existent_exit}),
    ?WAIT100,
    ?assertMatch(RoomNorth, val(owner, Player)).

player_move_exit_locked(Config) ->
    start(?WORLD_1),
    Player = get_pid(player),
    RoomNorth =  get_pid(room_nw),
    RoomEast =  get_pid(room_e),
    ExitEastWest =  get_pid(exit_ew),
    ?assertMatch(RoomNorth, val(owner, Player)),
    attempt(Config, Player, {Player, move, e}),
    ?WAIT100,
    ?assertMatch(RoomNorth, val(owner, Player)),
    egre_object:set(ExitEastWest, {is_locked, false}),
    attempt(Config, Player, {Player, move, e}),
    ?WAIT100,
    ?assertMatch(RoomEast, val(owner, Player)).

player_get_item(Config) ->
    start(?WORLD_2),
    Player = get_pid(player),
    Sword = get_pid(sword),
    true = has(Sword, room),
    false = has(Sword, player),
    attempt(Config, Player, {Player, get, <<"sword">>}),
    ?WAIT100,
    true = has(Sword, player).

player_drop_item(Config) ->
    start(?WORLD_2),
    Player = get_pid(player),
    Helmet = get_pid(helmet),
    true = has(Helmet, player),
    attempt(Config, Player, {Player, drop, <<"helmet">>}),
    ?WAIT100,
    [] = all(item, Player),
    true = has(Helmet, room).

character_owner_add_remove(Config) ->
    start(?WORLD_10),
    Player = get_pid(player),
    Rifle = get_pid(rifle),
    Suppressor = get_pid(suppressor),
    Grip = get_pid(grip),
    Clip = get_pid(clip),
    Bullet = get_pid(bullet),
    attempt(Config, Player, {Player, get, <<"rifle">>}),
    ?WAIT100,
    true = has(Rifle, player),
    %WaitFun =
        %fun() ->
            %val(character, Rifle)
        %end,
    %true = wait_loop(WaitFun, Player, 30),
    wait_value(Rifle, character, Player, 30),
    %Player = val(character, Rifle),
    ?assertMatch(Player, val(character, Suppressor)),
    ?assertMatch(Player, val(character, Grip)),
    ?assertMatch(Player, val(character, Clip)),
    ?assertMatch(Player, val(character, Bullet)),
    attempt(Config, Player, {Player, drop, <<"rifle">>}),
    ?WAIT100,
    false = has(Rifle, player),
    [] = val(character, Rifle),
    [] = val(character, Suppressor),
    [] = val(character, Grip),
    [] = val(character, Clip),
    [] = val(character, Bullet).

player_attack(Config) ->
    _SupPid = whereis(egre_object_sup),
    %fprof:trace([start, {file, "my_prof.trace"}, {procs, [SupPid, self()]}]),
    start(?WORLD_3),
    ct:pal("~p:player_attack(Config) world 3 started~n", [?MODULE]),
    Player = get_pid(player),
    ct:pal("~p: Player~n\t~p~n", [?MODULE, Player]),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    receive after 1000 -> ok end,
    %fprof:trace(stop),
    Conditions =
        [{"Zombie is dead",
          fun() -> val(is_alive, z_life) == false end},
         {"Zombie hp < 1",
          fun() -> val(hitpoints, z_hp) =< 0 end}],
    wait_for(Conditions, 4).

wait_for(_NoUnmetConditions = [], _) ->
    ok;
wait_for(Conditions, Count) when Count =< 0 ->
    {Failures, _} = lists:unzip(Conditions),
    ct:fail("Failed waiting for conditions: ~p~n", [Failures]);
wait_for(Conditions, Count) ->
    {Descriptions, _} = lists:unzip(Conditions),
    ct:pal("Checking conditions: ~p~n", [Descriptions]),
    timer:sleep(1000),
    {_, ConditionsUnmet} = lists:partition(fun run_condition/1, Conditions),
    wait_for(ConditionsUnmet, Count - 1).

run_condition({_Desc, Fun}) ->
    Fun().

    %false = val(is_alive, z_life),
    %true = val(hitpoints, z_hp) =< 0.

player_resource_wait(Config) ->
    start(?WORLD_3),
    Player = get_pid(player),
    Fist = get_pid(p_fist_right),
    Stamina = get_pid(p_stamina),
    Zombie = get_pid(zombie),
    egre_object:set(Stamina, {current, 5}),
    egre_object:set(Stamina, {tick_time, 10000}),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    ct:pal("Waiting for player_resource_wait"),
    wait_loop(fun() -> val(hitpoints, z_hp) < 10 end, true, 5),
    true = val(hitpoints, z_hp) < 10,
    true = val(is_alive, z_life),
    true = val(is_attacking, p_fist_right),
    ?assertMatch(Zombie, val(target, Fist)).

one_sided_fight(Config) ->
    start(?WORLD_3),
    Player = get_pid(player),
    _Zombie = get_pid(zombie),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    WaitFun =
        fun() ->
            case val(hitpoints, z_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 60),
    1000 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    false = val(is_alive, z_life).

counterattack_behaviour(Config) ->
    start(?WORLD_3),
    Player = get_pid(player),
    Stamina = val(stamina, zombie),
    egre_object:set(Stamina, {current, 5}),
    egre_object:set(Stamina, {tick_time, 100000}),
    Dexterity = get_pid(dexterity0),
    egre_object:set(Dexterity, {defence_hit_modifier, 0}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"zombie">>}),

    WaitFun =
        fun() ->
            case val(hitpoints, z_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 40),

    case val(is_alive, z_life) of
        true ->
            ct:fail("Zombie should be dead but isn't~n", []);
        _ ->
            ok
    end,
    case val(hitpoints, p_hp) of
        LessThan1000 when LessThan1000 < 1000 ->
            ct:fail("Player hitpoints should be 1000 or more but are ~p~n", [LessThan1000]);
        _ ->
            ok
    end,
    case val(is_alive, p_life) of
        false ->
            ct:fail("Player should be alive but isn't~n", []);
        _ ->
            ok
    end,
    ok.

attack_with_modifiers(Config) ->
    %egre_dbg:add(rules_item_attack),
    start(?WORLD_8),
    Room = get_pid(room1),
    Player = get_pid(player),
    %% TODO make sure that the giant is counterattacking
    _Giant = get_pid(giant),
    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Room, to, Player}),
    attempt(Config, Player, {<<"shield">>, move, from, Room, to, Player}),
    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {<<"shield">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    ?WAIT100,

    %% The giant shouldn't be able to attack the player at all,
    %% so the giant should die and the player should be alive.
    10 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    WaitFun =
        fun() ->
            case val(hitpoints, g_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    ct:pal("Giant hitpoints: ~p~n", [val(hitpoints, g_hp)]),
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 40),
    WaitFun2 =
        fun() ->
            val('is_alive', g_hp)
        end,
    false = wait_loop(WaitFun2, false, 30),
    ok.

stop_attack_on_move(Config) ->
    start(?WORLD_8),
    Room1 = get_pid(room1),
    Room2 = get_pid(room2),
    Player = get_pid(player),

    % TODO make sure the attack stops when the player leaves
    %  - leave
    %  - check that attack is stopped

    _Giant = get_pid(giant),

    %% Keep the attack going, but so that we can prove that it happened
    GiantHPAmt = 10000000,
    GiantHP =  get_pid(g_hp),
    egre_object:set(GiantHP, {hitpoints, GiantHPAmt}),

    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Room1, to, Player}),
    attempt(Config, Player, {<<"shield">>, move, from, Room1, to, Player}),
    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {<<"shield">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    ?WAIT100,

    WaitFun1 =
        fun() ->
            case val(hitpoints, g_hp) of
                LessThanFull when is_integer(LessThanFull),
                                  LessThanFull < GiantHPAmt ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun1, true, 30),

    %% Now that the giant has taken some damage, move the player
    %% and make sure the attack stops.

    attempt(Config, Player, {Player, move, r2}),
    WaitFun2 =
        fun() ->
            case val(room, player) of
                Room2 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun2, true, 30),

    % Make sure the attack has stopped by checking that there
    % are no reservations for the item

    wait_value(p_stamina, reservations, [], 30),
    %WaitFun3 =
        %fun() ->
            %val(reservations, p_stamina)
        %end,
    %true = wait_loop(WaitFun3, [], 30),

    ok.

wait_value(ObjectId, Key, ExpectedValue, Count) ->
    WaitFun =
        fun() ->
            val(Key, ObjectId)
        end,
    true = wait_loop(WaitFun, ExpectedValue, Count).

wait_loop(Fun, ExpectedValue, _Count = 0) ->
    ct:pal("Mismatched function result:~n\tFunction: ~p~n\tResult: ~p",
           [erlang:fun_to_list(Fun), ExpectedValue]),
    false;
wait_loop(Fun, ExpectedValue, Count) ->
    case Fun() == ExpectedValue of
        true ->
            true;
        false ->
            ?WAIT100,
            wait_loop(Fun, ExpectedValue, Count - 1)
    end.

player_wield(Config) ->
    start(?WORLD_4),
    Player = get_pid(player),
    Helmet = get_pid(helmet),
    Head = get_pid(head1),
    ?assertMatch(Helmet, val(item, Player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    ?WAIT100,
    WaitFun =
        fun() ->
            val(item, player)
        end,
    true = wait_loop(WaitFun, [], 30),
    %[] = val(item, Player),
    ?assertMatch({Helmet, _BodyPartRef0}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _BodyPartRef1}, val(body_part, Helmet)).

player_wield_first_available(Config) ->
    start(?WORLD_4),
    Player = get_pid(player),
    Head = get_pid(head1),
    Helmet = get_pid(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    [] = val(item, Player),
    ?assertMatch({Helmet, _BodyPartRef0}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _BodyPartRef1}, val(body_part, Helmet)).

player_wield_missing_body_part(Config) ->
    start(?WORLD_4),
    Player = get_pid(player),
    Head = get_pid(head1),
    Helmet = get_pid(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"finger">>}),
    ?WAIT100,
    [] = val(item, head1),
    ?assertMatch(Helmet, val(item, player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    ?assertMatch({Helmet, _BodyPartRef0}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _BodyPartRef1}, val(body_part, Helmet)),
    [] = val(item, player).

player_wield_wrong_body_part(Config) ->
    start(?WORLD_5),
    Player = get_pid(player),
    Head = get_pid(head1),
    Helmet = get_pid(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"finger">>}),
    ?WAIT100,
    [] = val(item, head1),
    ?assertEqual(Helmet, val(item, player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    ?assertMatch({Helmet, _BodyPartRef1}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _BodyPartRef2}, val(body_part, Helmet)),
    [] = val(item, player).

player_wield_body_part_is_full(Config) ->
    start(?WORLD_6),
    Player = get_pid(player),
    Finger1 = get_pid(finger1),
    Finger2 = get_pid(finger2),
    Ring1 = get_pid(ring1),
    Ring2 = get_pid(ring2),
    AllItems = [_, _] = all(item, player),
    true = lists:member(Ring1, AllItems),
    true = lists:member(Ring2, AllItems),
    [] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring1">>, move, from, Player, to, <<"finger1">>}),
    ?WAIT100,
    ?assertMatch([Ring2], all(item, player)),
    ?assertMatch([{Ring1, _BodyPartRef1}], all(item, finger1)),
    ?assertMatch({body_part, Finger1, finger, _BodyPartRef2}, val(body_part, Ring1)),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring2">>, move, from, Player, to, <<"finger1">>}),
    ?WAIT100,
    ?assertMatch([Ring2], all(item, player)),
    ?assertMatch([{Ring1, _BodyPartRef3}], all(item, finger1)),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring2">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    [] = all(item, player),
    ?assertMatch([{Ring1, _BodyPartRef4}], all(item, finger1)),
    ?assertMatch([{Ring2, _BodyPartRef5}], all(item, finger2)),
    ?assertMatch({body_part, Finger2, finger, _BodyPartRef6}, val(body_part, Ring2)).

player_remove(Config) ->
    start(?WORLD_4),
    Player = get_pid(player),
    Head = get_pid(head1),
    Helmet = get_pid(helmet),
    DexBuff = get_pid(dex_buff),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT1000,
    [] = val(item, player),
    ?assertMatch({Helmet, _Ref}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _Ref0}, val(body_part, Helmet)),
    ?assertMatch({body_part, Head, head, _Ref1}, val(body_part, DexBuff)),
    egre_dbg:add(rules_item_inject_self, attempt),
    attempt(Config, Player, {<<"helmet">>, move, from, <<"head">>, to, Player}),
    ?WAIT1000,
    ?assertMatch(Helmet, val(item, player)),
    [] = val(body_part, Helmet),
    [] = val(body_part, DexBuff),
    [] = val(item, head1),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    [] = val(item, player),
    ?assertMatch({Helmet, _Ref2}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _Ref3}, val(body_part, Helmet)),
    ?assertMatch({body_part, Head, head, _Ref4}, val(body_part, DexBuff)),
    attempt(Config, Player, {<<"helmet">>, move, from, current_body_part, to, Player}),
    ?WAIT100,
    ?assertMatch(Helmet, val(item, player)),
    [] = val(body_part, Helmet),
    [] = val(body_part, DexBuff),
    [] = val(item, head1).

look_player(_Config) ->
    start(?WORLD_7),
    egremud_test_socket:send(<<"AnyLoginWillDo">>),
    egremud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    LoginMessages = egremud_test_socket:messages(),
    ct:pal("~p:~p: LoginMessages~n\t~p~n", [?MODULE, ?FUNCTION_NAME, LoginMessages]),
    egremud_test_socket:send(<<"look Pete">>),
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    NakedDescriptions = egremud_test_socket:messages(),

    ExpectedDescriptions =
        [<<"Pete -> 4.0m tall">>,
         <<"Pete -> 400.0kg">>,
         <<"Pete -> body part hands">>,
         <<"Pete -> body part legs">>,
         <<"Pete -> gender: male">>,
         <<"Pete -> pants_: pants">>,
         <<"Pete -> race: giant">>,
         <<"Pete -> scroll_: scroll">>,
         <<"Pete -> sword_: sword">>,
         <<"character Pete">>],

    case lists:sort(NakedDescriptions) of
        ExpectedDescriptions2 when ExpectedDescriptions == ExpectedDescriptions2 ->
            ok;
        _ ->
            ct:fail("Got descriptions:~p~nbut expected~p~n", [lists:sort(NakedDescriptions), ExpectedDescriptions])
    end.

look_player_clothed(Config) ->
    start(?WORLD_7),
    egremud_test_socket:send(<<"AnyLoginWillDo">>),
    egremud_test_socket:send(<<"AnyPasswordWillDo">>),
    Giant = get_pid(giant),
    attempt(Config, Giant, {<<"pants">>, move, from, Giant, to, <<"legs">>}),
    ?WAIT100,
    egremud_test_socket:send(<<"look pete">>),
    ?WAIT100,
    ClothedDescriptions = egremud_test_socket:messages(),
    ct:pal("ClothedDescriptions: ~p", [ClothedDescriptions]),
    ExpectedDescriptions =
        lists:sort([<<"Pete -> hands">>,
                    <<"Pete -> legs">>,
                    <<"Pete -> legs -> pants_: pants">>,
                    <<"Pete -> sword_: sword">>,
                    <<"Pete -> scroll_: scroll">>,
                    <<"Pete -> giant">>,
                    <<"Pete -> weighs 400.0kg">>,
                    <<"Pete -> 4.0m tall">>,
                    <<"Pete -> male">>]),
    ?assertMatch(ExpectedDescriptions, lists:sort(ClothedDescriptions)).

look_room(_Config) ->
    start(?WORLD_7),
    egremud_test_socket:send(<<"AnyLoginWillDo">>),
    egremud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    _LoginMessages = egremud_test_socket:messages(),
    egremud_test_socket:send(<<"look">>),
    ?WAIT100,
    Descriptions = lists:sort(egremud_test_socket:messages()),
    ct:pal("Descriptions: ~p~n", [Descriptions]),
    Expected = lists:sort([<<"room -> character Bob">>,
                           <<"room -> character Pete">>,
                           <<"room -> bread_: a loaf of bread">>,
                           <<"room: an empty space">>]),
    ?assertMatch(Expected, Descriptions).

look_item(_Config) ->
    start(?WORLD_7),
    egremud_test_socket:send(<<"AnyLoginWillDo">>),
    egremud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    _LoginMessages = egremud_test_socket:messages(),
    egremud_test_socket:send(<<"look bread_">>),
    ?WAIT100,
    Descriptions = lists:sort(egremud_test_socket:messages()),
    ct:pal("Descriptions: ~p~n", [Descriptions]),
    Expected = lists:sort([<<"bread_: a loaf of bread">>]),
    ?assertMatch(Expected, Descriptions).

set_character(Config) ->
    start(?WORLD_9),
    Room = get_pid(room),
    Dog = get_pid(dog),
    Collar = get_pid(collar),
    attempt(Config, Dog, {Collar, move, from, Room, to, Dog}),
    ?WAIT100,
    ?assertMatch(Dog, val(character, collar)),
    ?assertMatch(Dog, val(character, transmitter)),
    ?assertMatch(Dog, val(character, stealth)).

counterattack_with_spell(Config) ->
    start(?WORLD_11),
    Giant = get_pid(giant),
    Player = get_pid(player),

    Stamina = val(stamina, giant),
    egre_object:set(Stamina, {current, 5}),
    egre_object:set(Stamina, {tick_time, 100000}),

    attempt(Config, Player, {Player, memorize, <<"fireball">>}),
    ?WAIT100,
    true = val(is_memorized, fireball_spell),

    attempt(Config, Giant, {Giant, attack, <<"bob">>}),

    WaitFun =
        fun() ->
            case val(hitpoints, g_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 40),

    false = val(is_alive, g_life),
    true = 1000 > val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    ok.

cast_spell(Config) ->
    start(?WORLD_11),
    Player = get_pid(player),
    _Giant = get_pid(giant),
    attempt(Config, Player, {Player, memorize, <<"fireball">>}),
    ?WAIT100,
    true = val(is_memorized, fireball_spell),
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    WaitFun =
        fun() ->
            case val(hitpoints, g_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 30),
    ?WAIT100,
    10 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    false = val(is_alive, g_life).

revive_process(_Config) ->
    start(?WORLD_3),

    PlayerV1 = get_pid(player),
    ct:pal("~p: PlayerV1~n\t~p~n", [?MODULE, PlayerV1]),

    Room = val(owner, player),
    true = is_pid(Room),
    HP = val(hitpoints, player),
    true = is_pid(HP),
    Life = val(life, player),
    true = is_pid(Life),
    Dex = val(attribute, player),
    true = is_pid(Dex),
    Stamina = val(resource, player),
    true = is_pid(Stamina),
    Hand = val(body_part, player),
    true = is_pid(Hand),

    ?assertMatch(PlayerV1, val(owner, p_hp)),
    ?assertMatch(PlayerV1, val(owner, p_life)),
    ?assertMatch(PlayerV1, val(owner, p_hand_right)),
    ?assertMatch(PlayerV1, val(character, p_fist_right)),
    ?assertMatch(PlayerV1, val(owner, dexterity0)),
    ?assertMatch(PlayerV1, val(owner, p_stamina)),

    exit(PlayerV1, kill),
    ?WAIT100,

    PlayerV2 = get_pid(player),
    false = PlayerV1 == PlayerV2,

    ?assertMatch(Room, val(owner, player)),
    ?assert(is_pid(Room)),
    ?assertMatch(HP, val(hitpoints, player)),
    ?assert(is_pid(HP)),
    ?assertMatch(Life, val(life, player)),
    ?assert(is_pid(Life)),
    ?assertMatch(Dex, val(attribute, player)),
    ?assert(is_pid(Dex)),
    ?assertMatch(Stamina, val(resource, player)),
    ?assert(is_pid(Stamina)),
    ?assertMatch(Hand, val(body_part, player)),
    ?assert(is_pid(Hand)),

    ?assertMatch(PlayerV2, val(owner, p_hp)),
    ?assertMatch(PlayerV2, val(owner, p_life)),
    ?assertMatch(PlayerV2, val(owner, p_hand_right)),
    ?assertMatch(PlayerV2, val(character, p_fist_right)),
    ?assertMatch(PlayerV2, val(owner, dexterity0)),
    ?assertMatch(PlayerV2, val(owner, p_stamina)).

decompose(Config) ->
    start(?WORLD_3),
    application:set_env(mud, corpse_cleanup_milis, 1),
    Player = get_pid(player),
    Zombie = get_pid(zombie),
    Room = get_pid(room),
    Sword = get_pid(sword),
    attempt(Config, Player, {Player, cause, 1000, 'of', fire, to, Zombie, with, undefined}),
    ?WAIT1000,
    Conditions =
        [{"Zombie process is dead",
          fun() -> is_process_alive(Zombie) == false end},
         {"Room has Sword",
          fun() -> all_vals(item, room) == [Sword] end},
         {"Sword owner is Room",
          fun() -> val(owner, sword) == Room end}
        ],
    wait_for(Conditions, 3).

search_character(_Config) ->
    %egre_dbg:add(egre_event_log, log),
    %egre_dbg:add(egre_event_log, handle_cast),
    %egre_dbg:add(egre_object, log),
    %egre_dbg:add(egre_event_log, flatten, 1),
    start(?WORLD_12),
    egremud_test_socket:send(<<"AnyLoginWillDo">>),
    egremud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    _LoginMessages = egre_test_socket:messages(),
    egremud_test_socket:send(<<"search Arlene">>),
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    NakedDescriptions = egremud_test_socket:messages(),

    ExpectedDescriptions =
        [<<"Arlene has book name: book desc">>,
         <<"Arlene has coin name: coin desc">>,
         <<"Arlene has shield name: shield desc">>,
         <<"Arlene has sword name: sword desc">>],

    case lists:sort(NakedDescriptions) of
        ExpectedDescriptions2 when ExpectedDescriptions == ExpectedDescriptions2 ->
            ok;
        _ ->
            ct:fail("Got item descriptions:~p~nbut expected~p~n",
                    [lists:sort(NakedDescriptions), ExpectedDescriptions])
    end.

log(_Config) ->
    {ok, Cwd} = file:get_cwd(),
    ct:pal("~p: Cwd~n\t~p~n", [?MODULE, Cwd]),
    LogFile = "../egre_ct.log",
    start(?WORLD_7),
    ct:sleep(500),
    Player = get_pid(player),

    egre_event_log:log(Player, debug, [{foo, bar}]),
    ?WAIT100,
    ct:pal("JSON: ~p~n", [jsx:decode(last_line(LogFile))]),
    [{<<"foo">>, <<"bar">>},
     {<<"level">>, <<"debug">>},
     {<<"process">>, _},
     {<<"process_name">>, <<"player">>}] = jsx:decode(last_line(LogFile)),

    egre_event_log:log(Player, debug, [{foo, bar}, {props, [{player, Player}, {baz, 1}]}]),
    ?WAIT100,
    ct:pal("JSON: ~p~n", [jsx:decode(last_line(LogFile))]),
    [{<<"props">>, [[<<"player">>, _], [<<"baz">>, 1]]},
     {<<"foo">>, <<"bar">>},
     {<<"level">>, <<"debug">>},
     {<<"process">>, _},
     {<<"process_name">>, <<"player">>}] = jsx:decode(last_line(LogFile)),

    egre_event_log:log(Player, debug, [{foo, [1, <<"a">>, 2.0]}]),
    ?WAIT100,
    ct:pal("JSON: ~p~n", [jsx:decode(last_line(LogFile))]),
    [{<<"foo">>, [1, <<"a">>, 2.0]},
     {<<"level">>, <<"debug">>},
     {<<"process">>, _},
     {<<"process_name">>, <<"player">>}] = jsx:decode(last_line(LogFile)).

last_line(Filename) ->
    {ok, Data} = file:read_file(Filename),
    LastLine = hd(lists:reverse(binary:split(Data, <<"\n">>, [trim, global]))),
    ct:pal("~p: LastLine~n\t~p~n", [?MODULE, LastLine]),
    LastLine.

%% TODO use egre:create_graph(Objects)
start(Objects) ->
    IdPids = [{Id, start_obj(Id, Props)} || {Id, Props} <- Objects],
    _Objs = [egre_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    timer:sleep(100).

start_obj(Id, Props) ->
    {ok, Pid} = supervisor:start_child(egre_object_sup, [Id, Props]),
    Pid.

attempt(Config, Target, Message) ->
    TestObject = proplists:get_value(test_object, Config),
    TestObject ! {attempt, Target, Message}.

mock_object() ->
    receive
        X ->
            case X of
                {'$gen_call', _Msg = {From, MonitorRef}, props} ->
                    From ! {MonitorRef, _MockProps = []};
                {attempt, Target, Message} ->
                    egre_object:attempt(Target, Message, false);
                stop ->
                    exit(normal);
                _Other ->
                    ok
            end
    end,
    mock_object().

get_pid(Id) ->
    #object{pid = Pid} = egre_index:get(Id),
    Pid.
