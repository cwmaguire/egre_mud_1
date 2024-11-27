%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_SUITE).
-compile(export_all).
-compile({parse_transform, mud_test_completions}).

-include("mud_test_worlds.hrl").
-include_lib("eunit/include/eunit.hrl").


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
     look_player_clothed,
     look_room,
     look_item,
     set_character,
     cast_spell,
     cast_heal,
     decompose,
     search_character,
     player_say,
     player_emote,
     player_shout,
     get_experience_from_killing,
     achievement,
     historical_achievement_enough,
     historical_achievement_not_enough,
     ask_for_quest,
     complete_quest,
     turn_in_quest,
     no_quests_available,
     no_quests_left,
     self_healing_over_time,
     buy,
     buy_fail_no_item].

init_per_testcase(TestCase, Config) ->
    Port = ct:get_config(port),
    application:load(egre),
    application:set_env(egre, serialize_fun, {mud_util, serialize, 2}),
    {ok, _Started} = application:ensure_all_started([recon, mud]),
    application:set_env(mud, corpse_cleanup_milis, 30000),
    application:set_env(egremud, port, Port),
    application:set_env(egremud, parse_fun, {mud_parse, parse, 2}),
    application:set_env(egre, extract_fun, {mud_util, extract_from_props, 1}),
    application:set_env(egre, tag, TestCase),
    egre:wait_db_ready(),
    {atomic, ok} = mnesia:clear_table(object),
    TestObject = spawn_link(fun mock_object/0),
    % use egre module - fix api
    egre_index:put([{pid, TestObject}, {id, test_object}]),
    [{test_object, TestObject} | Config].

end_per_testcase(_, _Config) ->
    logout(player),
    logout(player2),
    logout(player3),
    logout(player4),
    egre:wait_db_done(500),
    application:stop(recon),
    application:stop(mud),
    application:stop(egremud),
    application:stop(egre),
    wait(1000).

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
            {_RecordName, Props, _ExtractRecordFun, _Tag} = sys:get_state(Pid),
            Tuple = sys:get_state(Pid),
            case erlang:element(2, Tuple) of
                Props when is_list(Props) ->
                    Props;
                _ ->
                    throw(<<"EGRE object state record has changed">>)
            end;
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
    wait(100),
    ?assertMatch(RoomSouth, val(owner, Player)).

player_move_fail(Config) ->
    start(?WORLD_1),
    Player = get_pid(player),
    RoomNorth =  get_pid(room_nw),
    ?assertMatch(RoomNorth, val(owner, Player)),
    attempt(Config, Player, {Player, move, non_existent_exit}),
    wait(100),
    ?assertMatch(RoomNorth, val(owner, Player)).

player_move_exit_locked(Config) ->
    start(?WORLD_1),
    Player = get_pid(player),
    RoomNorth =  get_pid(room_nw),
    RoomEast =  get_pid(room_e),
    ExitEastWest =  get_pid(exit_ew),
    ?assertMatch(RoomNorth, val(owner, Player)),
    attempt(Config, Player, {Player, move, e}),
    wait(100),
    ?assertMatch(RoomNorth, val(owner, Player)),
    egre_object:set(ExitEastWest, {is_locked, false}),
    attempt(Config, Player, {Player, move, e}),
    wait(100),
    ?assertMatch(RoomEast, val(owner, Player)).

player_get_item(Config) ->
    start(?WORLD_2),
    Player = get_pid(player),
    Sword = get_pid(sword),
    true = has(Sword, room),
    false = has(Sword, player),
    attempt(Config, Player, {Player, get, <<"sword">>}),
    wait(100),
    true = has(Sword, player).

player_drop_item(Config) ->
    start(?WORLD_2),
    Player = get_pid(player),
    Helmet = get_pid(helmet),
    true = has(Helmet, player),
    attempt(Config, Player, {Player, drop, <<"helmet">>}),
    wait(100),
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
    wait(100),
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
    wait(100),
    false = has(Rifle, player),
    [] = val(character, Rifle),
    [] = val(character, Suppressor),
    [] = val(character, Grip),
    [] = val(character, Clip),
    [] = val(character, Bullet).

player_attack(_Config) ->
    _SupPid = whereis(egre_object_sup),
    start(?WORLD_3),
    login(player),
    egremud_test_socket:send(player, <<"attack zombie">>),
    Conditions =
        [{"Zombie is dead",
          fun() -> val(is_alive, z_life) == false end},
         {"Zombie hp < 1",
          fun() -> val(hitpoints, z_hp) =< 0 end}],
    wait_for(Conditions, 8).

player_resource_wait(Config) ->
    start(?WORLD_RESOURCE_WAIT),
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

    start_profile(),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    stop_profile(),

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
    wait(1000),
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
    true = wait_loop(WaitFun, true, 100),

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

    %egre_dbg:add(egre_object, pids),
    %egre_dbg:add(egre_object, default_pid_filter),

    %% TODO make sure that the giant is counterattacking
    _Giant = get_pid(giant),
    wait(1000),
    attempt(Config, Player, {<<"force field">>, move, from, Room, to, Player}),
    attempt(Config, Player, {<<"shield">>, move, from, Room, to, Player}),

    wait(1000),
    attempt(Config, Player, {<<"force field">>, move, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {<<"shield">>, move, from, Player, to, first_available_body_part}),
    wait(1000),
  
     %{ok, IO} = file:open("recon", [write]),
    %recon_trace:calls({egre_object, handle_attempt, return_trace}, 100, [{scope, local}, {io_server, IO}]),
    %recon_trace:calls({rules_stop, attempt, return_trace}, 200, [{scope, local}, {io_server, IO}]),
    %recon_trace:calls({rules_attribute_modify, attempt, return_trace}, 200, [{scope, local}, {io_server, IO}]),

    start_profile(),
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    wait(10000),
    stop_profile(),

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
            val('is_alive', g_life)
        end,
    true = wait_loop(WaitFun2, false, 30),
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

    wait(100),
    attempt(Config, Player, {<<"force field">>, move, from, Room1, to, Player}),
    attempt(Config, Player, {<<"shield">>, move, from, Room1, to, Player}),
    wait(100),
    attempt(Config, Player, {<<"force field">>, move, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {<<"shield">>, move, from, Player, to, first_available_body_part}),
    wait(100),
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    wait(100),

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

wait_loop(_Fun, _Count = 0) ->
    ok;
wait_loop(Fun, Count) when Count > 0 ->
    Fun(),
    wait(100),
    wait_loop(Fun, Count - 1).

wait_loop(Fun, ExpectedValue, _Count = 0) ->
    ct:pal("Mismatched function result:~n\tFunction: ~p~n\tExected: ~p",
           [erlang:fun_to_list(Fun), ExpectedValue]),
    false;
wait_loop(Fun, ExpectedValue, Count) ->
    Result = Fun(),
    case Result == ExpectedValue of
        true ->
            true;
        false ->
            ct:pal("wait_loop waiting for ~p, got ~p", [ExpectedValue, Result]),
            wait(100),
            wait_loop(Fun, ExpectedValue, Count - 1)
    end.

player_wield(Config) ->
    start(?WORLD_4),
    Player = get_pid(player),
    Helmet = get_pid(helmet),
    Head = get_pid(head1),
    ?assertMatch(Helmet, val(item, Player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    wait(100),
    wait(100),
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
    wait(100),
    [] = val(item, Player),
    ?assertMatch({Helmet, _BodyPartRef0}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _BodyPartRef1}, val(body_part, Helmet)).

player_wield_missing_body_part(Config) ->
    start(?WORLD_4),
    Player = get_pid(player),
    Head = get_pid(head1),
    Helmet = get_pid(helmet),
    wait(1000),
    ?assertMatch(Helmet, val(item, player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"finger">>}),
    wait(100),
    [] = val(item, head1),
    ?assertMatch(Helmet, val(item, player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    wait(100),
    ?assertMatch({Helmet, _BodyPartRef0}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _BodyPartRef1}, val(body_part, Helmet)),
    [] = val(item, player),
    wait(1000).

player_wield_wrong_body_part(Config) ->
    start(?WORLD_5),
    Player = get_pid(player),
    Head = get_pid(head1),
    Helmet = get_pid(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"finger">>}),
    wait(100),
    [] = val(item, head1),
    ?assertEqual(Helmet, val(item, player)),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    wait(1000),
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
    wait(100),
    ?assertMatch([Ring2], all(item, player)),
    ?assertMatch([{Ring1, _BodyPartRef1}], all(item, finger1)),
    ?assertMatch({body_part, Finger1, finger, _BodyPartRef2}, val(body_part, Ring1)),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring2">>, move, from, Player, to, <<"finger1">>}),
    wait(100),
    ?assertMatch([Ring2], all(item, player)),
    ?assertMatch([{Ring1, _BodyPartRef3}], all(item, finger1)),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring2">>, move, from, Player, to, first_available_body_part}),
    wait(100),
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
    wait(1000),
    [] = val(item, player),
    ?assertMatch({Helmet, _Ref}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _Ref0}, val(body_part, Helmet)),
    ?assertMatch({body_part, Head, head, _Ref1}, val(body_part, DexBuff)),
    %egre_dbg:add(rules_item_inject_self, attempt),
    attempt(Config, Player, {<<"helmet">>, move, from, <<"head">>, to, Player}),
    wait(1000),
    ?assertMatch(Helmet, val(item, player)),
    [] = val(body_part, Helmet),
    [] = val(body_part, DexBuff),
    [] = val(item, head1),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    wait(100),
    [] = val(item, player),
    ?assertMatch({Helmet, _Ref2}, val(item, head1)),
    ?assertMatch({body_part, Head, head, _Ref3}, val(body_part, Helmet)),
    ?assertMatch({body_part, Head, head, _Ref4}, val(body_part, DexBuff)),
    attempt(Config, Player, {<<"helmet">>, move, from, current_body_part, to, Player}),
    wait(100),
    ?assertMatch(Helmet, val(item, player)),
    [] = val(body_part, Helmet),
    [] = val(body_part, DexBuff),
    [] = val(item, head1).

look_player(_Config) ->
    start(?WORLD_7),
    login(player),
    egremud_test_socket:send(player, <<"look Pete">>),
    wait(300),
    NakedDescriptions = egremud_test_socket:messages(player),

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
    login(player),
    Giant = get_pid(giant),
    attempt(Config, Giant, {<<"pants">>, move, from, Giant, to, <<"legs">>}),
    wait(1000),
    egremud_test_socket:send(player, <<"look pete">>),
    wait(1000),
    ClothedDescriptions = egremud_test_socket:messages(player),
    ct:pal("ClothedDescriptions: ~p", [ClothedDescriptions]),
    SortedClothedDescriptions = lists:sort(ClothedDescriptions),
    ct:pal("~p:~p: SortedClothedDescriptions~n\t~p~n", [?MODULE, ?FUNCTION_NAME, SortedClothedDescriptions]),
    ExpectedDescriptions =
        lists:sort([<<"Pete -> body part hands">>,
                    <<"Pete -> body part legs">>,
                    <<"Pete -> legs -> pants_: pants">>,
                    <<"Pete -> sword_: sword">>,
                    <<"Pete -> scroll_: scroll">>,
                    <<"Pete -> race: giant">>,
                    <<"Pete -> 400.0kg">>,
                    <<"Pete -> 4.0m tall">>,
                    <<"Pete -> gender: male">>,
                    <<"character Pete">>]),
    ct:pal("~p:~p: ExpectedDescriptions~n\t~p~n", [?MODULE, ?FUNCTION_NAME, ExpectedDescriptions]),
    ?assertMatch(ExpectedDescriptions, lists:sort(ClothedDescriptions)).

look_giants_legs(Config) ->
    start(?WORLD_7),
    login(player),
    Giant = get_pid(giant),
    attempt(Config, Giant, {<<"pants">>, move, from, Giant, to, <<"legs">>}),
    wait(1000),
    egremud_test_socket:send(player, <<"look legs">>),
    wait(1000),
    ClothedDescriptions = egremud_test_socket:messages(player),
    SortedClothedDescriptions = lists:sort(ClothedDescriptions),
    ExpectedDescriptions =
        lists:sort([<<"Pete -> body part hands">>,
                    <<"Pete -> body part legs">>,
                    <<"Pete -> legs -> pants_: pants">>,
                    <<"Pete -> sword_: sword">>,
                    <<"Pete -> scroll_: scroll">>,
                    <<"Pete -> race: giant">>,
                    <<"Pete -> 400.0kg">>,
                    <<"Pete -> 4.0m tall">>,
                    <<"Pete -> gender: male">>,
                    <<"character Pete">>]),
    ?assertMatch(ExpectedDescriptions, lists:sort(SortedClothedDescriptions)).

look_room(_Config) ->
    start(?WORLD_7),
    login(player),
    drain_socket(player),
    egremud_test_socket:send(player, <<"look">>),
    Expected = lists:sort([<<"room -> character Bob">>,
                           <<"room -> character Pete">>,
                           <<"room -> bread_: a loaf of bread">>,
                           <<"room: an empty space">>]),
    wait_for_sorted_messages(player, Expected, 5).

look_item(_Config) ->
    start(?WORLD_7),
    login(player),
    egremud_test_socket:send(player, <<"look bread_">>),
    wait(100),
    Descriptions = lists:sort(egremud_test_socket:messages(player)),
    Expected = lists:sort([<<"bread_: a loaf of bread">>]),
    ?assertMatch(Expected, Descriptions).

set_character(Config) ->
    start(?WORLD_9),
    Room = get_pid(room),
    Dog = get_pid(dog),
    Collar = get_pid(collar),
    attempt(Config, Dog, {Collar, move, from, Room, to, Dog}),
    wait(100),
    ?assertMatch(Dog, val(character, collar)),
    ?assertMatch(Dog, val(character, transmitter)),
    ?assertMatch(Dog, val(character, stealth)).

counterattack_with_spell(Config) ->
    start(?WORLD_SPELL_ATTACK),
    Giant = get_pid(giant),
    Player = get_pid(player),

    Stamina = val(stamina, giant),
    egre_object:set(Stamina, {current, 5}),
    egre_object:set(Stamina, {tick_time, 100000}),

    attempt(Config, Player, {Player, memorize, <<"fireball">>}),
    wait(100),
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
    start(?WORLD_SPELL_ATTACK),
    Player = get_pid(player),
    _Giant = get_pid(giant),
    attempt(Config, Player, {Player, memorize, <<"fireball">>}),
    wait(100),
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
    wait(100),
    10 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    false = val(is_alive, g_life).

cast_heal(_Config) ->
    start(?WORLD_SPELL_HEAL),
    _Player1 = login(player1),
    _Player2 = login(player2),
    _Player3 = login(player3),
    _Player4 = login(player4),

    Conditions =
        [{"Spell 'heal' is not memorized",
          fun() -> not val(is_memorized, heal_spell ) end}],
    wait_for(Conditions, 10),

    egremud_test_socket:send(player1, <<"cast heal">>),

    MessageFilter1 =
       fun(<<"bob does [", _:1/binary, "] heal spell healing to bob">>) ->
               true;
          (_) ->
               false
       end,
    [] = wait_for_message(player1, MessageFilter1, 5),
    [] = wait_for_message(player2, MessageFilter1, 5),
    [] = wait_for_message(player3, MessageFilter1, 5),
    ?assertEqual([], egremud_test_socket:messages(player4)),

    BobHP = val(hitpoints, p_hp),
    ct:pal("~p:~p: HP: ~p", [?MODULE, ?FUNCTION_NAME, BobHP]),

    Conditions2 =
        [{"Player 1 should have health higher than before (>1)",
          fun() -> val(hitpoints, p_hp) > 1 end}],
    wait_for(Conditions2, 10),

    egre_dbg:add(rules_resource_reserve, update_tick),

    egremud_test_socket:send(player1, <<"cast heal Pete">>),

    MessageFilter2 =
       fun(<<"bob does [", _:1/binary, "] heal spell healing to Pete">>) ->
               true;
          (_) ->
               false
       end,
    [] = wait_for_message(player1, MessageFilter2, 5),
    [] = wait_for_message(player2, MessageFilter2, 5),
    [] = wait_for_message(player3, MessageFilter2, 5),
    ?assertEqual([], egremud_test_socket:messages(player4)),

    Conditions3 =
        [{"Player 2 ('Pete') should have health higher than before (>1)",
          fun() -> val(hitpoints, p2_hp) > 1 end}],
    wait_for(Conditions3, 10),

           %{hit_roll, {1, 10}},
           %{effect_roll, -9},
    HealEffectPrototype = get_pid(p_heal_effect_prototype),
    egre_object:set(HealEffectPrototype, {hit_roll, {1, -1}}),
    PeteHP = val(hitpoints, p2_hp),
    ct:pal("~p:~p: PeteHP~n\t~p~n", [?MODULE, ?FUNCTION_NAME, PeteHP]),
    egremud_test_socket:send(player1, <<"cast heal Pete">>),

    wait_for_sorted_messages(player1, [<<"bob misses Pete with heal spell">>], 5),
    wait_for_sorted_messages(player2, [<<"bob misses Pete with heal spell">>], 5),
    wait_for_sorted_messages(player3, [<<"bob misses Pete with heal spell">>], 5),
    ?assertEqual([], egremud_test_socket:messages(player4)),

    Conditions4 =
        [{"Player 2 ('Pete') should have the same health as before",
          fun() -> PeteHP == val(hitpoints, p2_hp) end}],
    wait_for(Conditions4, 10),

    egre_object:set(HealEffectPrototype, {hit_roll, {1, 10}}),
    egre_object:set(HealEffectPrototype, {effect_roll, {1, -1}}),
    egremud_test_socket:send(player1, <<"cast heal Pete">>),

    wait_for_sorted_messages(player1, [<<"bob has no effect on Pete with heal spell">>], 5),
    wait_for_sorted_messages(player2, [<<"bob has no effect on Pete with heal spell">>], 5),
    wait_for_sorted_messages(player3, [<<"bob has no effect on Pete with heal spell">>], 5),
    ?assertEqual([], egremud_test_socket:messages(player4)),

    ct:pal("~p:~p: PeteHP~n\t~p~n", [?MODULE, ?FUNCTION_NAME, PeteHP]),

    Conditions5 =
        [{"Player 2 ('Pete') should have the same health as before",
          fun() -> PeteHP == val(hitpoints, p2_hp) end}],
    wait_for(Conditions5, 10).

revive_process(_Config) ->
    start(?WORLD_3),
    PlayerV1 = get_pid(player),
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
    wait(100),

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
    attempt(Config, Player, {Player, cause, 1000, 'of', fire,
                             to, Zombie,
                             with, undefined}),
    wait(1000),
    Conditions =
        [{"Zombie process is dead",
          fun() -> is_process_alive(Zombie) == false end},
         {"Room has Sword",
          fun() -> all_vals(item, room) == [Sword] end},
         {"Sword owner is Room",
          fun() -> val(owner, sword) == Room end}
        ],
    wait_for(Conditions, 10).

search_character(_Config) ->
    start(?WORLD_12),
    login(player),
    egremud_test_socket:send(player, <<"search Arlene">>),
    wait(300),
    NakedDescriptions = egremud_test_socket:messages(player),

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

player_say(_Config) ->
    start(?WORLD_SAY),
    login(player2),
    login(player),
    login(player3),
    Fun = fun() ->
                  egremud_test_socket:messages(player),
                  egremud_test_socket:messages(player2),
                  egremud_test_socket:messages(player3)
          end,
    wait_loop(Fun, 5),
    egremud_test_socket:send(player, <<"say foo bar">>),
    wait(200),
    PlayerReceived = egremud_test_socket:messages(player),
    Player2Received = egremud_test_socket:messages(player2),
    Player3Received = egremud_test_socket:messages(player3),
    Expected = [<<"Reginald says: foo bar">>],
    ?assertMatch(Expected, PlayerReceived),
    ?assertMatch(Expected, Player2Received),
    ?assertMatch([], Player3Received).

player_emote(_Config) ->
    start(?WORLD_SAY),
    login(player2),
    login(player),
    login(player3),
    Fun = fun() ->
                  egremud_test_socket:messages(player),
                  egremud_test_socket:messages(player2),
                  egremud_test_socket:messages(player3)
          end,
    wait_loop(Fun, 5),
    egremud_test_socket:send(player, <<"emote coughs">>),
    wait(200),
    PlayerReceived = egremud_test_socket:messages(player),
    Player2Received = egremud_test_socket:messages(player2),
    Player3Received = egremud_test_socket:messages(player3),
    Expected = [<<"Reginald coughs">>],
    ?assertMatch(Expected, PlayerReceived),
    ?assertMatch(Expected, Player2Received),
    ?assertMatch([], Player3Received).

player_shout(_Config) ->
    start(?WORLD_SHOUT),
    login(player),
    login(player2),
    login(player3),
    login(player4),
    Fun = fun() ->
                  egremud_test_socket:messages(player),
                  egremud_test_socket:messages(player2),
                  egremud_test_socket:messages(player3),
                  egremud_test_socket:messages(player4)
          end,
    wait_loop(Fun, 5),
    egremud_test_socket:send(player, <<"shout foo bar">>),
    wait(200),
    PlayerReceived = egremud_test_socket:messages(player),
    Player2Received = egremud_test_socket:messages(player2),
    Player3Received = egremud_test_socket:messages(player3),
    Player4Received = egremud_test_socket:messages(player4),
    ExpectedInLivingRoom = [<<"Reginald shouts: foo bar">>],
    ?assertMatch(ExpectedInLivingRoom, PlayerReceived),
    ?assertMatch(ExpectedInLivingRoom, Player2Received),
    ExpectedInKitchen = [<<"Reginald shouts: foo bar from living_room">>],
    ?assertMatch(ExpectedInKitchen, Player3Received),
    ?assertMatch([], Player4Received).

get_experience_from_killing(Config) ->
    start(?WORLD_EXP_FROM_KILL),
    login(player),
    Fun = fun() ->
                  egremud_test_socket:messages(player)
          end,
    wait_loop(Fun, 5),
    Player = get_pid(player),

    attempt(Config, Player, {Player, attack, <<"rat">>}),
    wait(3000),
    Conditions1 =
        [{"Rat is dead",
          fun() -> val(is_alive, r_life) == false end},
         {"Player has 1 experience",
          fun() -> val(gained, p_exp) == 1 end}],
    wait_for(Conditions1, 5),

    attempt(Config, Player, {Player, attack, <<"big rat">>}),
    wait(3000),

    Conditions2 =
        [{"Big Rat is dead",
          fun() -> val(is_alive, br_life) == false end},
         {"Player has 4 experience",
          fun() -> val(gained, p_exp) == 4 end}],
    wait_for(Conditions2, 5).

level_up(Config) ->
    start(?WORLD_LEVEL_UP),
    login(player),
    Fun = fun() ->
                  egremud_test_socket:messages(player)
          end,
    wait_loop(Fun, 5),
    Player = get_pid(player),

    attempt(Config, Player, {Player, gains, 10, experience}),
    wait(300),
    Props = get_props(p_level),
    ct:pal("~p:~p: Props~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Props]),
    Level = val(level, p_level),
    ct:pal("~p:~p: Level~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Level]),

    Conditions1 =
        [{"Player is level 1",
          fun() -> val(level, p_level) == 1 end}],
    wait_for(Conditions1, 5),

    wait_for_sorted_messages(player, [<<"You have reached level 1">>], 5),

    attempt(Config, Player, {Player, gains, 10, experience}),
    wait(300),
    ?assertEqual(1, val(level, p_level), "Not enough exp for level 2, should be level 1 still."),

    attempt(Config, Player, {Player, gains, 40, experience}),
    wait(300),

    ExpectedMessages = [<<"You have reached level 2">>,
                        <<"You have reached level 3">>],
    wait_for_sorted_messages(player, ExpectedMessages, 5),
    ?assertEqual(3, val(level, p_level), "Enough exp for 2 levels, should be up to level 3.").

achievement(Config) ->
    start(?WORLD_ACHIEVEMENT),
    login(player),
    Fun = fun() ->
                  egremud_test_socket:messages(player)
          end,
    wait_loop(Fun, 5),

    %recon_trace:calls({mud_SUITE, get_pid, return_trace}, 40, [{scope, local}]),
    Player = get_pid(player),

    attempt(Config, Player, {Player, chopped, tree, 1}),
    wait(300),

    ?assertNot(val(done, p_achievement_got_wood_1), "Only 1 tree chopped, no achievement"),
    attempt(Config, Player, {Player, chopped, tree, 2}),
    attempt(Config, Player, {Player, chopped, tree, 3}),

    Conditions =
        [{"Achievement 'Got Wood?' is done",
          fun() -> val(done, p_achievement_got_wood_1) end}],
    wait_for(Conditions, 5),

    ExpectedMessages = [<<"You achieved 'Got Wood?'!">>],
    wait_for_sorted_messages(player, ExpectedMessages, 5).

historical_achievement_enough(Config) ->
    start(?WORLD_HISTORICAL_ACHIEVEMENT_ENOUGH),
    Player = login(player),
    drain_socket(player),

    attempt(Config, Player, {Player, metrics, add, trees_chopped, 10}),
    wait(300),

    %egre_dbg:add(rules_char_metrics, succeed),

    Conditions =
        [{"Metrics set to >= 10 chopped trees",
          fun() ->
              #{trees_chopped := Count} = val(metrics, p_metrics),
              Count >= 10
          end}],
    wait_for(Conditions, 5),

    AchievementProps =
        [{owner, Player},
         {count, 0},
         {target, 10},
         {done, false},
         {allow_previous, true},
         ?ACHIEVEMENT_GOT_WOOD_1_RULES],

    egre_dbg:add(rules_achievement_got_wood_1, succeed),
    {ok, Pid} = supervisor:start_child(egre_object_sup, [p_achievement, AchievementProps]),
    ct:pal("~p:~p: achievement Pid~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Pid]),
    Props = get_props(Pid),
    ct:pal("~p:~p: Props~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Props]),

    Conditions2 =
        [{"Achievement 'Got Wood?' is done",
          fun() -> val(done, p_achievement) end}],
    wait_for(Conditions2, 5),

    ExpectedMessages = [<<"You achieved 'Got Wood?'!">>],
    wait_for_sorted_messages(player, ExpectedMessages, 5).

historical_achievement_not_enough(Config) ->
    start(?WORLD_HISTORICAL_ACHIEVEMENT_ENOUGH),
    login(player),
    drain_socket(player),
    Player = get_pid(player),

    attempt(Config, Player, {Player, metrics, add, trees_chopped, 10}),
    Conditions =
        [{"Metrics set to 10 chopped trees",
          fun() ->
              #{trees_chopped := Count} = val(metrics, p_metrics),
              Count >= 10
          end}],
    wait_for(Conditions, 5),

    AchievementProps =
        [{owner, Player},
         {count, 0},
         {target, 1},
         {done, false},
         {allow_previous, false},
         ?ACHIEVEMENT_GOT_WOOD_1_RULES],

    {ok, Achievement}
        = supervisor:start_child(egre_object_sup,
                                 [p_achievement, AchievementProps]),
    wait(400),
    ?assertNot(val(done, p_achievement),
               "Achievement 'Got Wood?' should not be done"),

    egre:attempt(Achievement, {Player, chopped, tree, <<"Tree">>}),

    ExpectedMessages = [<<"You achieved 'Got Wood?'!">>],
    wait_for_sorted_messages(player, ExpectedMessages, 5).

no_quests_available(_Config) ->
    start(?WORLD_NO_AVAILABLE_QUESTS),
    _Player = login(player),

    egremud_test_socket:send(player, <<"say quests">>),
    ExpectedMessages =
        lists:sort([<<"Dorkle says: quests">>,
                    <<"Smorf says: I don't have any quests for you, Dorkle">>]),
    wait_for_sorted_messages(player, ExpectedMessages, 5).

no_quests_left(_Config) ->
    start(?WORLD_NO_QUESTS_LEFT),
    _Player = login(player),

    egremud_test_socket:send(player, <<"say quests">>),
    ExpectedMessages =
        lists:sort([<<"Smorkennical says: quests">>,
                    <<"Gaa'ark says: you already have all the quests, Smorkennical">>]),
    wait_for_sorted_messages(player, ExpectedMessages, 5).

ask_for_quest(_Config) ->
    start(?WORLD_GET_QUEST),
    Player = login(player),

    egremud_test_socket:send(player, <<"say quests">>),
    ExpectedQuestListMessages1 = lists:sort([<<"Peter says: quests">>,
                                            <<"Brenda says: Peter, I have these quests: five rats, temporary">>]),
    wait_for_sorted_messages(player, ExpectedQuestListMessages1, 5),

    egremud_test_socket:send(player, <<"say quest five rats">>),
    wait(400),
    ?assertMatch(Pid when is_pid(Pid), val(quest, player)),
    Quest = val(quest, player),
    ?assertEqual(Player, val(owner, Quest)),

    ExpectedQuestGetMessages = lists:sort([<<"Peter says: quest five rats">>,
                                   <<"Brenda says: Here is your quest, Peter: five rats">>,
                                   <<"You've received a quest!">>]),
    wait_for_sorted_messages(player, ExpectedQuestGetMessages, 5),

    egremud_test_socket:send(player, <<"say quests">>),
    ExpectedQuestListMessages2 = lists:sort([<<"Peter says: quests">>,
                                            <<"Brenda says: Peter, I have these quests: temporary">>]),
    wait_for_sorted_messages(player, ExpectedQuestListMessages2, 5),

    egremud_test_socket:send(player, <<"say quest five rats">>),
    ExpectedQuestListMessages3 = lists:sort([<<"Peter says: quest five rats">>,
                                             <<"Brenda says: You're already working on five rats, Peter">>]),
    wait_for_sorted_messages(player, ExpectedQuestListMessages3, 5),

    egremud_test_socket:send(player, <<"say quest ready to turn in">>),
    ExpectedQuestListMessages4 = lists:sort([<<"Peter says: quest ready to turn in">>,
                                             <<"Brenda says: You're already working on ready to turn in, Peter">>]),
    wait_for_sorted_messages(player, ExpectedQuestListMessages4, 5).

    %% TODO ask for quests when there is none
    %% TODO ask for turned-in quest and don't get it, unless it's set to allow repeats

turn_in_quest(_Config) ->
    start(?TURN_IN_QUEST),
    _Player = login(player),
    ?assertEqual(0, val(amount, p_charisma)),

    egremud_test_socket:send(player, <<"say quest turn in">>),

    Conditions =
        [{"Charisma updated from 0 to 1",
          fun() -> 1 == val(amount, p_charisma) end}],
    wait_for(Conditions, 5),

    %% TODO add rewards messages: e.g. "Your charisma has increased by 1 to N"
    ExpectedMessages = [<<"Peter says: quest turn in">>,
                        <<"You have completed the the quest \"turn in\"!">>],
    wait_for_sorted_messages(player, ExpectedMessages, 5).

complete_quest(Config) ->
    start(?WORLD_COMPLETE_QUEST),
    Player = login(player),
    drain_socket(player),
    Room = get_pid(room),
    LeftHand = get_pid(p_hand_left),
    Glove = get_pid(p_glove),

    egre_dbg:add(rules_hitpoints_attack),

    attempt(Config, Player, {Player, attack, <<"rat 1">>}),
    Conditions = [{"Rat 1 is dead", fun() -> val(is_alive, r_life) == false end}],
    wait_for(Conditions, 5),

    attempt(Config, Player, {<<"Left-hand of Vecna">>, move, from, Room, to, Player}),
    Conditions5 =
        [{"Glove owner is player",
          fun() -> val(owner, p_glove) == Player end},
         {"Player has glove",
          fun() ->
                  lists:member(Glove, all_vals(item, Player))
          end}],
    wait_for(Conditions5, 5),

    attempt(Config, Player, {<<"Left-hand of Vecna">>, move, from, Player, to, LeftHand}),
    Conditions4 =
        [{"Glove body_part is left hand",
          fun() ->
                  BodyPart = val(body_part, p_glove),
                  case val(body_part, p_glove) of
                      {body_part, LeftHand, hand, _Ref} ->
                          true;
                      _ ->
                          ct:pal("~p p_glove body_part: ~p", [self(), BodyPart]),
                          false
                  end
          end},
         {"Glove owner is hand",
          fun() -> val(owner, p_glove) == LeftHand end},
         {"Left Hand has item glove",
          fun() ->
                  ValsAsMixedProplist = all_vals(item, LeftHand),
                  case proplists:get_value(Glove, ValsAsMixedProplist) of
                      Ref when is_reference(Ref) ->
                          true;
                      _NoGloveRefTuple ->
                          ct:pal("~p ~p:~p: LeftHand (~p) 'item' vals: ~p",
                                 [self(), ?MODULE, ?FUNCTION_NAME, LeftHand, ValsAsMixedProplist]),
                          false
                  end
          end}],
    wait_for(Conditions4, 5),

    attempt(Config, Player, {Player, attack, <<"rat 2">>}),
    Conditions6 = [{"Rat 2 is dead", fun() -> val(is_alive, r2_life) == false end}],
    wait_for(Conditions6, 10),

    wait(400),
    ?assertNot(val(is_complete, p_quest),
               "We've only killed one rat wearing the glove"),

    attempt(Config, Player, {Player, attack, <<"rat 3">>}),
    Conditions2 = [{"Rat 3 is dead", fun() -> val(is_alive, r3_life) == false end}],
    wait_for(Conditions2, 10),

    Conditions3 = [{"Quest complete", fun() -> val(is_complete, p_quest) end}],
    wait_for(Conditions3, 10).

complete_get_quest(Config) ->
    start(?WORLD_COMPLETE_GET_QUEST),
    Player = login(player),
    Room = get_pid(room),
    Glove = get_pid(r_glove),

    attempt(Config, Player, {<<"Left-hand of Vecna">>, move, from, Room, to, Player}),
    Conditions5 =
        [{"Glove owner is player",
          fun() -> val(owner, r_glove) == Player end},
         {"Player has glove",
          fun() ->
                  lists:member(Glove, all_vals(item, Player))
          end}],
    wait_for(Conditions5, 5),

    Conditions3 = [{"Quest item type room name complete",
                    fun() -> val(is_complete, p_quest_1) end},
                   {"Quest item name room name complete",
                    fun() -> val(is_complete, p_quest_2) end}],
    wait_for(Conditions3, 10),

    wait(400),
    ?assertNot(val(is_complete, p_quest_3),
               "Glove was in wrong room"),
    ?assertNot(val(is_complete, p_quest_4),
               "Glove was in wrong room").

self_healing_over_time(Config) ->
    start(?WORLD_RECOVER_HEALTH),
    Player = login(player1),

    %egre_dbg:add(rules_resource_tick, allocate),
    %egre_dbg:add(rules_resource_reserve),

    attempt(Config,
            Player,
            {test, cause, 1000, 'of', blunt_force, to, Player, with, bar}),

    Conditions1 =
        [{"Player has taken 1000 damage, going from 5000 to 4000 hp",
          fun() -> val(hitpoints, p_hp) < 5000 end}],
    wait_for(Conditions1, 5),

    PlayerHP1 = val(hitpoints, p_hp),

    Conditions2 =
        [{["Player has healed above ", PlayerHP1],
          fun() -> Val = val(hitpoints, p_hp),
                   ct:pal("~p~n", [Val]),
                   Val > PlayerHP1
          end}],
    wait_for(Conditions2, 5),

    PlayerHP2 = val(hitpoints, p_hp),

    Conditions3 =
        [{["Player has healed above ", PlayerHP2],
          fun() -> val(hitpoints, p_hp) > PlayerHP2 end}],
    wait_for(Conditions3, 5).

buy(_Config) ->
    start(?WORLD_BUY),
    Player = login(player),
    Glove = get_pid(r_glove),

    egremud_test_socket:send(player, <<"buy Left_hand_of_Vecna">>),

    Conditions1 =
        [{"Player has glove",
          fun() -> val(item, player) == Glove end},
         {"Player has 1 money left",
          fun() -> val(money, player) == 1 end},
         {"Seller has no item",
          fun() -> val(item, shopkeeper) == [] end},
         {"Seller 10 more money",
          fun() -> val(money, shopkeeper) == 12 end},
         {"Item is owned by player",
          fun() -> val(owner, r_glove) == Player end}],
    wait_for(Conditions1, 10).

buy_fail_no_item(_Config) ->
    start(?WORLD_BUY),
    login(player),

    egremud_test_socket:send(player, <<"buy item_that_doesnt_exist">>),

    egre_dbg:add(egremud_test_socket, messages),
    egre_dbg:add(mud_SUITE, wait_for_sorted_messages),

    ExpectedMsg = [<<"Item item_that_doesnt_exist doesn't exist">>],
    wait_for_sorted_messages(player, ExpectedMsg, 5).

log(_Config) ->
    {ok, Cwd} = file:get_cwd(),
    ct:pal("~p: Cwd ~p", [?MODULE, Cwd]),
    LogFile = "../egre_ct.log",
    start(?WORLD_7),
    ct:sleep(500),
    Player = get_pid(player),

    egre_event_log:log(Player, debug, [{foo, bar}]),
    wait(100),
    ct:pal("JSON: ~p~n", [jsx:decode(last_line(LogFile))]),
    [{<<"foo">>, <<"bar">>},
     {<<"level">>, <<"debug">>},
     {<<"process">>, _},
     {<<"process_name">>, <<"player">>}] = jsx:decode(last_line(LogFile)),

    egre_event_log:log(Player, debug, [{foo, bar}, {props, [{player, Player}, {baz, 1}]}]),
    wait(100),
    ct:pal("JSON: ~p~n", [jsx:decode(last_line(LogFile))]),
    [{<<"props">>, [[<<"player">>, _], [<<"baz">>, 1]]},
     {<<"foo">>, <<"bar">>},
     {<<"level">>, <<"debug">>},
     {<<"process">>, _},
     {<<"process_name">>, <<"player">>}] = jsx:decode(last_line(LogFile)),

    egre_event_log:log(Player, debug, [{foo, [1, <<"a">>, 2.0]}]),
    wait(100),
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

start(Objects) ->
    egre:create_graph(Objects),
    timer:sleep(300).

start_obj(Id, Props) ->
    {ok, Pid} = supervisor:start_child(egre_object_sup, [Id, Props]),
    Pid.

attempt(Config, Target, Message) ->
    TestObject = proplists:get_value(test_object, Config),
    TestObject ! {attempt, Target, Message}.

login(Player) ->
    egremud_test_socket:start(Player),
    egremud_test_socket:send(Player, atom_to_binary(Player)),
    egremud_test_socket:send(Player, <<"AnyPasswordWillDo">>),
    drain_socket(Player),
    get_pid(Player).

logout(Player) ->
    egremud_test_socket:stop(Player).

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

wait(Millis) ->
    %ct:pal("~p: Waiting ~p~n", [self(), Millis]),
    receive X ->
        ct:pal("Received ~p~n", [X])
    after Millis ->
         ok
    end.
    %ct:pal("~p: Waited ~p~n", [self(), Millis]).

drain_socket(Player) ->
    Fun = fun() ->
              egremud_test_socket:messages(Player)
          end,
    wait_loop(Fun, 5).

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

wait_for_sorted_messages(Character, Expected, Count) ->
    wait_for_sorted_messages(Character, Expected, [], Count).

wait_for_sorted_messages(Character, Expected, Received, Count) when Count =< 0 ->
    ct:fail("Failed waiting for ~p socket messages: ~p~nReceived: ~p",
            [Character, Expected, Received]);
wait_for_sorted_messages(Character, Expected, Received, Count) ->
    New = egremud_test_socket:messages(Character),
    Sorted = lists:sort(New ++ Received),
    ct:pal("~p:~p: Sorted~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Sorted]),
    case Sorted of
        Expected ->
            ok;
        _ ->
            wait(1000),
            wait_for_sorted_messages(Character, Expected, Sorted, Count - 1)
    end.

wait_for_message(Character, FilterFun, Count) ->
    wait_for_message(Character, FilterFun, Count, []).

wait_for_message(_Character, _FilterFun, _Count = 0, _OtherMessages) ->
    ct:fail("Failed waiting for specific message");
wait_for_message(Character, FilterFun, Count, OtherMessages) ->
    NewMessages = egremud_test_socket:messages(Character),
    ct:pal("~p:~p: Waiting for message, got ~p", [?MODULE, ?FUNCTION_NAME, NewMessages]),
    case lists:partition(FilterFun, NewMessages) of
        {[], Unmatched} ->
           wait(1000),
           wait_for_message(Character, FilterFun, Count - 1, Unmatched ++ OtherMessages);
        {[_Match | Rest], Unmatched} ->
           Rest ++ Unmatched ++ OtherMessages
    end.

start_profile() ->
    cprof:start().

stop_profile() ->
    {ok, IO} = file:open("cprof", [write]),
    cprof:pause(),
    Result = cprof:analyse(),
    io:format(IO, "~p", [Result]),
    file:close(IO),
    cprof:stop().

recon_trace(Mod, Fun) ->
    {ok, IO} = file:open("recon", [write]),
    recon_trace:calls({Mod, Fun, return_trace}, 200, [{scope, local}, {io_server, IO}]).
