-include("mud_rules.hrl").

-define(TEST_CONN_RULES, {rules, [rules_handler_test_connection_attack,
                                  ?UNIVERSAL_RULES]}).

-define(WORLD_1, [{room_nw,
                   [{exit, exit_ns},
                    {exit, exit_ew},
                    {visitor, player},
                    {icon, room},
                    ?ROOM_RULES]},

                  {room_s,
                   [{exit, exit_ns},
                    {icon, room},
                    ?ROOM_RULES]},

                  {room_e,
                   [{exit, exit_ew},
                    {icon, room},
                    ?ROOM_RULES]},

                  {player,
                   [{owner, room_nw},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {exit_ns,
                   [{{room, n}, room_nw},
                    {{room, s}, room_s},
                    {icon, exit},
                    ?EXIT_RULES]},

                  {exit_ew,
                   [{{room, w}, room_nw},
                    {{room, e}, room_e},
                    {is_locked, true},
                    {icon, exit},
                    ?EXIT_RULES]}]).

-define(WORLD_2, [{room,
                   [{visitor, player},
                    {item, sword},
                    {item, apple},
                    {icon, room},
                    ?ROOM_RULES]},

                  {player,
                   [{owner, room},
                    {item, helmet},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {sword,
                   [{owner, room},
                    {name, <<"sword">>},
                    {icon, weapon},
                    ?ITEM_RULES]},

                  {helmet,
                   [{owner, player},
                    {name, <<"helmet">>},
                    {icon, armor},
                    ?ITEM_RULES]},

                  {apple,
                   [{owner, room},
                    {name, <<"apple">>},
                    {icon, food},
                    ?ITEM_RULES]}]).

-define(WORLD_3, [{room,
                   [{visitor, player},
                    {visitor, zombie},
                    {icon, room},
                    ?ROOM_RULES]},

                  {player,
                   [{owner, room},
                    {hitpoints, p_hp},
                    {life, p_life},
                    {attribute, dexterity0},
                    {attack_types, [melee]},
                    %% TODO: why is stamina a first class property
                    %% instead of just an attribute?
                    %% It might not matter what the index of the property
                    %% is if we don't look them up by index
                    {resource, p_stamina},
                    {body_part, p_hand_right},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {p_hp,
                   [{hitpoints, 1000},
                    {owner, player},
                    {icon, stat},
                    ?HITPOINTS_RULES]},

                  {p_life,
                   [{is_alive, true},
                    {owner, player},
                    {icon, stat},
                    ?LIFE_RULES]},

                  {p_hand_right,
                   [{name, <<"right hand">>},
                    {item, p_fist_right},
                    {owner, player},
                    {max_items, 1},
                    {body_part, hand},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {p_fist_right,
                   [{name, <<"right fist">>},
                    {owner, p_hand_right},
                    {character, player},
                    {wielding_body_parts, [hand]},
                    {body_part, {?PID(p_hand_right), hand}},
                    {is_attack, true},
                    {is_defence, false},
                    {should_attack_module, mud_attack_melee},
                    {should_defend_module, mud_defence_melee},
                    {effect_prototype, p_fist_melee_effect_prototype},
                    {attack_type, melee},
                    {resources, [{stamina, 5}]},
                    {icon, weapon},
                    ?WEAPON_RULES]},

                  {p_fist_melee_effect_prototype,
                   [{owner, p_fist_right},
                    {character, player},
                    {type, blunt_force},
                    {lifecycle, once},
                    {hit_roll, {1, 10}},
                    {effect_roll, 10},
                    {child_rules, ?EFFECT_RULES}, %% e.g. {child_rules, {rules, [...]}}
                    ?EFFECT_PROTOTYPE_RULES]},

                  {dexterity0,
                   [{attack_hit_modifier, 1},
                    {defence_hit_modifier, 99},
                    {owner, player},
                    {character, player},
                    {icon, stat}, ?ATTRIBUTE_RULES]},

                  {p_stamina,
                   [{owner, player},
                    {type, stamina},
                    {per_tick, 1},
                    {tick_time, 100},
                    {max, 10},
                    {icon, resource},
                    ?RESOURCE_RULES]},

                  {zombie,
                   [{owner, room},
                    {attack_wait, 10},
                    {name, <<"zombie">>},
                    {hitpoints, z_hp},
                    {life, z_life},
                    {attribute, dexterity1},
                    {body_part, z_hand},
                    %% TODO Do something with this
                    %% "melee" can even be an attack command that's
                    %% more specific than just attack:
                    %% "spell zombie"
                    %% "melee zombie"
                    %% "shoot zombie"
                    {attack_types, [melee]},
                    {stamina, z_stamina},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {z_hand,
                   [{name, <<"left hand">>},
                    {owner, zombie},
                    {body_part, hand},
                    {max_items, 1},
                    {item, sword},
                    {icon, person},
                    ?BODY_PART_RULES]},

                  {z_hp,
                   [{hitpoints, 10},
                    {owner, zombie},
                    {icon, stat},
                    ?HITPOINTS_RULES]},

                  {z_life,
                   [{is_alive, true},
                    {owner, zombie},
                    {icon, stat},
                    ?LIFE_RULES]},

                  {dexterity1,
                   [{attack_hit_modifier, 1},
                    {owner, zombie},
                    {character, zombie},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {z_stamina,
                   [{owner, zombie},
                    {type, stamina},
                    {per_tick, 1},
                    {tick_time, 500},
                    {max, 10},
                    {current, 0},
                    {icon, resource},
                    ?RESOURCE_RULES]},

                  {sword,
                   [{attack_damage_modifier, 5},
                    {owner, z_hand},
                    % I don't think an item is supposed to know what its character is
                    %{character, zombie},
                    {is_attack, true},
                    {is_auto_attack, true},
                    {resources, [{stamina, 5}]},
                    {wielding_body_parts, [hand]},
                    {body_part, {?PID(z_hand), hand}},
                    {icon, weapon},
                    {drop_on_death, true},
                    ?ITEM_RULES]}]).

-define(WORLD_4, [{room,
                   [{visitor, player},
                    ?ROOM_RULES]},

                  {player,
                   [{owner, room},
                    {item, helmet},
                    {body_part, head1},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {head1,
                   [{name, <<"head">>},
                    {body_part, head},
                    {owner, player},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {helmet,
                   [{name, <<"helmet">>},
                    {owner, player},
                    {character, player},
                    {attribute, dex_buff},
                    {body_parts, [head]},
                    {icon, armor},
                    ?ITEM_RULES]},

                  {dex_buff,
                   [{name, <<"dex_buff">>},
                    {owner, helmet},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]}]).

-define(WORLD_5, [{player,
                   [{item, helmet},
                    {body_part, head1},
                    {body_part, finger1},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {head1,
                   [{name, <<"head">>},
                    {owner, player},
                    {body_part, head},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {finger1,
                   [{name, <<"finger">>},
                    {owner, player},
                    {body_part, finger},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {helmet,
                   [{owner, player},
                    {name, <<"helmet">>},
                    {body_parts, [head, hand]},
                    {icon, armor},
                    ?ITEM_RULES]}]).

-define(WORLD_6, [{player,
                   [{body_part, finger1},
                    {body_part, finger2},
                    {item, ring1},
                    {item, ring2},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {finger1,
                   [{name, <<"finger1">>},
                    {owner, player},
                    {max_items, 1},
                    {body_part, finger},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {finger2,
                   [{name, <<"finger2">>},
                    {owner, player},
                    {max_items, 1},
                    {body_part, finger},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {ring1,
                   [{owner, player},
                    {name, <<"ring1">>},
                    {body_parts, [finger]},
                    {icon, clothing},
                    ?ITEM_RULES]},

                  {ring2,
                   [{owner, player},
                    {name, <<"ring2">>},
                    {body_parts, [finger]},
                    {icon, clothing},
                    ?ITEM_RULES]}]).

-define(WORLD_7, [{room,
                   [{visitor, player},
                    {visitor, giant},
                    {name, <<"room">>},
                    {desc, <<"an empty space">>},
                    {item, bread},
                    {icon, room},
                    ?ROOM_RULES]},

                  {player,
                   [{name, <<"Bob">>},
                    {owner, room},
                    {attribute, height0},
                    {attribute, weight0},
                    {attribute, gender0},
                    {attribute, race0},
                    {owner, room},
                    %% TODO is the room property used anywhere?
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {height0,
                   [{owner, player},
                    {type, height},
                    {value, <<"2.2">>},
                    %% XXX I don't think this is used anymore: I have desc templates in mud.app.src
                    {desc, [value, <<"m tall">>]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {weight0,
                   [{owner, player},
                    {type, weight},
                    {value, <<"128">>},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {gender0,
                   [{owner, player},
                    {type, gender},
                    {value, <<"female">>},
                    {desc, [value]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {race0,
                   [{owner, player},
                    {type, race},
                    {value, <<"human">>},
                    {desc, [value]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {giant,
                   [{owner, room},
                    {name, <<"Pete">>},
                    {item, pants},
                    {item, sword},
                    {item, scroll},
                    {body_part, legs0},
                    {body_part, hands0},
                    {attribute, height1},
                    {attribute, weight1},
                    {attribute, gender1},
                    {attribute, race1},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {height1,
                   [{owner, giant},
                    {type, height},
                    {value, <<"4.0">>},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {weight1,
                   [{owner, giant},
                    {type, weight},
                    {value, <<"400.0">>},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {gender1,
                   [{owner, giant},
                    {type, gender},
                    {value, <<"male">>},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {race1,
                   [{owner, giant},
                    {type, race},
                    {value, <<"giant">>},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {legs0, %% if we name this 'legs' then 'legs' will be known as
                   %% as an object ID. If 'legs' is an object identifier
                   %% then a {body_part, legs} property on a body_part,
                   %% i.e. the type of the body part, will be changed
                   %% into {body_part, <PID OF LEGS OBJECT>}
                   [{name, <<"legs">>},
                    {owner, giant},
                    {max_items, 1},
                    {body_part, legs},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {hands0,
                   [{name, <<"hands">>},
                    {owner, giant},
                    {max_items, 1},
                    {body_part, hands},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {pants,
                   [{owner, giant},
                    {body_parts, [legs]},
                    {name, <<"pants_">>},
                    {desc, <<"pants">>},
                    {icon, clothing},
                    ?ITEM_RULES]},

                  {sword,
                   [{owner, giant},
                    {body_parts, [hands]},
                    {name, <<"sword_">>},
                    {desc, <<"sword">>},
                    {icon, weapon},
                    ?ITEM_RULES]},

                  {scroll,
                   [{owner, giant},
                    {body_parts, []},
                    {name, <<"scroll_">>},
                    {desc, <<"scroll">>},
                    {icon, book},
                    ?ITEM_RULES]},

                  {shoes,
                   [{owner, giant},
                    {body_parts, [feet]},
                    {name, <<"shoes_">>},
                    {desc, <<"shoes">>},
                    {icon, clothing},
                    ?ITEM_RULES]},

                  {bread,
                   [{owner, room},
                    {name, <<"bread_">>},
                    {desc, <<"a loaf of bread">>},
                    {icon, food},
                    ?ITEM_RULES]}
                 ]).

-define(WORLD_8, [{room1,
                   [{is_room, true},
                    {visitor, giant},
                    {visitor, player},
                    {item, shield},
                    {item, force_field},
                    {name, <<"room">>},
                    {desc, <<"an empty space">>},
                    {exit, exit_1_2},
                    {icon, room},
                    ?ROOM_RULES]},

                  {room2,
                   [{exit, exit_1_2},
                    {icon, room},
                    ?ROOM_RULES]},

                  {exit_1_2,
                   [{{room, r1}, room1},
                    {{room, r2}, room2},
                    {icon, exit},
                    ?EXIT_RULES]},

                  {player,
                   [{name, <<"Bob">>},
                    {owner, room1},
                    {room, room1},
                    {hitpoints, p_hp},
                    {life, p_life},
                    {attribute, strength0},
                    {attribute, dexterity0},
                    {stamina, p_stamina},
                    {body_part, p_back},
                    {body_part, hand0},
                    {body_part, hand1},
                    {race, race0},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {p_hp,
                   [{hitpoints, 10},
                    {owner, player},
                    {icon, stat},
                    ?HITPOINTS_RULES]},

                  {p_life,
                   [{is_alive, true},
                    {owner, player},
                    {icon, stat},
                    ?LIFE_RULES]},

                  {force_field,
                   [{owner, player},
                    {body_parts, [back]},
                    {wielding_body_parts, [back]},
                    {name, <<"force field">>},
                    {desc, [name]},
                    {defence_damage_modifier, 100},
                    {is_defence, true},
                    {icon, technology},
                    ?ITEM_RULES]},

                  {shield,
                   [{owner, player},
                    {body_parts, [hand]},
                    {wielding_body_parts, [hand]},
                    {name, <<"shield">>},
                    {desc, [name]},
                    {defence_hit_modifier, 100},
                    {is_defence, true},
                    {icon, armor},
                    ?ITEM_RULES]},

                  {strength0,
                   [{owner, player},
                    {type, strength},
                    {value, 17},
                    {attack_damage_modifier, 100},
                    {desc, [<<"strength ">>, value]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {dexterity0,
                   [{owner, player},
                    {type, dexterity},
                    {value, 15},
                    {attack_hit_modifier, 100},
                    {desc, [<<"dexterity ">>, value]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {p_stamina,
                   [{owner, player},
                    {type, stamina},
                    {per_tick, 1},
                    {tick_time, 10},
                    {max, 10},
                    {icon, resource},
                    ?RESOURCE_RULES]},

                  {hand0,
                   [{name, <<"left hand">>},
                    {owner, player},
                    {body_part, hand},
                    {max_items, 1},
                    {item, p_fist},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {hand1,
                   [{name, <<"right hand">>},
                    {owner, player},
                    {body_part, hand},
                    {max_items, 1},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {p_fist,
                   [{name, <<"left fist">>},
                    {owner, hand0},
                    {character, player},
                    {wielding_body_parts, [hand]},
                    {body_part, {?PID(hand0), hand}},
                    {is_attack, true},
                    {is_defence, false},
                    {is_auto_attack, true},
                    {should_attack_module, mud_attack_melee},
                    {should_defend_module, mud_defence_melee},
                    {effect_prototype, p_fist_melee_effect_prototype},
                    {resources, [{stamina, 5}]},
                    {icon, weapon},
                    ?WEAPON_RULES]},

                  {p_fist_melee_effect_prototype,
                   [{owner, p_fist},
                    {character, player},
                    {type, blunt_force},
                    {lifecycle, once},
                    {hit_roll, {1, 10}},
                    {effect_roll, 30},
                    {child_rules, ?EFFECT_RULES}, %% e.g. {child_rules, {rules, [...]}}
                    ?EFFECT_PROTOTYPE_RULES]},

                  {p_back,
                   [{name, <<"back">>},
                    {owner, player},
                    {body_part, back},
                    {max_items, 2},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {giant,
                   [{owner, room1},
                    {name, <<"Pete">>},
                    {hitpoints, g_hp},
                    {life, g_life},
                    {body_part, g_hand_r},
                    {attribute, strength1},
                    {attribute, dexterity1},
                    {attribute, race},
                    {stamina, g_stamina},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {g_hp,
                   [{hitpoints, 310},
                    {owner, giant},
                    {icon, stat},
                    ?HITPOINTS_RULES]},

                  {g_life,
                   [{is_alive, true},
                    {owner, giant},
                    {icon, stat},
                    ?LIFE_RULES]},

                  {g_hand_r,
                   [{name, <<"right hand">>},
                    {owner, giant},
                    {body_part, hand},
                    {item, g_club},
                    {icon, body_part},
                    ?BODY_PART_RULES]},

                  {strength1,
                   [{owner, giant},
                    {type, strength},
                    {value, 17},
                    {attack_damage_modifier, 50},
                    {desc, [<<"strength ">>, value]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {dexterity1,
                   [{owner, player},
                    {type, dexterity},
                    {value, 15},
                    {attack_hit_modifier, 50},
                    {defence_hit_modifier, 50},
                    {desc, [<<"dexterity ">>, value]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {g_stamina,
                   [{owner, giant},
                    {type, stamina},
                    {per_tick, 1},
                    {tick_time, 10},
                    {max, 10},
                    {icon, resource},
                    ?RESOURCE_RULES]},

                  {race0,
                   [{owner, giant},
                    {defence_damage_modifier, 50},
                    {desc, [<<"giant">>]},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]},

                  {g_club,
                   [{name, <<"giant club">>},
                    {attack_damage_modifier, 50},
                    {attack_hit_modifier, 5},
                    {owner, g_hand_r},
                    {character, giant},
                    {wielding_body_parts, [hand]},
                    {body_part, {?PID(g_hand_r), hand}},
                    {is_attack, true},
                    {is_defence, false},
                    {is_auto_attack, true},
                    {should_attack_module, mud_attack_melee},
                    {should_defend_module, mud_defence_melee},
                    {effect_prototype, g_glub_melee_effect_prototype},
                    {attack_type, melee},
                    {resources, [{stamina, 5}]},
                    {icon, weapon},
                    ?ITEM_RULES]},

                  {g_club_melee_effect_prototype,
                   [{owner, p_fist},
                    {character, giant},
                    {type, blunt_force},
                    {lifecycle, once},
                    {hit_roll, {1, 10}},
                    {effect_roll, 30},
                    {child_rules, ?EFFECT_RULES}, %% e.g. {child_rules, {rules, [...]}}
                    ?EFFECT_PROTOTYPE_RULES]}
                 ]).

-define(WORLD_9, [{room,
                   [{visitor, dog},
                    {item, collar},
                    {icon, room},
                    ?ROOM_RULES]},

                  {dog,
                   [{owner, room},
                    {icon, person},
                    ?CHARACTER_RULES]},

                  {collar,
                   [{owner, room},
                    {item, transmitter},
                    {icon, clothing},
                    ?ITEM_RULES]},

                  {transmitter,
                   [{owner, collar},
                    {attribute, stealth},
                    {icon, technology},
                    ?ITEM_RULES]},

                  {stealth,
                   [{owner, transmitter},
                    {icon, stat},
                    ?ATTRIBUTE_RULES]} ]).

-define(WORLD_10, [{room,
                    [{visitor, player},
                     {item, rifle},
                     {exit, exit_1_2},
                     {icon, exit},
                     ?ROOM_RULES]},

                   {player,
                    [{owner, room},
                     {icon, person},
                     ?CHARACTER_RULES]},

                   {rifle,
                    [{owner, room},
                     {name, <<"rifle">>},
                     {item, suppressor},
                     {item, grip},
                     {item, clip},
                     {icon, weapon},
                     ?ITEM_RULES]},

                   {suppressor,
                    [{owner, rifle},
                     {name, <<"suppressor">>},
                     {top_item, rifle},
                     {icon, weapon},
                     ?ITEM_RULES]},

                   {grip,
                    [{owner, rifle},
                     {name, <<"grip">>},
                     {top_item, rifle},
                     {icon, weapon},
                     ?ITEM_RULES]},

                   {clip,
                    [{owner, rifle},
                     {name, <<"clip">>},
                     {top_item, rifle},
                     {item, bullet},
                     {icon, weapon},
                     ?ITEM_RULES]},

                   {bullet,
                    [{owner, clip},
                     {name, <<"bullet">>},
                     {top_item, rifle},
                     {icon, ammo},
                     ?ITEM_RULES]} ]).

-define(WORLD_11, [{room,
                    [{visitor, player},
                     {visitor, giant},
                     {icon, room},
                     ?ROOM_RULES]},

                   {player,
                    [{name, <<"bob">>},
                     {owner, room},
                     {room, room},
                     {icon, person},
                     {resource, p_mana},
                     {spell, fireball_spell},
                     ?CHARACTER_RULES]},

                   {p_mana,
                    [{owner, player},
                     {type, mana},
                     {per_tick, 1},
                     {tick_time, 10},
                     {max, 10},
                     {icon, resource},
                     ?RESOURCE_RULES]},

                   {p_hp,
                    [{hitpoints, 10},
                     {owner, player},
                     {icon, stat},
                     ?HITPOINTS_RULES]},

                   {p_life,
                    [{is_alive, true},
                     {owner, player},
                     {icon, stat},
                     ?LIFE_RULES]},

                   {fireball_spell,
                    [{desc, <<"fireball spell">>},
                     {owner, player},
                     {character, player},
                     {name, <<"fireball">>},
                     {attack_type, spell},
                     {is_attack, true},
                     {is_defence, false},
                     {is_auto_attack, true},

                     {should_attack_module, mud_attack_spell},
                     {should_defend_module, mud_defence_spell},
                     {effect_prototype, p_fireball_effect_prototype},

                     {resources, [{mana, 5}]},
                     {is_memorized, false},
                     {icon, spell},
                     ?SPELL_RULES]},

                   {p_fireball_effect_prototype,
                    [{owner, fireball_spell},
                     {character, player},

                     %% FIXME need to create effect icon?
                     {icon, effect},

                     {type, fire},

                     %% TODO Hmmmm, light them on fire?
                     {lifecycle, once},
                     {hit_roll, {1, 10}},
                     {effect_roll, 10},
                     {child_rules, ?EFFECT_RULES},
                     ?EFFECT_PROTOTYPE_RULES]},

                   {giant,
                    [{owner, room},
                     {room, room},
                     {name, <<"Pete">>},
                     {hitpoints, g_hp},
                     {life, g_life},
                     {resource, g_stamina},
                     {icon, person},
                     ?CHARACTER_RULES]},

                   {g_hp,
                    [{hitpoints, 2},
                     {owner, giant},
                     {icon, stat},
                     ?HITPOINTS_RULES]},

                   {g_life,
                    [{is_alive, true},
                     {owner, giant},
                     {icon, stat},
                     ?LIFE_RULES]},

                   {g_foot,
                    [{name, <<"left foot">>},
                     {attack_damage, 50},
                     {attack_hit, 1},
                     {owner, giant},
                     {character, giant},
                     {is_attack, true},
                     {is_auto_attack, true},
                     {resources, [{stamina, 5}]},
                     {icon, body_part},
                     ?ITEM_RULES]},

                   {g_stamina,
                    [{owner, player},
                     {type, mana},
                     {per_tick, 1},
                     {tick_time, 10},
                     {max, 10},
                     {icon, resource},
                     ?RESOURCE_RULES]}]).

-define(WORLD_12, [{room,
                    [{visitor, player},
                     {visitor, zombie},
                     {item, bucket},
                     {icon, room},
                     ?ROOM_RULES]},

                   {player,
                    [{owner, room},
                     {icon, person},
                     ?CHARACTER_RULES]},

                   {zombie,
                    [{owner, room},
                     {name, <<"Arlene">>},
                     {life, z_life},
                     {body_part, z_hand_left},
                     {body_part, z_hand_right},
                     {item, coin},
                     {item, book},
                     {icon, person},
                     ?CHARACTER_RULES]},

                   {z_hand_left,
                    [{owner, zombie},
                     {item, sword},
                     {icon, person},
                     ?BODY_PART_RULES]},

                   {z_hand_right,
                    [{owner, zombie},
                     {item, shield},
                     {icon, person},
                     ?BODY_PART_RULES]},

                   {coin,
                    [{owner, zombie},
                     {icon, item},
                     {name, <<"coin name">>},
                     {desc, <<"coin desc">>},
                     {drop_on_death, true},
                     ?ITEM_RULES]},

                   {bucket,
                    [{owner, room},
                     {icon, item},
                     {name, <<"bucket name">>},
                     {desc, <<"bucket desc">>},
                     {drop_on_death, true},
                     ?ITEM_RULES]},

                   {book,
                    [{owner, zombie},
                     {icon, book},
                     {name, <<"book name">>},
                     {desc, <<"book desc">>},
                     {drop_on_death, true},
                     ?ITEM_RULES]},

                   {sword,
                    [{owner, z_hand_left},
                     {icon, weapon},
                     {name, <<"sword name">>},
                     {desc, <<"sword desc">>},
                     {drop_on_death, true},
                     ?ITEM_RULES]},

                   {shield,
                    [{owner, z_hand_right},
                     {icon, weapon},
                     {name, <<"shield name">>},
                     {desc, <<"shield desc">>},
                     {drop_on_death, true},
                     ?ITEM_RULES]}]).
