-ifndef(MUD_RULES_HRL).

-include("mud.hrl").

-define(MUD_RULES_HRL, already_included).

-define(UNIVERSAL_RULES, [rules_set_child_property,
                          rules_delete_property,
                          rules_stop]).

-define(ROOM_RULES, {rules, [rules_room_inject_self,
                             rules_room_inv,
                             rules_room_look,
                             rules_room_move |
                             ?UNIVERSAL_RULES]}).

-define(CHARACTER_RULES, {rules, [rules_char_attack,
                                  rules_char_look,
                                  rules_char_inv,
                                  rules_char_move,
                                  rules_char_inject_self,
                                  rules_char_enter_world,
                                  rules_char_search,
                                  rules_char_cleanup,
                                  rules_char_say,
                                  rules_char_shout,
                                  rules_char_emote,
                                  rules_char_send,
                                  rules_char_achievement,
                                  rules_char_effect,
                                  rules_char_quest_giver,
                                  rules_char_quest_doer |
                                  ?UNIVERSAL_RULES]}).

-define(ITEM_RULES, {rules, [rules_item_look,
                             rules_item_inv,
                             rules_item_inject_self,
                             rules_item_search,
                             rules_item_cleanup,
                             rules_item_effect |
                             ?UNIVERSAL_RULES]}).

-define(CONN_RULES, {rules, [rules_conn_enter_world,
                             rules_conn_move,
                             rules_conn_send |
                             ?UNIVERSAL_RULES]}).

-define(BODY_PART_RULES, {rules, [rules_body_part_look,
                                  rules_body_part_inv,
                                  rules_body_part_inject_self,
                                  rules_body_part_search,
                                  rules_body_part_cleanup |
                                  ?UNIVERSAL_RULES]}).

-define(ATTRIBUTE_RULES, {rules, [rules_attribute_look,
                                  rules_attribute_attack,
                                  rules_attribute_modify |
                                  ?UNIVERSAL_RULES]}).

-define(EXIT_RULES, {rules, [rules_exit_move,
                             rules_exit_look,
                             rules_exit_shout |
                             ?UNIVERSAL_RULES]}).

-define(HITPOINTS_RULES, {rules, [rules_hitpoints_attack,
                                  rules_hitpoints_recover |
                                  ?UNIVERSAL_RULES]}).

-define(LIFE_RULES, {rules, [rules_life_attack,
                             rules_life_inject_self |
                             ?UNIVERSAL_RULES]}).

-define(STAT_RULES, {rules, [rules_stat_look |
                             ?UNIVERSAL_RULES]}).

-define(RESOURCE_RULES, {rules, [rules_resource_inject_self,
                                 rules_resource_tick,
                                 rules_resource_reserve |
                                 ?UNIVERSAL_RULES]}).

-define(SPELL_RULES, {rules, [rules_attack, %% TODO use macro like ?RULES_SPELL_ATTACK
                              rules_spell_ack,
                              rules_spell_cast,
                              rules_spell_memorize,
                              rules_spell_inject_self |
                              ?UNIVERSAL_RULES]}).

-define(EFFECT_PROTOTYPE_RULES, {rules, [rules_effect_create]}).

-define(EFFECT_RULES, {rules, [rules_effect_ack,
                               rules_effect_attack,
                               rules_effect_inject_self |
                               ?UNIVERSAL_RULES]}).

-define(WEAPON_RULES, {rules, [rules_attack,
                               rules_weapon_effect |
                               element(2, ?ITEM_RULES)]}).

-define(ARMOR_RULES, {rules, [rules_defend |
                              element(2, ?ITEM_RULES)]}).

-define(EXPERIENCE_RULES, {rules, [rules_experience_gain |
                                   ?UNIVERSAL_RULES]}).

-define(LEVEL_RULES, {rules, [rules_level_gain |
                              ?UNIVERSAL_RULES]}).

-define(ACHIEVEMENT_GOT_WOOD_1_RULES,
        {rules, [rules_achievement_got_wood_1 |
                 ?UNIVERSAL_RULES]}).

-define(QUEST_RULES,
        {rules, [rules_quest_init,
                 rules_quest_get,
                 rules_quest_ack,
                 rules_quest_turn_in,
                 rules_quest_exists,
                 rules_quest_killed,
                 rules_quest_item_get |
                 ?UNIVERSAL_RULES]}).

-define(CHARACTER_METRICS_RULES,
        {rules, [rules_char_metrics |
                 ?UNIVERSAL_RULES]}).

-define(TIME_RULES,
        {rules, [rules_time_effect]}).

-define(QUEST_KILL_RAT_AT_NOON_WITH_FIST_WEARING_GLOVE,
        {rules, [rules_quest_kill_rat_at_noon_with_fist_wearing_glove]}).

-define(life(Id, Owner, Props),
        {Id,
         [{owner, Owner},
          {is_alive, true},
          {icon, stat},
          ?LIFE_RULES |
          Props]}).

-define(exp(Id, Owner, Gained, Gives, Props),
        {Id,
         [{owner, Owner},
          {gained, Gained},
          {gives, Gives},
          ?EXPERIENCE_RULES |
          Props]}).

-define(hp(Id, Owner, HP, Props),
        ?hp(Id, Owner, HP, HP, Props)).

-define(hp(Id, Owner, HP, Max, Props),
        {Id,
         [{owner, Owner},
          {hitpoints, HP},
          {max, Max},
          {icon, stat},
          ?HITPOINTS_RULES |
          Props]}).

-define(hand(Id, Owner, Name, Props),
        {Id,
         [{name, Name},
          {owner, Owner},
          {body_part, hand},
          {max_items, 1},
          {icon, body_part},
          ?BODY_PART_RULES |
          Props]}).

-define(attribute(Id, AttHitMod, DefHitMod, AttDmgMod, DefDmgMod, Owner, Props),
        {Id,
         [{attack_hit_modifier, AttHitMod},
          {defence_hit_modifier, DefHitMod},
          {attack_dmg_modifier, AttDmgMod},
          {defence_dmg_modifier, DefDmgMod},
          {owner, Owner},
          {character, Owner},
          {icon, stat},
          ?ATTRIBUTE_RULES |
          Props]}).

-define(resource(Id, Owner, Type, PerTick, TickMillis, Max, Props),
        {Id,
         [{owner, Owner},
          {type, Type},
          {per_tick, PerTick},
          {tick_time, TickMillis},
          {current, Max},
          {max, Max},
          {icon, resource},
          ?RESOURCE_RULES |
          Props]}).

-define(clothing(Id, Name, Owner, Char, BodyParts, Props),
        {Id,
         [{name, Name},
          {owner, Owner},
          {character, Char},
          {body_parts, BodyParts},
          {icon, armor},
          ?ITEM_RULES]}).

-endif.
