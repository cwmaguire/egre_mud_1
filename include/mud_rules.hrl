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
                                  rules_char_cleanup |
                                  ?UNIVERSAL_RULES]}).

-define(ITEM_RULES, {rules, [rules_item_look,
                             rules_item_inv,
                             rules_item_inject_self,
                             rules_item_search,
                             rules_item_cleanup |
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
                                  rules_attribute_attack |
                                  ?UNIVERSAL_RULES]}).

-define(EXIT_RULES, {rules, [rules_exit_move,
                             rules_exit_look |
                             ?UNIVERSAL_RULES]}).

-define(HITPOINTS_RULES, {rules, [rules_hitpoints_attack |
                                  ?UNIVERSAL_RULES]}).

-define(LIFE_RULES, {rules, [rules_life_attack |
                             ?UNIVERSAL_RULES]}).

-define(STAT_RULES, {rules, [rules_stat_look |
                             ?UNIVERSAL_RULES]}).

-define(RESOURCE_RULES, {rules, [rules_resource_inject_self,
                                 rules_resource_tick,
                                 rules_resource_reserve |
                                 ?UNIVERSAL_RULES]}).

-define(SPELL_RULES, {rules, [rules_attack,
                              rules_spell_memorize,
                              rules_spell_inject_self |
                              ?UNIVERSAL_RULES]}).

-define(EFFECT_PROTOTYPE_RULES, {rules, [rules_effect_create]}).

-define(EFFECT_RULES, {rules, [rules_effect_attack |
                               ?UNIVERSAL_RULES]}).

-define(WEAPON_RULES, {rules, [rules_attack |
                               element(2, ?ITEM_RULES)]}).

-define(ARMOR_RULES, {rules, [rules_defend |
                              element(2, ?ITEM_RULES)]}).

-endif.
