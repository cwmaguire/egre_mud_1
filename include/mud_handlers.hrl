-ifndef(GERLSHMUD_HANDLERS_HRL).
-define(GERLSHMUD_HANDLERS_HRL,already_included).

-define(UNIVERSAL_HANDLERS, [mud_handler_set_child_property,
                             mud_handler_delete_property,
                             mud_handler_stop]).

-define(ROOM_HANDLERS, {handlers, [mud_handler_room_inject_self,
                                   mud_handler_room_inv,
                                   mud_handler_room_look,
                                   mud_handler_room_move |
                                   ?UNIVERSAL_HANDLERS]}).

-define(CHARACTER_HANDLERS, {handlers, [mud_handler_char_attack,
                                        mud_handler_char_look,
                                        mud_handler_char_inv,
                                        mud_handler_char_move,
                                        mud_handler_char_inject_self,
                                        mud_handler_char_enter_world,
                                        mud_handler_char_search,
                                        mud_handler_char_cleanup |
                                        ?UNIVERSAL_HANDLERS]}).

-define(ITEM_HANDLERS, {handlers, [mud_handler_item_look,
                                   mud_handler_item_inv,
                                   mud_handler_item_inject_self,
                                   mud_handler_item_search,
                                   mud_handler_item_cleanup |
                                   ?UNIVERSAL_HANDLERS]}).

-define(CONN_HANDLERS, {handlers, [mud_handler_conn_enter_world,
                                   mud_handler_conn_move,
                                   mud_handler_conn_send |
                                   ?UNIVERSAL_HANDLERS]}).

-define(BODY_PART_HANDLERS, {handlers, [mud_handler_body_part_look,
                                        mud_handler_body_part_inv,
                                        mud_handler_body_part_inject_self,
                                        mud_handler_body_part_search,
                                        mud_handler_body_part_cleanup |
                                        ?UNIVERSAL_HANDLERS]}).

-define(ATTRIBUTE_HANDLERS, {handlers, [mud_handler_attribute_look,
                                        mud_handler_attribute_attack |
                                        ?UNIVERSAL_HANDLERS]}).

-define(EXIT_HANDLERS, {handlers, [mud_handler_exit_move,
                                   mud_handler_exit_look |
                                   ?UNIVERSAL_HANDLERS]}).

-define(HITPOINTS_HANDLERS, {handlers, [mud_handler_hitpoints_attack |
                                        ?UNIVERSAL_HANDLERS]}).

-define(LIFE_HANDLERS, {handlers, [mud_handler_life_attack |
                                   ?UNIVERSAL_HANDLERS]}).

-define(STAT_HANDLERS, {handlers, [mud_handler_stat_look |
                                   ?UNIVERSAL_HANDLERS]}).

-define(RESOURCE_HANDLERS, {handlers, [mud_handler_resource_inject_self,
                                       mud_handler_resource_tick,
                                       mud_handler_resource_reserve |
                                       ?UNIVERSAL_HANDLERS]}).

-define(SPELL_HANDLERS, {handlers, [mud_handler_attack,
                                    mud_handler_spell_memorize,
                                    mud_handler_spell_inject_self |
                                    ?UNIVERSAL_HANDLERS]}).

-define(EFFECT_PROTOTYPE_HANDLERS, {handlers, [mud_handler_effect_create]}).

-define(EFFECT_HANDLERS, {handlers, [mud_handler_effect_attack |
                                     ?UNIVERSAL_HANDLERS]}).

-define(WEAPON_HANDLERS, {handlers, [mud_handler_attack |
                                     element(2, ?ITEM_HANDLERS)]}).

-define(ARMOR_HANDLERS, {handlers, [mud_handler_defend |
                                    element(2, ?ITEM_HANDLERS)]}).

-endif.
