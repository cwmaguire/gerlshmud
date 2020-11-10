-define(UNIVERSAL_HANDLERS, gerlshmud_handler_set_child_property).

-define(ROOM_HANDLERS, {handlers, [gerlshmud_handler_room_inject_self,
                                   gerlshmud_handler_room_inv,
                                   gerlshmud_handler_room_look,
                                   gerlshmud_handler_room_move,
                                   ?UNIVERSAL_HANDLERS]}).

-define(CHARACTER_HANDLERS, {handlers, [gerlshmud_handler_char_attack,
                                        gerlshmud_handler_char_look,
                                        gerlshmud_handler_char_inv,
                                        gerlshmud_handler_char_move,
                                        gerlshmud_handler_char_inject_self,
                                        gerlshmud_handler_char_enter_world,
                                        ?UNIVERSAL_HANDLERS]}).

-define(ITEM_HANDLERS, {handlers, [gerlshmud_handler_item_look,
                                   gerlshmud_handler_item_inv,
                                   gerlshmud_handler_item_inject_self,
                                   ?UNIVERSAL_HANDLERS]}).

-define(CONN_HANDLERS, {handlers, [gerlshmud_handler_conn_enter_world,
                                   gerlshmud_handler_conn_move,
                                   gerlshmud_handler_conn_send,
                                   ?UNIVERSAL_HANDLERS]}).

-define(BODY_PART_HANDLERS, {handlers, [gerlshmud_handler_body_part_look,
                                        gerlshmud_handler_body_part_inv,
                                        gerlshmud_handler_body_part_inject_self,
                                        ?UNIVERSAL_HANDLERS]}).

-define(ATTRIBUTE_HANDLERS, {handlers, [gerlshmud_handler_attribute_look,
                                        gerlshmud_handler_attribute_attack,
                                        ?UNIVERSAL_HANDLERS]}).

-define(EXIT_HANDLERS, {handlers, [gerlshmud_handler_exit_move,
                                   gerlshmud_handler_exit_look,
                                   ?UNIVERSAL_HANDLERS]}).

-define(HITPOINTS_HANDLERS, {handlers, [gerlshmud_handler_hitpoints_attack,
                                        ?UNIVERSAL_HANDLERS]}).

-define(LIFE_HANDLERS, {handlers, [gerlshmud_handler_life_attack,
                                   ?UNIVERSAL_HANDLERS]}).

-define(STAT_HANDLERS, {handlers, [gerlshmud_handler_stat_look,
                                   ?UNIVERSAL_HANDLERS]}).

-define(RESOURCE_HANDLERS, {handlers, [gerlshmud_handler_resource_inject_self,
                                       gerlshmud_handler_resource_tick,
                                       gerlshmud_handler_resource_reserve,
                                       ?UNIVERSAL_HANDLERS]}).

-define(SPELL_HANDLERS, {handlers, [gerlshmud_handler_attack,
                                    gerlshmud_handler_spell_memorize,
                                    gerlshmud_handler_spell_inject_self,
                                    ?UNIVERSAL_HANDLERS]}).

-define(EFFECT_HANDLERS, {handlers, [gerlshmud_handler_effect_create,
                                     gerlshmud_handler_effect_attack,
                                     ?UNIVERSAL_HANDLERS]}).

-define(WEAPON_HANDLERS, {handlers, [gerlshmud_handler_attack |
                                     ?ITEM_HANDLERS]).
