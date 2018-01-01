-define(UNIVERSAL_HANDLERS, erlmud_handler_set_child_property).

-define(ROOM_HANDLERS, {handlers, [erlmud_handler_room_inject_self,
                                   erlmud_handler_room_inv,
                                   erlmud_handler_room_look,
                                   erlmud_handler_room_move,
                                   ?UNIVERSAL_HANDLERS]}).

-define(CHARACTER_HANDLERS, {handlers, [erlmud_handler_char_attack,
                                        erlmud_handler_char_look,
                                        erlmud_handler_char_inv,
                                        erlmud_handler_char_move,
                                        erlmud_handler_char_inject_self,
                                        erlmud_handler_char_enter_world,
                                        %erlmud_handler_counterattack,
                                        ?UNIVERSAL_HANDLERS]}).

-define(ITEM_HANDLERS, {handlers, [erlmud_handler_item_attack,
                                   erlmud_handler_item_look,
                                   erlmud_handler_item_inv,
                                   erlmud_handler_item_inject_self,
                                   ?UNIVERSAL_HANDLERS]}).

-define(CONN_HANDLERS, {handlers, [erlmud_handler_conn_enter_world,
                                   erlmud_handler_conn_move,
                                   erlmud_handler_conn_send,
                                   ?UNIVERSAL_HANDLERS]}).

-define(BODY_PART_HANDLERS, {handlers, [erlmud_handler_body_part_look,
                                        erlmud_handler_body_part_inv,
                                        erlmud_handler_body_part_inject_self,
                                        ?UNIVERSAL_HANDLERS]}).

-define(ATTRIBUTE_HANDLERS, {handlers, [erlmud_handler_attribute_look,
                                        erlmud_handler_attribute_attack,
                                        ?UNIVERSAL_HANDLERS]}).

-define(EXIT_HANDLERS, {handlers, [erlmud_handler_exit_move,
                                   ?UNIVERSAL_HANDLERS]}).

-define(HITPOINTS_HANDLERS, {handlers, [erlmud_handler_hitpoints_attack,
                                        ?UNIVERSAL_HANDLERS]}).

-define(LIFE_HANDLERS, {handlers, [erlmud_handler_life_attack,
                                   ?UNIVERSAL_HANDLERS]}).

-define(STAT_HANDLERS, {handlers, [erlmud_handler_stat_look,
                                   ?UNIVERSAL_HANDLERS]}).

-define(RESOURCE_HANDLERS, {handlers, [erlmud_handler_resource_inject_self,
                                       erlmud_handler_resource_tick,
                                       erlmud_handler_resource_reserve,
                                       ?UNIVERSAL_HANDLERS]}).

