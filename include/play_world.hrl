%% This world is used when trying out erlmud from the console.

%%  Create rooms on a grid named after their X,Y coordinate
%%  positions
%%
%%       Room 5,3
%%         |
%%         |
%%       Exit 5,4:s <-> 5,3:n
%%         |
%%         |
%%       Room 5,4
%%         |
%%         |
%%       Exit 5,5:s <-> 5,4:n
%%         |
%%         |
%%       Room 5,5  --- Exit 5,5:w <-> 6,5:e --- Room 6,5

-define(WORLD, [{room_5_5, [{is_room, true},
                            {character, giant},
                            {item, shield},
                            {item, force_field},
                            {name, <<"room">>},
                            {desc, <<"an empty space">>},
                            {icon, room},
                            {exit, exit_5_5_and_5_4},
                            ?ROOM_HANDLERS]},

                {room_5_4, [{exit, exit_5_5_and_5_4},
                            {icon, room},
                            ?ROOM_HANDLERS]},

                {room_5_3, [{exit, exit_5_4_and_5_3},
                            {icon, room},
                            ?ROOM_HANDLERS]},

                {room_6_5, [{exit, exit_5_5_and_6_5},
                            {icon, room},
                            ?ROOM_HANDLERS]},

                {exit_5_5_and_5_4,
                           [{{room, n}, room_5_4},
                            {{room, s}, room_5_5},
                            {icon, exit},
                            ?EXIT_HANDLERS]},

                {exit_5_4_and_5_3,
                           [{{room, n}, room_5_3},
                            {{room, s}, room_5_4},
                            {icon, exit},
                            ?EXIT_HANDLERS]},

                {exit_5_5_and_6_5,
                           [{{room, e}, room_6_5},
                            {{room, w}, room_5_5},
                            {icon, exit},
                            ?EXIT_HANDLERS]},

                {player, [{name, <<"Bob">>},
                          {owner, room_5_5},
                          {room, room_5_5},
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
                          ?CHARACTER_HANDLERS]},

                {p_hp, [{hitpoints, 10},
                        {owner, player},
                        {icon, stat},
                        ?HITPOINTS_HANDLERS]},

                {p_life, [{is_alive, true},
                          {owner, player},
                          {icon, stat},
                          ?LIFE_HANDLERS]},

                {force_field, [{owner, player},
                               {body_parts, [back]},
                               {wielding_body_parts, [back]},
                               {name, <<"force field">>},
                               {desc, [name]},
                               {defence_damage_modifier, 100},
                               {is_defence, true},
                               {icon, technology},
                               ?ITEM_HANDLERS]},

                {sword, [{desc, "sword"},
                         {owner, player},
                         {body_parts, [hand]},
                         {wielding_body_parts, [hand]},
                         {name, <<"sword">>},
                         {desc, [name]},
                         {defence_hit_modifier, 0},
                         {attack_damage_modifier, 20},
                         {is_defence, true},
                         {icon, weapon},
                         ?ITEM_HANDLERS]},

                {shield, [{owner, player},
                          {body_parts, [hand]},
                          {wielding_body_parts, [hand]},
                          {name, <<"shield">>},
                          {desc, [name]},
                          {defence_hit_modifier, 100},
                          {is_defence, true},
                         {icon, weapon},
                          ?ITEM_HANDLERS]},

                {strength0, [{owner, player},
                             {type, strength},
                             {value, 17},
                             {attack_damage_modifier, 100},
                             {desc, [<<"strength ">>, value]},
                             {icon, stat},
                             ?ATTRIBUTE_HANDLERS]},

                {dexterity0, [{owner, player},
                              {type, dexterity},
                              {value, 15},
                              {attack_hit_modifier, 100},
                              {desc, [<<"dexterity ">>, value]},
                              {icon, stat},
                              ?ATTRIBUTE_HANDLERS]},

                {p_stamina, [{owner, player},
                             {type, stamina},
                             {per_tick, 1},
                             {tick_time, 10},
                             {max, 10},
                             {icon, stat},
                             ?RESOURCE_HANDLERS]},

                {hand0,   [{name, <<"left hand">>},
                           {owner, player},
                           {body_part, hand},
                           {max_items, 1},
                           {item, p_fist},
                           {icon, body_part},
                           ?BODY_PART_HANDLERS]},

                {hand1,   [{name, <<"right hand">>},
                           {owner, player},
                           {body_part, hand},
                           {max_items, 1},
                           {icon, body_part},
                           ?BODY_PART_HANDLERS]},

                {p_fist, [{name, <<"left fist">>},
                          {attack_damage_modifier, 50},
                          {attack_hit_modifier, 1},
                          {owner, p_hand},
                          {character, player},
                          {wielding_body_parts, [hand]},
                          {body_part, {?PID(hand0), hand}},
                          {is_attack, true},
                          {is_auto_attack, true},
                          {resources, [{stamina, 5}]},
                          {icon, body_part},
                          ?ITEM_HANDLERS]},

                {p_back,   [{name, <<"back">>},
                           {owner, player},
                           {body_part, back},
                           {max_items, 2},
                           {icon, body_part},
                           ?BODY_PART_HANDLERS]},

                {giant, [{owner, room_5_4},
                         {name, <<"Pete">>},
                         {hitpoints, g_hp},
                         {life, g_life},
                         {body_part, g_hand_r},
                         {attribute, strength1},
                         {attribute, dexterity1},
                         {attribute, race},
                         {stamina, g_stamina},
                         {icon, person},
                         ?CHARACTER_HANDLERS]},

                {g_hp, [{hitpoints, 310},
                        {owner, giant},
                        {icon, stat},
                        ?HITPOINTS_HANDLERS]},

                {g_life, [{is_alive, true},
                          {owner, giant},
                          {icon, stat},
                          ?LIFE_HANDLERS]},

                {g_hand_r, [{name, <<"right hand">>},
                            {owner, giant},
                            {body_part, hand},
                            {item, g_club},
                            {icon, body_part},
                            ?BODY_PART_HANDLERS]},

                {strength1, [{owner, giant},
                             {type, strength},
                             {value, 17},
                             {attack_damage_modifier, 50},
                             {desc, [<<"strength ">>, value]},
                             {icon, stat},
                             ?ATTRIBUTE_HANDLERS]},

                {dexterity1, [{owner, player},
                              {type, dexterity},
                              {value, 15},
                              {attack_hit_modifier, 50},
                              {defence_hit_modifier, 50},
                              {desc, [<<"dexterity ">>, value]},
                              {icon, stat},
                              ?ATTRIBUTE_HANDLERS]},

                {g_stamina, [{owner, giant},
                             {type, stamina},
                             {per_tick, 1},
                             {tick_time, 10},
                             {max, 10},
                             {icon, stat},
                             ?RESOURCE_HANDLERS]},

                {race0, [{owner, giant},
                         {defence_damage_modifier, 50},
                         {desc, [<<"giant">>]},
                         {icon, stat},
                         ?ATTRIBUTE_HANDLERS]},

                {g_club, [{name, <<"giant club">>},
                          {attack_damage_modifier, 50},
                          {attack_hit_modifier, 5},
                          {owner, g_hand_r},
                          {character, giant},
                          {wielding_body_parts, [hand]},
                          {body_part, {?PID(g_hand_r), hand}},
                          {is_attack, true},
                          {is_auto_attack, true},
                          {resources, [{stamina, 5}]},
                          {icon, weapon},
                          ?ITEM_HANDLERS]}
                 ]).
