-define(WORLD_1, [{erlmud_room, room_nw, [{exit, exit_ns}, {exit, exit_ew}, {character, player}]},
                  {erlmud_room, room_s, [{exit, exit_ns}]},
                  {erlmud_room, room_e, [{exit, exit_ew}]},
                  {erlmud_character, player, [{room, room_nw}]},
                  {erlmud_exit, exit_ns, [{{room, n}, room_nw}, {{room, s}, room_s}]},
                  {erlmud_exit, exit_ew, [{{room, w}, room_nw}, {{room, e}, room_e}, {is_locked, true}]}]).

-define(WORLD_2, [{erlmud_room, room, [{player, player}, {item, sword}, {item, apple}]},
                  {erlmud_character, player, [{room, room}, {item, helmet}]},
                  {erlmud_item, sword, [{owner, room}, {name, <<"sword">>}]},
                  {erlmud_item, helmet, [{owner, player}, {name, <<"helmet">>}]},
                  {erlmud_item, apple, [{owner, room}, {name, <<"apple">>}]}]).

-define(WORLD_3, [{erlmud_room, room, [{character, player},
                                       {character, zombie}]},

                  {erlmud_character, player, [{room, room},
                                              {attack_wait, 10},
                                              {item, fist},
                                              {hitpoints, p_hp},
                                              {life, p_life}]},
                  {erlmud_hitpoints, p_hp, [{hitpoints, 1000},
                                            {owner, player}]},
                  {erlmud_life, p_life, [{is_alive, true},
                                         {owner, player}]},
                  {erlmud_item, fist, [{dmg, 5},
                                       {owner, player}]},

                  {erlmud_character, zombie, [{room, room},
                                              {attack_wait, 10},
                                              {item, sword},
                                              {name, <<"zombie">>},
                                              {hitpoints, z_hp},
                                              {life, z_life}]},
                  {erlmud_hitpoints, z_hp, [{hitpoints, 10},
                                            {owner, zombie}]},
                  {erlmud_life, z_life, [{is_alive, true},
                                         {owner, zombie}]},
                  {erlmud_item, sword, [{dmg, 5},
                                        {owner, zombie}]}]).

-define(WORLD_4, [{erlmud_room, room, [{player, player}]},
                  {erlmud_character, player, [{room, room},
                                           {item, helmet},
                                           {body_part, head}]},
                  {erlmud_body_part, head, [{name, <<"head">>}, {owner, player}]},
                  {erlmud_item, helmet, [{name, <<"helmet">>}, {owner, player}]}]).

-define(WORLD_5, [{erlmud_character, player, [{item, helmet},
                                           {body_part, head1},
                                           {body_part, finger1}]},
                  {erlmud_body_part, head1, [{name, <<"head">>},
                                             {owner, player},
                                             {body_part, head}]},
                  {erlmud_body_part, finger1, [{name, <<"finger">>},
                                               {owner, player},
                                               {body_part, finger}]},
                  {erlmud_item, helmet, [{owner, player},
                                         {name, <<"helmet">>},
                                         {body_parts, [head, hand]}]}]).

-define(WORLD_6, [{erlmud_character, player, [{body_part, finger1},
                                              {body_part, finger2},
                                              {item, ring1},
                                              {item, ring2}]},
                  {erlmud_body_part, finger1, [{name, <<"finger1">>},
                                               {owner, player},
                                               {max_items, 1},
                                               {body_part, finger}]},
                  {erlmud_body_part, finger2, [{name, <<"finger2">>},
                                               {owner, player},
                                               {max_items, 1},
                                               {body_part, finger}]},
                  {erlmud_item, ring1, [{owner, player},
                                        {name, <<"ring1">>},
                                        {body_parts, [finger]}]},
                  {erlmud_item, ring2, [{owner, player},
                                        {name, <<"ring2">>},
                                        {body_parts, [finger]}]}]).

-define(WORLD_7, [{erlmud_room, room, [{character, giant},
                                       {name, <<"room">>}]},

                  {erlmud_character, player, [{room, room},
                                              {name, <<"Bob">>},
                                              {species, <<"human">>}
                                              ]},

                  {erlmud_character, giant, [{room, room},
                                             {name, <<"Pete">>},
                                             {species, <<"Giant">>},
                                             {gender, <<"male">>},
                                             {height, <<"4">>},
                                             {weight, <<"400">>},
                                             {item, pants},
                                             {item, sword},
                                             {item, scroll},
                                             {body_part, legs0},
                                             {body_part, hands0}
                                            ]},

                  {erlmud_body_part, legs0, %% if we name this 'legs' then 'legs' will be known as
                                            %% as an object ID. If 'legs' is an object identifier
                                            %% then a {body_part, legs} property on a body_part,
                                            %% i.e. the type of the body part, will be changed
                                            %% into {body_part, <PID OF LEGS OBJECT>}
                                              [{name, <<"legs">>},
                                               {owner, giant},
                                               {max_items, 1},
                                               {body_part, legs}]},
                  {erlmud_body_part, hands0,   [{name, <<"hands">>},
                                                {owner, giant},
                                                {max_items, 1},
                                                {body_part, hands}]},

                  {erlmud_item, pants, [{owner, giant},
                                        {body_parts, [legs]},
                                        {name, <<"pants_">>}]},
                  {erlmud_item, sword, [{owner, giant},
                                        {body_parts, [hands]},
                                        {name, <<"sword_">>}]},
                  {erlmud_item, scroll, [{owner, giant},
                                         {body_parts, []},
                                         {name, <<"scroll_">>}]},
                  {erlmud_item, shoes, [{owner, giant},
                                        {body_parts, [feet]},
                                        {name, <<"shoes_">>}]}]).
