-module(erlmud_hierarchy_SUITE).
-compile(export_all).

all() ->
    [new,
     insert].

new(_Config) ->
    random:seed(os:timestamp()),
    Rand = random:uniform(1000),
    {Rand, []} = erlmud_hierarchy:new(Rand).

insert(_Config) ->
    random:seed(os:timestamp()),
    [R1, R2, R3, R4] = [random:uniform(1000) || _ <- lists:seq(1,4)],
    {R1, []} = erlmud_hierarchy:new(R1),
    {R1, [{R2, []}]} = erlmud_hierarchy:insert(R1, R2),
    {R1, [{R2, [{R3, []}]}]} = erlmud_hierarchy:insert(R2, R3),
    {R1, [{R2, [{R3, []}]}, {R4, []}]} = erlmud_hierarchy:insert(R3, R4).
