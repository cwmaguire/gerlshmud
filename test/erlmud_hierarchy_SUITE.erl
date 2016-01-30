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
    H = fun(X) -> erlmud_hierarchy:new(X) end,
    I = fun(H1, X) -> {ok, H2} = erlmud_hierarchy:insert(H1, X), H2 end,
    random:seed(os:timestamp()),
    [R1, R2, R3, R4] = [random:uniform(1000) || _ <- lists:seq(1,4)],
    {R1, []} = H(R1),
    {R1, [{R2, []}]} = I(H(R1), {R1, R2}),
    {R1, [{R2, [{R3, []}]}]} = I(I(H(R1),{R1, R2}), {R2, R3}),
    {R1, [{R2, [{R3, [{R4, []}]}]}]} = I(I(I(H(R1), {R1, R2}), {R2, R3}), {R3, R4}).
