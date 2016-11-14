-module(erlmud_attack).

-export([update_attack/3]).
-export([add_value/2]).

-include("include/erlmud.hrl").

add_value(Attack = #attack{calc_type = hit, hit = Hit}, Value) ->
    Attack#attack{hit = Hit + Value};
add_value(Attack = #attack{calc_type = damage, damage = Damage}, Value) ->
    Attack#attack{damage = Damage + Value};
add_value(Attack = #attack{calc_type = wait, wait = Wait}, Value) ->
    Attack#attack{wait = Wait + Value}.

update_attack(Attack = #attack{}, SourceOrTarget, Props) ->
    Modifier = modifier(Attack#attack.calc_type, SourceOrTarget, Props),
    log(debug, [self(), <<": Modifier is: ">>, Modifier]),
    UpdatedAttack = erlmud_attack:add_value(Attack, Modifier),
    {succeed, UpdatedAttack, false, Props}.

modifier(CalcType, SourceOrTarget, Props) ->
    {AttackModifierProp, DefenceModifierProp} =
        case CalcType of
            hit ->
                {attack_hit_modifier, defence_hit_modifier};
            damage ->
                {attack_damage_modifier, defence_damage_modifier}
        end,
    log(debug, [self(), <<": Modifier prperties are: ">>, AttackModifierProp, <<", ">>, DefenceModifierProp]),
    Modifier = case SourceOrTarget of
                   source ->
                       proplists:get_value(AttackModifierProp, Props, 0);
                   target ->
                       proplists:get_value(DefenceModifierProp, Props, 0)
               end,
    log(debug, [self(), <<": Modifier is: ">>, Modifier]),
    Modifier.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
