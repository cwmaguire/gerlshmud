-module(erlmud_attack).

%% This looks to be a utility module to hold the common code to
%% update an attack event. Several different handlers will use this
%% same code

%% I don't think I'm going to use this: for now I'd rather have really
%% descriptive events like {Character, calc, Hit, on, Target, with, Weapon}.
%% If I have #attack{character = Character,
%%                   target = Target,
%%                   calc = hit,
%%                   weapon = Weapon} ...
%% ... it gives me more flexibility, but doesn't read as well.
%% I dunno, we'll see.
%%
%% One problem with {C, calc, H, on, T, with, W} is that it's  very
%% close to ...     {C, calc, D, to, T, with, W}.
%% The first is calc hit, the second is calc damage.
%% Granted, when spelled out with "Hit" and "Damage" it's more obvious.

-export([update_attack/3]).
-export([add_value/2]).

-include("include/erlmud.hrl").

add_value(Attack = #attack{calc_type = hit, hit = Hit}, Value) ->
    Attack#attack{hit = Hit + Value};
add_value(Attack = #attack{calc_type = damage, damage = Damage}, Value) ->
    Attack#attack{damage = Damage + Value}.

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
