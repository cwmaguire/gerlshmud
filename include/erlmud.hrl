-record(attack, {source :: pid(),
                 target :: pid(),
                 attack_types = [] :: [atom()],
                 hit :: integer(),
                 damage :: integer(),
                 calc_type :: hit | damage | weight}).
