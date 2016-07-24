-record(attack, {source :: pid(),
                 target :: pid(),
                 attack :: pid(),
                 weapon :: pid(),
                 attack_types = [] :: [atom()],
                 hit :: integer(),
                 damage :: integer(),
                 wait :: integer(),
                 calc_type :: hit | damage | weight}).
