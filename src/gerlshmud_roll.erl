%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_roll).

-export([roll/1]).

roll(Roll) when is_integer(Roll) ->
    roll({Roll, 0});
roll({0 = _Roll, 0 = _Base}) ->
  0;
roll({0 = _Roll, Base}) ->
  Base;
roll({Roll, Base}) ->
    rand:uniform(Roll) + Base.
