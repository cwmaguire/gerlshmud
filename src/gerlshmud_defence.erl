%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_defence).

-callback should_defend(tuple()) -> boolean().

