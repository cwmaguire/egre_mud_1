%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_attack).

-callback should_attack(tuple()) -> boolean().
