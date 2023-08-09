%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(mud_defence).

-callback should_defend(tuple()) -> boolean().

