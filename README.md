EGRE_MUD_1 - Reference implementation of a MUD using EGRE_MUD

EGRE_MUD_1 is one of several parts being split out of GERLSHMUD.

```mermaid
flowchart TD
    em1["egre_mud_1"] --> em["egre_mud"]
    etd["egre_tower_defence"] --> ea["egre_arcade"]
    em & ea --> e["egre"]
```

The goal of EGRE_MUD_1 is to provide the rule processes for a MUD that
will be loaded by the EGRE_MUD engine into EGRE, the Erlang Graph Rules
Engine.

I'm not exactly sure yet how I'm going to do that.
