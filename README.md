EGRE_MUD_1 - Reference implementation of a MUD using EGRE_MUD

EGRE_MUD_1 is one of several parts that have been split out of GERLSHMUD:
- egre
- egre_mud
- egre_mud_1

```mermaid
flowchart TD
    em1["egre_mud_1"] --> em["egre_mud"]
    etd["egre_tower_defence"] --> ea["egre_arcade"]
    em & ea --> e["egre"]
```
### Purpose
This is a proof of concept which might eventually turn into a MUD.
I wondered what would happen if you built a MUD with no "manager", and had everything
in the MUD fend for itself by responding to events in the world.
The entire MUD world is networked together in a graph, and every (local) object sees every event in the world.
Only one object (an object being anything with game logic) ever sees an event at one time, so events
can be ignored, modified, resent, or even just killed.

### Installation
Uh, we haven't really gotten that far yet. You can run the tests with ./t, or ./t name_of_test.
To date, we're just getting tests to work as we get EGRE, EGRE_MUD and EGRE_MUD_1 working in their separate projects.

### Running
Right now there's a *tiny* bit of code to run the MUD, but mostly we're just focused on getting commands working in the tests.
Since most of the tests are passing, the MUD should run.

### Tests
As of 2024-10-20, all but one tests are passing.

### Logging
There is a web page that shows up at logs/egre_mud_1_log.html and lists out all the events in a somewhat graphical form.
There is also DB logging to Postgres.

### Twitch

As of 2024-11-12 we're coding live on EGRE_MUD_1 and EGRE from 5-7 pm Pacific, Monday to Saturday.
https://www.twitch.tv/lulu58e2

### YouTube updates:

---
https://youtu.be/ryaasVMpiS0 - Web logging updates

### Progress

##### Done
[x] move
[x] look
[x] say
[x] shout (1 room away)
[x] emote ("emote farts" -> "Reginald farts")
[x] get
[x] put
[x] attack
[x] heal spell
[x] natural healing with attribute
[x] die
[x] loot / search
[x] wield
[x] cast spell
[x] activate quest
[x] complete quest
[x] turn in quest
[x] parse cast spell

##### TODO
[ ] buy / sell
[ ] resurrect
[ ] build the MUD from within the MUD (rooms, items, etc.)
[ ] roles (e.g. admin rights)
[ ] inventory
[ ] lists quests
[ ] list quest history
[ ] parse attack

##### Nice to Have:
[ ] custom TTL
[ ] buffs
[ ] debuffs
[ ] max hitpoints
