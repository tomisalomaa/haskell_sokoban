# Sokoban
-------

## Rules

You are the warehouse man! Move crates to storages by pushing them around. Try not to get stuck.

## Instructions

### Start game

The game initializes by executing the game.exe.
Exe can be found from the sokoban folder.

### Controls

The game uses WASD control scheme to move the player worker around the level:

w -- Up  
a -- Left  
s -- Down  
d -- Right  

NOTE:
Due to a bug affecting the game when played on Windows os, each input needs to be "confirmed" by entering the line in cmd console.
Linux os should not suffer from such a bug and each input characted should be read as soon as struck on the console.

### Legend

@&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Player
o&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Crate
#&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Wall
<space>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Floor / open space
.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Storage
*&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;A stored crate / crate inside a storage
+&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Player on a storage

### State of the game / additional info

The current version of the game only includes one hard coded level.
The game ends as soon as all crates have been stored. If you find yourself stuck, not being able to complete the level,
there is currently no functionality available to restart the level --> close and start the game again if you wish to replay.