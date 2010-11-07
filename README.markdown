Overview
========

Made because I was on my 4th mine, and was still nowhere near having enough
materials to make tracks across the world.  That and I was bored.  So I 
picked up Erlang for it's bit syntax, read through Notch's description of
the [NBT Format](http://www.minecraft.net/docs/NBT.txt), and made a 
set of cheats.

So...What do I do with this?
============================

Once you've got the repo, use the provided rebar to compile it.  Rebar will
complain, but that's because this isn't an application proper, just a 
couple of modules.

**Do not do this while Minecraft is running!**  You will likely corrupt your
level.dat file, thus losing the hundreds of hours put into building that 
awesome obsidian castle.

Launch erl, making sure to have the nbt and cheatcraft modules available.
In the shell, the fastest and usualy best way to cheat:

	cheatcraft:do("path/to/level.dat", [fix_all, stack_blocks]).

The above will fix all items (pick axes, armor, etc...) and stack any 
blocks you have in your inventory to 64 (like diamond, iron, iron igots).

Once you feel you've cheated enough:

	q().

And then open Mincraft, and marvel at the number of sticks you have because
you didn't remove them from your inventory!

Okay, what else?
================

Load an level.dat (or any nbt encoded) file into a decompressed nbt 
structure:

	cheatcraft:read_file("path/to/level.dat").

Write an nbt structure to a file:

	cheatcraft:write_file("path/to/level.dat").

Get a value from the nbt structure:

	nbt:get_value(Nbtstruct, [<<"names_or_list_positions">>]).

Set a Value:

	nbt:set_value(NBTStruct, [<<"names_or_list_postions">>], Value).

Stack everything in your invtory (in case you want 64 pickaxes):

	cheatcraft:do("path/to/level.dat", [stack_all]).
