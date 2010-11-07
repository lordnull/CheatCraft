%%%-------------------------------------------------------------------
%%% File          : cheatcraft.erl
%%% Author        : Micah Warren
%%% Organization  : KGB
%%% Project       : NBTErl
%%% Description   : 
%%%
%%% Created       :  10/30/10
%%%-------------------------------------------------------------------
-module(cheatcraft).
-author(null).

-export([
	fix_all/1,
	stack_all/1,
	stack_blocks/1,
	read_file/1,
	write_file/2,
	do/2
]).

-define(inventory, [<<"Data">>, <<"Player">>, <<"Inventory">>]).

fix_all(Res) ->
	{compound, OldInv} = nbt:get_value(Res, ?inventory),
	Fixed = [fix_item(X) || X <- OldInv],
	nbt:set_value(Res, ?inventory, {compound, Fixed}).
	
stack_all(Res) ->
	{compound, OldInv} = nbt:get_value(Res, ?inventory),
	Stacked = [set_existing_key(X, <<"Count">>, 64) || X <- OldInv],
	nbt:set_value(Res, ?inventory, {compound, Stacked}).

stack_blocks(Res) ->
	{compound, OldInv} = nbt:get_value(Res, ?inventory),
	Stacked = [
		case proplists:get_value(<<"id">>, X) of
			{short, N} when N < 255, is_integer(N) ->
				set_existing_key(X, <<"Count">>, 64);
			_ ->
				X
		end || X <- OldInv ],
	nbt:set_value(Res, ?inventory, {compound, Stacked}).

set_existing_key(Proplist, Key, Value) ->
	case proplists:get_value(Key, Proplist) of
		{Type, _OldVal} ->
			Midlist = proplists:delete(Key, Proplist),
			[{Key, {Type, Value}} | Midlist];
		_ ->
			Proplist
	end.

do(File, Tasks) when is_list(File) ->
	Res = read_file(File),
	NewRes = do(Res, Tasks),
	write_file(File, NewRes);
do(Res, []) ->
	Res;
do(Res, [{Task, Args} | Tasks]) ->
	NewRes = apply(?MODULE, Task, [Res | Args]),
	do(NewRes, Tasks);
do(Res, [Task | Tasks]) ->
	do(apply(?MODULE, Task, [Res]), Tasks).

fix_item(Proplist) ->
	set_existing_key(Proplist, <<"Damage">>, 0).
	
read_file(File) ->
	{ok, ZippedBin} = file:read_file(File),
	Bin = zlib:gunzip(ZippedBin),
	nbt:decode(Bin).

write_file(File, Res) ->
	Bin = nbt:encode(Res),
	Zipped = zlib:gzip(Bin),
	file:write_file(File, Zipped).