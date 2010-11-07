%%%-------------------------------------------------------------------
%%% File          : nbt.erl
%%% Author        : Micah Warren
%%% Organization  : Myself
%%% Project       : NBTErl
%%% Description   : 
%%%
%%% Created       :  10/29/10
%%%-------------------------------------------------------------------
-module(nbt).
-author(micahw).

-define(TAG_End, 0).
-define(TAG_Byte, 1).
-define(TAG_Short, 2).
-define(TAG_Int, 3).
-define(TAG_Long, 4).
-define(TAG_Float, 5).
-define(TAG_Double, 6).
-define(TAG_Byte_Array, 7).
-define(TAG_String, 8).
-define(TAG_List, 9).
-define(TAG_Compound, 10).

-export([
	decode/1,
	encode/1,
	get_value/2,
	set_value/3
]).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

get_value(Struct, Path) when not is_list(Path) ->
	get_value(Struct, [Path]);
get_value(Val, []) ->
	Val;
get_value({_Name, {compound, Props}}, [Prop | Tail]) when is_binary(Prop) ->
	Next = proplists:get_value(Prop, Props),
	get_value(Next, Tail);
get_value({_Name, {Type, Items}}, [Prop | Tail]) when is_list(Items), is_integer(Prop) ->
	Next = lists:nth(Prop, Items),
	get_value({Type, Next}, Tail);
get_value({compound, Props}, [Prop | Tail]) when is_binary(Prop) ->
	Next = proplists:get_value(Prop, Props),
	get_value(Next, Tail);
get_value({Type, Items}, [Prop | Tail]) when is_list(Items), is_integer(Prop) ->
	Next = lists:nth(Prop, Items),
	get_value({Type, Next}, Tail).

set_value(Struct, Path, Value) when not is_list(Path) ->
	set_value(Struct, [Path], Value);
set_value({Name, {compound, Props}}, [Path], Value) when is_binary(Path) ->
	NewVal = set_value({compound, Props}, [Path], Value),
	{Name, NewVal};
set_value({Name, {Type, Items}}, [Prop], Value) when is_list(Items), is_integer(Prop) ->
	NewVal = set_value({Type, Items}, [Prop], Value),
	{Name, NewVal};
set_value({compound, Props}, [Prop], Value) when is_binary(Prop) ->
	 MidProps = proplists:delete(Prop, Props),
	{compound, [{Prop, Value} | MidProps]};
set_value({Type, Items}, [Prop], Value) when is_list(Items), is_integer(Prop) ->
	{Head, [_Ditch | Tail]} = lists:split(Prop, Items),
	{Type, lists:append(Head, [Value | Tail])};
set_value({Name, {compound, Props}}, [Prop | PathTail], Value) when is_binary(Prop) ->
	NewVal = set_value({compound, Props}, [Prop | PathTail], Value),
	{Name, NewVal};
set_value({Name, {Type, Items}}, [Nth | PathTail], Value) when is_list(Items), is_integer(Nth) ->
	NewVal = set_value({Type, Items}, [Nth | PathTail], Value),
	{Name, NewVal};
set_value({compound, Props}, [Prop | Tail], Value) when is_binary(Prop) ->
	NewValue = set_value(proplists:get_value(Prop, Props), Tail, Value),
	MidProps = proplists:delete(Prop, Props),
	{compound, [{Prop, NewValue} | MidProps]};
set_value({Type, Items}, [Nth | PathTail], Value) when is_list(Items), is_integer(Nth) ->
	NewValue = set_value(lists:nth(Nth, Items), PathTail, Value),
	{Head, [_Ditch | Tail]} = lists:split(Nth, Items),
	{Type, lists:append(Head, [NewValue | Tail])}.


decode(<<?TAG_Compound, 0, NameLen, Rest/binary>>) ->
	<<Name:NameLen/binary, MoreRest/binary>> = Rest,
	List = decode_compound(MoreRest, []),
	{Name, {compound, List}}.

decode_compound(<<?TAG_End>>, Acc) ->
	lists:reverse(Acc);
decode_compound(<<?TAG_End, Rest/binary>>, Acc) ->
	{Rest, lists:reverse(Acc)};
decode_compound(Bin, Acc) ->
	{Item, MoreBin} = decode_item(Bin),
	decode_compound(MoreBin, [Item | Acc]).

decode_item(<<Tag, Rest/binary>>) ->
	{Name, Payload} = get_name(Rest),
	{DecodedPayload, Bin} = decode_payload(Tag, Payload),
	{{Name, DecodedPayload}, Bin}.

decode_payload(?TAG_Byte, <<Byte:8/signed, Rest/binary>>) ->
	{{byte, Byte}, Rest};
decode_payload(?TAG_Short, <<Short:16/signed, Rest/binary>>) ->
	{{short, Short}, Rest};
decode_payload(?TAG_Int, <<Int:32/signed, Rest/binary>>) ->
	{{int, Int}, Rest};
decode_payload(?TAG_Long, <<Long:64/signed, Rest/binary>>) ->
	{{long, Long}, Rest};
decode_payload(?TAG_Float, <<Float:32/signed-float, Rest/binary>>) ->
	{{float, Float}, Rest};
decode_payload(?TAG_Double, <<Double:64/signed-float, Rest/binary>>) ->
	{{double, Double}, Rest};
decode_payload(?TAG_Byte_Array, <<Length:32/signed, MidRest/binary>>) ->
	<<Bin:Length/binary, Rest/binary>> = MidRest,
	{{byte_array, Bin}, Rest};
decode_payload(?TAG_String, <<Length:16, MidRest/binary>>) ->
	<<Bin:Length/binary, Rest/binary>> = MidRest,
	{{string, Bin}, Rest};
decode_payload(?TAG_List, <<Type, Length:32, MidRest/binary>>) ->
	decode_list(MidRest, Type, Length);
decode_payload(?TAG_Compound, Bin) ->
	{Rest, List} = decode_compound(Bin, []),
	{{compound, List}, Rest}.

decode_list(Bin, Tag, Length) when Length >= 0->
	decode_list(Bin, Tag, Length, []).

decode_list(Rest, Tag, Length, Acc) when length(Acc) =:= Length ->
	{{tag_to_atom(Tag), lists:reverse(Acc)}, Rest};
decode_list(Payload, Tag, Length, Acc) ->
	{{_Type, DecodedPayload}, Rest} = decode_payload(Tag, Payload),
	decode_list(Rest, Tag, Length, [DecodedPayload | Acc]).

encode({_Name, {compound, _List}} = Item) ->
	encode_item(Item).
	
encode_item({Name, {Type, Payload}}) ->
	NameBin = case is_list(Payload) of
		true ->
			case {Type, Payload} of
				{compound, [T | _]} when is_tuple(T) ->
					encode_head(Type, Name);
				_ ->
					encode_head(list, Name)
			end;
		false ->
			encode_head(Type, Name)
	end,
	PayloadBin = encode_payload(Type, Payload),
	<<NameBin/binary, PayloadBin/binary>>.

encode_head(Type, Name) ->
	<<(atom_to_tag(Type)):8, 0, (byte_size(Name)), Name/binary>>.

encode_payload(compound, [T | _] = Payload) when is_list(Payload), is_tuple(T) ->
	Bin = encode_compound(Payload),
	<<Bin/binary, ?TAG_End>>;
encode_payload(Type, Payload) when is_list(Payload) ->
	Count = length(Payload),
	BinItems = encode_list(Type, Payload),
	<<(atom_to_tag(Type)), Count:32, BinItems/binary>>;
encode_payload(byte, Payload) ->
	<<Payload:8>>;
encode_payload(short, Payload) ->
	<<Payload:16>>;
encode_payload(int, Payload) ->
	<<Payload:32>>;
encode_payload(long, Payload) ->
	<<Payload:64>>;
encode_payload(float, Payload) ->
	<<Payload:32/float>>;
encode_payload(double, Payload) ->
	<<Payload/float>>;
encode_payload(byte_array, Payload) ->
	<<(byte_size(Payload)):32, Payload/binary>>;
encode_payload(string, Payload) ->
	<<(byte_size(Payload)):16, Payload/binary>>.
encode_list(Type, Items) ->
	encode_list(Type, Items, <<>>).

encode_list(_Type, [], Acc) ->
	Acc;
encode_list(Type, [Head | Tail], Acc) ->
	Bin = encode_payload(Type, Head),
	encode_list(Type, Tail, <<Acc/binary, Bin/binary>>).

encode_compound(Items) ->
	encode_compound(Items, <<>>).

encode_compound([], Acc) ->
	Acc;
encode_compound([Head | Tail], Acc) ->
	Bin = encode_item(Head),
	encode_compound(Tail, <<Acc/binary, Bin/binary>>).

get_name(<<0, NameLen, Rest/binary>>) ->
	<<Name:NameLen/binary, MoreRest/binary>> = Rest,
	{Name, MoreRest}.

tag_to_atom(Tag) ->
	case Tag of
		0 -> 'end';
		1 -> 'byte';
		2 -> 'short';
		3 -> 'int';
		4 -> 'long';
		5 -> 'float';
		6 -> 'double';
		7 -> 'byte_array';
		8 -> 'string';
		9 -> 'list';
		10 -> 'compound'
	end.

atom_to_tag(Atom) -> 
	case Atom of
		'end' -> ?TAG_End;
		'byte' -> ?TAG_Byte;
		'short' -> ?TAG_Short;
		'int' -> ?TAG_Int;
		'long' -> ?TAG_Long;
		'float' -> ?TAG_Float;
		'double' -> ?TAG_Double;
		'byte_array' -> ?TAG_Byte_Array;
		'string' -> ?TAG_String;
		'list' -> ?TAG_List;
		'compound' -> ?TAG_Compound
	end.
			
-ifdef(TEST).
decode_test_() ->
	[{"An empty compound",
	fun() ->
		In = <<10, 0, 11, "hello world", ?TAG_End>>,
		Expected = {<<"hello world">>, {compound, []}},
		?assertEqual(Expected, decode(In))
	end},
	{"One byte",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_Byte, 0, 4, "byte", -1:8, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"byte">>, {byte, -1}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a short",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_Short, 0, 5, "short", -5:16, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"short">>, {short, -5}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"an int",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_Int, 0, 3, "int", -137:32, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"int">>, {int, -137}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a long",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_Long, 0, 4, "long", -785:64, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"long">>, {long, -785}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a float",
	fun() ->
		% erlang is dumb in that it reaaaaally wants to fill out all 64 bits.
		In = <<10, 0, 8, "compound", ?TAG_Float, 0, 5, "float", -27.345:32/float, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"float">>, {float, -27.344999313354492}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a double",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_Double, 0, 6, "double", -345.772/float, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"double">>, {double, -345.772}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a byte array",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_Byte_Array, 0, 10, "byte array", 6:32, "abcdef", ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"byte array">>, {byte_array, <<"abcdef">>}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a string",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_String, 0, 6, "string", 11:16, "hello world", ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"string">>, {string, <<"hello world">>}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"an empty tag list",
	fun() ->
		In = <<10, 0, 8, "compound", ?TAG_List, 0, 4, "list", 1, 0:32, ?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"list">>, {byte, []}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"a tag list with 2 strings",
	fun() ->
		In = <<10, 0, 8, "compound", 
			?TAG_List, 0, 4, "list", ?TAG_String, 2:32, 
				12:16, "first string",
				13:16, "second string",
		?TAG_End>>,
		Expected = {<<"compound">>, {compound, [{<<"list">>, {string, [<<"first string">>, <<"second string">>]}}]}},
		?assertEqual(Expected, decode(In))
	end},
	{"compound in a compound",
	fun() ->
		In = <<10, 0, 8, "compound",
			?TAG_Compound, 0, 14, "inner compound", 
				?TAG_String, 0, 7, "string1", 12:16, "first string",
				?TAG_String, 0, 7, "string2", 13:16, "second string",
			?TAG_End,
		?TAG_End>>,
		Expected = {<<"compound">>, {compound, [
			{<<"inner compound">>, {compound, [
				{<<"string1">>, {string, <<"first string">>}},
				{<<"string2">>, {string, <<"second string">>}}
			]}}
		]}},
		?assertEqual(Expected, decode(In))
	end}].

encode_test_() ->
	[{"encoding a byte",
	fun() ->
		In = {<<"byte">>, {byte, 3}},
		Expected = <<?TAG_Byte, 0, 4, "byte", 3:8>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"encoding a short",
	fun() ->
		In = {<<"short">>, {short, -97}},
		Expected = <<?TAG_Short, 0, 5, "short", -97:16>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"an int",
	fun() ->
		In = {<<"integer">>, {int, 5782}},
		Expected = <<?TAG_Int, 0, 7, "integer", 5782:32>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"a long",
	fun() ->
		In = {<<"long">>, {long, 28335628}},
		Expected = <<?TAG_Long, 0, 4, "long", 28335628:64>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"a float",
	fun() ->
		In = {<<"float">>, {float, -27.34}},
		Expected = <<?TAG_Float, 0, 5, "float", -27.34:32/float>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"a double",
	fun() ->
		In = {<<"double">>, {double, -744323432.54883}},
		Expected = <<?TAG_Double, 0, 6, "double", -744323432.54883/float>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"a byte array",
	fun() ->
		In = {<<"byte array">>, {byte_array, <<"this is a binary">>}},
		Expected = <<?TAG_Byte_Array, 0, 10, "byte array", 16:32, "this is a binary">>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"a string",
	fun() ->
		In = {<<"string">>, {string, <<"goober pants">>}},
		Expected = <<?TAG_String, 0, 6, "string", 12:16, "goober pants">>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"an empty list",
	fun() ->
		In = {<<"list">>, {byte, []}},
		Expected = <<?TAG_List, 0, 4, "list", ?TAG_Byte, 0:32>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"a list with 2 strings",
	fun() ->
		In = {<<"list">>, {string, [<<"hi there!">>, <<"bye there!">>]}},
		Expected = <<?TAG_List, 0, 4, "list", ?TAG_String, 2:32, 9:16, "hi there!", 10:16, "bye there!">>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"simple compound",
	fun() ->
		In = {<<"compound">>, {compound, [{<<"string">>, {string, <<"string text">>}}, {<<"integer">>, {int, 3}}]}},
		Expected = <<?TAG_Compound, 0, 8, "compound", ?TAG_String, 0, 6, "string", 11:16, "string text", ?TAG_Int, 0, 7, "integer", 3:32, ?TAG_End>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"nested compound",
	fun() ->
		In = {<<"outer">>, {compound, [{<<"inner">>, {compound, [{<<"string">>, {string, <<"goober">>}}]}}]}},
		Expected = <<?TAG_Compound, 0, 5, "outer", ?TAG_Compound, 0, 5, "inner", ?TAG_String, 0, 6, "string", 6:16, "goober", ?TAG_End, ?TAG_End>>,
		?assertEqual(Expected, encode_item(In))
	end},
	{"compound in a list",
	fun() ->
		In = {<<"list">>, {compound, [[{<<"id">>, {byte, 1}}, {<<"name">>, {string, <<"one">>}}], [{<<"id">>, {byte, 2}}, {<<"name">>, {string, <<"two">>}}]]}},
		Expected = <<?TAG_List, 0, 4, "list", ?TAG_Compound, 2:32, 
			?TAG_Byte, 0, 2, "id", 1:8, ?TAG_String, 0, 4, "name", 3:16, "one", ?TAG_End, 
			?TAG_Byte, 0, 2, "id", 2:8, ?TAG_String, 0, 4, "name", 3:16, "two", ?TAG_End>>,
		?assertEqual(Expected, encode_item(In))
	end}].
-endif.