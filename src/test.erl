%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to test
-module(test).
-export([all/0,all/1,t/1,t/2]).

all() ->
	all(file:get_cwd()).
all(Project) ->
	{ok,FileNames} = file:list_dir(Project),
	Mods = getModeNames(FileNames,[]),
	[t(Mod) || Mod <- Mods].

t(Mod) ->
	TestFunctions = getAllTestFunction(Mod),
	[t(Mod,Fun) || Fun <- TestFunctions].

t(Mod,Fun) ->
	Mod:Fun().

getAllTestFunction(Mod) ->
	io:format("pwd: ~p~n",[file:get_cwd()]),
	Functions = Mod:module_info(functions),
	[X || {X,ArgsNum} <- Functions,ArgsNum=:=0,
		  lists:sublist(atom_to_list(X),4) =:= "test"].

getModeNames([],L)->
	L;
getModeNames([H|T],L) ->
	FnStr = H,
	IsBeam = lists:suffix(".beam", FnStr),
	if 
		 IsBeam ->
			Mod = lists:sublist(FnStr,1, (length(FnStr)-5)),
			getModeNames(T,[list_to_atom(Mod)|L]);
		 true ->
			getModeNames(T,[L])
	end.