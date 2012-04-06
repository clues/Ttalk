%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to test
-module(test).
-export([all/0,all/1,t/1,t/2]).

all() ->
	all(file:get_cwd()).
all(Project) ->
%% 	if 
%% 		is_list(Project) ->
%% 			erlang:cd(Project);
%% 		true ->
%% 			erlang:cd(atom_to_list(Project))
%% 	end,
	{ok,FileNames} = file:list_dir("."),
	Mods = getModeNames(FileNames,[]),
	[t(Mod) || Mod <- Mods].

t(Mod) ->
	TestFunctions = getAllTestFunction(Mod),
	[t(Mod,Fun) || Fun <- TestFunctions].

t(Mod,Fun) ->
	Mod:Fun().

getAllTestFunction(Mod) ->
	io:format("pwd: ~p",[erlang:pwd()]),
	Functions = Mod:module_info(functions),
	[X || {X,ArgsNum} <- Functions,ArgsNum=:=0,
		  lists:sublist(atom_to_list(X),4) =:= "test"].

getModeNames([],L)->
	L;
getModeNames([H|T],L) ->
	FnStr = atom_to_list(H),
	IsBeam = lists:suffix(".beam", FnStr),
	if 
		 IsBeam ->
			Mod = lists:sublist(FnStr,0, (length(FnStr)-5)),
			getModeNames(T,[Mod|L]);
		 true ->
			getModeNames(T,[L])
	end.