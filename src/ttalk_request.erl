%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to ttalk_request
-module(ttalk_request).
-export([]).
-include("ttalk_cmd.hrl").

removeHeadSpace([]) ->
	[];
removeHeadSpace([H|T]) ->
	if
		H =:= $\s ->
			removeHeadSpace(T);
		true ->
			[H|T]
	end.

parseCmd([],Cmd,Data) ->
	#line{cmd=lists:reverse(Cmd),data=Data};

parseCmd(Line,Cmd,Data) ->
	[H|T] = Line,
	if
		H =/= $\s ->
			parseLine(T,[H|Cmd],Data);
		true ->
			parseCmd([],Cmd,T)
	end.

parseLine([],Cmd,Data) ->
	#line{};
parseLine(Line,Cmd,Data) ->
	Line1 = removeHeadSpace(Line),
	[H|T] = Line1,
	if
		H =:= [] ->
			#line{};
		H =:= $- ->
			parseCmd(T,[],[]);
		true ->
			#line{data=Line}
	end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
removeHeadSpace_null_test() ->
	?assertEqual([],removeHeadSpace("")).

removeHeadSpace_test() ->
	?assertEqual("-help",removeHeadSpace("  -help")),
	?assertEqual("-login abc",removeHeadSpace("  -login abc")).

parseLine_null_test() ->
	?assertEqual(#line{},parseLine("",[],[])).

parseLine_without_cmd_test() ->
	?assertEqual(#line{data="  hello,world"},parseLine("  hello,world",[],[])).

parseLine_with_cmd_test() ->
	?assertEqual(#line{cmd="login",data="   jias"},parseLine(" -login   jias",[],[])).
-endif.
