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
	Line = #line{cmd=lists:reverse(Cmd),data=Data},
	fliter_cmd(Line);

parseCmd(Line,Cmd,Data) ->
	[H|T] = Line,
	if
		H =/= $\s ->
			parseCmd(T,[H|Cmd],Data);
		true ->
			parseCmd([],Cmd,Line)
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

get_cmd(Cmd) ->
	if 
		Cmd =:= 'undefined' ->
			'undefined';
		Cmd =:= ?CMD_HELP ->
			{ok,?CMD_HELP};
		Cmd =:= ?CMD_LOGIN ->
			{ok,?CMD_LOGIN};
		true ->
			{none,Cmd}
	end.


fliter_cmd(Line) ->
	Cmd = get_cmd(Line#line.cmd),
	case Cmd of
		'undefined' ->
			#line{cmd='undefined',data=Line#line.data};
		{ok,C} ->
			NewLine = #line{cmd=C,data=Line#line.data},
			check_cmd_format(NewLine);
		_ ->
			#line{cmd='undefined',data=("-"++Line#line.cmd++Line#line.data)}
	end.

check_cmd_format(Line) ->
	if
		Line#line.cmd =:= 'undefined' ->
			Line;
		true ->
			if
				Line#line.cmd =:= ?CMD_HELP ->
					format_0(Line);
				Line#line.cmd =:= ?CMD_LOGIN ->
					format_1(Line)
			end
	end.

format_0(Line) ->
	if
		length(Line#line.data) =/= 0 ->
			#line{cmd=Line#line.cmd,data=Line#line.data,state=?CODE_CMD_FORMAT_ERROR};
		true ->
			Line
	end.

format_1(Line) ->
	Tokens = string:tokens(Line," "),
	if
		length(Tokens) =/= 1 ->
			#line{cmd=Line#line.cmd,data=Line#line.data,state=?CODE_CMD_FORMAT_ERROR};
		true ->
			#line{cmd=Line#line.cmd,data=Tokens}
	end.

format_2(Line) ->
	Tokens = string:tokens(Line#line.data," "),
	if
		length(Tokens) =/= 2 ->
			#line{cmd=Line#line.cmd,data=Line#line.data,state=?CODE_CMD_FORMAT_ERROR};
		true ->
			#line{cmd=Line#line.cmd,data=Tokens}
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

fliter_cmd_with_cmd_test() ->
	?assertEqual(#line{cmd="login",data="   jias"},parseLine(" -login   jias",[],[])).
fliter_cmd_with_undefined_cmd_test() ->
	?assertEqual(#line{cmd='undefined',data="-loginlogin   jias"},parseLine(" -loginlogin   jias",[],[])).
fliter_cmd_with_cmd_flag_test() ->
	?assertEqual(#line{cmd='undefined',data="- "},parseLine(" - ",[],[])).
format_cmd_ok_test() ->
	?assertEqual(#line{cmd='login',data="jias"},parseLine("-login jias",[],[])).
-endif.
