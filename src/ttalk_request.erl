%% Author: wave
%% Created: 2012-4-6
%% Description:ttalk_request,this module The main purpose of packt
%% original information from client
-module(ttalk_request).
-export([getLine/1]).
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

-spec getLine(RawLine::string()) ->term().
getLine(RawLine) ->
	parseLine(RawLine,[],[]).

parseLine([],Cmd,Data) ->
	#line{};
parseLine(RawLine,Cmd,Data) ->
	RawLine1 = removeHeadSpace(RawLine),
	[H|T] = RawLine1,
	if
		H =:= [] ->
			#line{};
		H =:= $- ->
			parseCmd(T,[],[]);
		true ->
			#line{data=RawLine}
	end.

get_cmd(Cmd) ->
	if 
		Cmd =:= 'undefined' ->
			'undefined';
		Cmd =:= ?CMD_HELP ->
			{ok,?CMD_HELP};
		Cmd =:= ?CMD_LOGIN ->
			{ok,?CMD_LOGIN};
		Cmd =:= ?CMD_HISTORY ->
			{ok,?CMD_HISTORY};
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
					format(Line,0);
				Line#line.cmd =:= ?CMD_LOGIN ->
					format(Line,1);
				Line#line.cmd =:= ?CMD_HISTORY ->
					format(Line,2)
			end
	end.


format(Line,ArgsNum) ->
	Tokens = string:tokens(Line#line.data," "),
	if
		length(Tokens) =/= ArgsNum ->
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
	?assertEqual(#line{},getLine("")).

parseLine_without_cmd_test() ->
	?assertEqual(#line{data="  hello,world"},getLine("  hello,world")).

parseLine_with_cmd_test() ->
	?assertEqual(#line{cmd="login",data=["jias"]},getLine(" -login   jias")).

fliter_cmd_with_undefined_cmd_test() ->
	?assertEqual(#line{cmd='undefined',data="-loginlogin   jias"},getLine(" -loginlogin   jias")).
fliter_cmd_with_cmd_flag_test() ->
	?assertEqual(#line{cmd='undefined',data="- "},getLine(" - ")).
format_cmd_0_ok_test() ->
	?assertEqual(#line{cmd="help",data=[]},getLine(" -help ")).
format_cmd_1_ok_test() ->
	?assertEqual(#line{cmd="login",data=["jias"]},getLine("-login jias")).
format_cmd_2_ok_test() ->
	?assertEqual(#line{cmd="history",data=["2","3"]},getLine("-history 2 3")).
format_cmd_state_error_test() ->
	?assertEqual(#line{cmd="login",data=" jias chao",state=?CODE_CMD_FORMAT_ERROR},getLine("-login jias chao")).

-endif.
