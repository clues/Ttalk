%% Author: Jias Chao
%% Created: 2011-1-18
%% Description: TODO: Add description to unit
-module(unit).
-include("../include/server.hrl").
-compile(export_all).

split(Bin, Chars) ->
    split(Chars, Bin, 0, []).

split(Chars, Bin, Idx, Acc) ->
    case Bin of
    	<<This:Idx/binary, Char, Tail/binary>> ->
    		case lists:member(Char, Chars) of
    			false ->
    				split(Chars, Bin, Idx+1, Acc);
    			true ->
    				split(Chars, Tail, 0, [This|Acc])
    		end;
    	<<This:Idx/binary>> ->
    		lists:reverse(Acc, [This])
    end.
	
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
test12() ->
	?_assertEqual(1,2),
    ok.
