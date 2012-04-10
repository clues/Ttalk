%% Author: Jias Chao
%% Created: 2011-1-18
%% Description: TODO: Add description to unit
-module(ttalk_unit).
-include("../include/server.hrl").
-compile(export_all).
	

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
	?assertEqual(2,2).

-endif.