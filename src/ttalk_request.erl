%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to ttalk_request
-module(ttalk_request).
-export([]).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tokenize_test() ->
	io:format("abc~n"),
	?_assertEqual(2,3).

tmp_test() ->
	io:format("abc~n"),
	?_assertEqual(2,3).

-endif.