%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to ttalk_request
-module(ttalk_request).
-export([]).

removeHeadSpace(Bin) ->
	<<H|Rest>> = Bin,
	if
		H =:= 32 ->
			removeHeadSpace(Bin);
		true ->
			<<H|Bin>>
	end.



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
	?assertEqual(2,2).

removeHeadSpace_test() ->
	?assertEqual(<<"-help">>,removeHeadSpace(<<"  -help">>)).

-endif.