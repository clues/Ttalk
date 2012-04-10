%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to ttalk_request
-module(ttalk_request).
-export([]).

removeHeadSpace() ->
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
removeHeadSpace_test() ->
	?assertEqual(<<"-help">>,removeHeadSpace(<<"  -help">>)).
-endif.