%% Author: wave
%% Created: 2012-4-6
%% Description: TODO: Add description to test
-module(test).
-export([test/1]).

test(Size) ->
	{ok,FD} = file:open("test", [write,binary,append]),
	write(Size,FD).


write(0,FD) ->
	file:close(FD);

write(M,FD) ->
	Mbin = binary:copy(<<"0123456789abcdef">>,1024*64),
	file:write(FD, Mbin),
	write(M-1,FD).
	
	