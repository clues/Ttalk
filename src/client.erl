%% Author: Jias Chao
%% Created: 2010-11-30
%% Description: TODO: Add description to client
-module(client).
-export([run/0]).

run() ->
	Pid = spawn(fun() -> loop() end),
	Pid ! {loop,"nino"},
	Pid ! {loop,"hao"}.

send(Socket) ->
		Bin = <<101,200,0,0,0,38,0,0,0,0,0,0,0,0,100,100,100,100,101,101,101,101,0,0,0,0,0,0,0,0,102,
			102,102,102,103,103,103,103>>,
		case gen_tcp:send(Socket,Bin) of
			ok ->
				true,
				gen_tcp:close(Socket);
			{error,Reas} ->
				io:format("Send error ~p",[Reas])
		end.


loop() ->
	inner().

inner() ->
	receive 
		{loop,Data} ->
			io:format("Inner ~p~n",[Data]);
		_ ->
			io:format("Inner~n")
	after 50 ->
			true
	end.

%%
%% Local Functions
%%

