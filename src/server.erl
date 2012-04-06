
-module(server).
-export([run/0,stop/0]).
-include("../include/server.hrl").

run() ->
	io:format("Go init [smplog]~n"),
	smplog:run(),
	
	smplog:info("GO init [ets_db]"),
	ets_db:init(),
	
	smplog:info("GO init [sokt_server]"),
	sokt_server:init(),
	
	smplog:info("GO registe server and start loop..."),
	process_flag(trap_exit, true),
	register(server,self()),
	loop().

stop() ->
	halt().

loop() ->
	receive
		{'EXIT',Pid,Rea} ->
			smplog:info("Receive exit info from: ~p by: ~p",[Pid,Rea]);
		{request,closed} ->
			{}
	end.