
-module(sokt_server).
-compile(export_all).

-include("../include/server.hrl").
-define(LINK_MAX,2048).
-define(PORT,?PORT_DEFAULT).

-record(sock,{port,max=?LINK_MAX,model=[]}).

init() ->
	ets_db:new_tab(onliner, #onliner{}),
	ets_db:new_tab(?SOCK_TAB, #sock{}),
	case gen_tcp:listen(?PORT_DEFAULT,?OPTIONS_TCP) of
		{ok,Lsocket} ->
			ets_db:add(?SOCK_TAB, #sock{port=?PORT_DEFAULT,model=?OPTIONS_TCP}),
			smplog:info("Listen on port [~p] ok",[?PORT_DEFAULT]),
			spawn(fun() -> process_flag(trap_exit,true),do_accept(Lsocket) end),
			smplog:info("Set socket is system process already"),
			{ok,Lsocket};
		{error,Reason} ->
			smplog:error("Socket listen on port [~p] failed!!! by: ~p",[?PORT_DEFAULT,Reason]),
			ets_db:del(?SOCK_TAB),
			{error,Reason}
	end.

do_accept(Lsocket) ->
	So = hd(ets_db:find(?SOCK_TAB, ?PORT_DEFAULT)),
	if
		So#sock.max =:= 0 ->
			smplog:warn("sockt server arrive max links:~p and refuse next request",[?LINK_MAX]),
			receive
				_ ->
					do_accept(Lsocket)
			after 20000 ->
					do_accept(Lsocket)
			end;
		true ->
			case gen_tcp:accept(Lsocket) of
				{ok,Socket} ->
					smplog:info("Receive a new connection from IP[~p] Port[~p]",[get_ip(Socket),get_port(Socket)]),
					smplog:debug("Create hander process for this sockt connect"),
					Pid_h = spawn_link(hander,loop,[Socket]),
					smplog:debug("Create request process for this socket connect"),
					spawn(fun() -> loop_hander(Socket,Pid_h) end),
					ets_db:update(?SOCK_TAB, #sock{port=So#sock.port,max=So#sock.max-1,model=So#sock.model}),
					do_accept(Lsocket);
				{error,Reason} ->
					smplog:error("Occurent an exception when recv a new connection by: ~p",[Reason]),
					do_accept(Lsocket)
			end
	end.

loop_hander(Socket,Pid_h) ->	
	case gen_tcp:recv(Socket, 0) of
		{ok,Data} ->
			smplog:debug("OOP:~P",[Data]),
			Pid_h ! {request,Data},	
			inet:setopts(Socket,?OPTIONS_TCP),
			loop_hander(Socket,Pid_h);
		{error,closed} ->
			gen_tcp:close(Socket),
			So = hd(ets_db:find(?SOCK_TAB, ?PORT_DEFAULT)),
			ets_db:update(?SOCK_TAB, #sock{port=So#sock.port,max=So#sock.max + 1,model=So#sock.model});			
		{error,Reason} ->
			smplog:warn("Receive data have error by: ~p",[Reason]),
			loop_hander(Socket,Pid_h)
	end.

response(Socket,Bin) ->
	case gen_tcp:send(Socket, Bin) of
		ok ->
			done;
		{error,Reason} ->
			smplog:error("Socket occuenter exception: ~p",[Reason])
	end.


get_ip(Socket) ->
	{ok,{Ip,_Port}} = inet:peername(Socket),
	Ip.

get_port(Socket) ->
	{ok,{{_,_,_,_},_Port}} = inet:peername(Socket),
	_Port.