%% Author: Jias Chao
%% Created: 2010-12-1
%% Description: TODO: Add description to response
-module(response,[Socket,Header]).
-compile(export_all).
-include("../include/server.hrl").


get(header) ->
	Header;

get(socket) ->
	Socket.

ok() ->
	ok(<<>>).
ok(BodyBin) ->
	Bin = packet(Header,BodyBin,?OK_STATE),
	re(Bin).

error() ->
	error(<<>>).

error(BodyBin) ->
	Bin = packet(Header,BodyBin,?ERROR_STATE),
	re(Bin).
	
re(Bin) ->
	case gen_tcp:send(Socket, Bin) of
		ok ->
			ok;
		{error,Reason} ->
			smplog:error("Send to socket:~p have error by ~p",[Socket,Reason]),
			{error,Reason}
	end.
			  
send(Socket,Body) ->
	case gen_tcp:send(Socket, Body) of
		ok ->
			ok;
		{error,Reason} ->
			smplog:error("Send to socket:~p have error by ~p",[Socket,Reason]),
			{error,Reason}
	end.

packet(Header,Body,State) ->
	Hbin = packet_header(Header,size(Body),State),
	list_to_binary([Hbin|Body]).	

packet_header(Header,BodySize,State) ->
	Cmd = Header#header.cmd,
	State = State,
	Sender = Header#header.receiver,
	Receiver = Header#header.sender,
	<<Cmd:8,State:8,BodySize:32,Sender:128,Receiver:128>>.


