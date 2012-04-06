-module(hander).
-export([loop/1,
		 get_header/1,
		 test/0
		 ]).
-include("../include/server.hrl").

loop(Socket) ->
	receive
		{request,Bin} ->
			do_hander(Socket,Bin),
			loop(Socket);
		{resp,Bin} ->
			gen_tcp:send(Socket, Bin);
		_ ->
			loop(Socket)
		after 500 ->
			loop(Socket)
	end.

do_hander(Socket,Bin) ->
	Header = get_header(Bin),
	Re = response:new(Socket,Header),
	smplog:debug("hander Header:~p~n",[Header]),
	case Header#header.cmd of
		?CONNECT_SERVER_CMD ->
			do_conet(Re);
		?REGISTER_CMD ->
			do_registe(Re);
		?LOGIN_CMD ->
			do_login(Re);
		?NORMAL_CMD ->
			do_chat(Re,get_body(Bin))
	end.

do_conet(Re) ->
	Re:ok().

get_header(Bin) ->
	<<Cmd:8,State:8,Size:32,Sender:128,Receiver:128>> = Bin,
	#header{cmd=Cmd,state=State,size=Size,sender=Sender,receiver=Receiver}.

get_body(Bin) ->
	HeadBitLen = ?LENGTH_HEADER *8,
	<<Header:HeadBitLen,Body/binary>> = Bin,
    Body.

do_login(Re) ->
	_Header = Re:get(header),
	Socket = Re:get(socket),
 	Name = _Header#header.sender,
	ets_db:add(onliner, #onliner{name=Name,socket=Socket,state=?ONLINE}),
	Re:ok(),
	flush_user(Re).

do_chat(Re,Body) ->
	_Header = Re:get(header),
	Rver = hd(ets_db:find(onliner, _Header#header.receiver)),
	Socket = Rver#onliner.socket,
	case Re:send(Socket,Body) of
		ok ->
			Re:ok();
		{error,Reason} ->
			Re:error()
	end.


do_registe(_Header) ->
	Sender = _Header#header.sender.

flush_user(Re) ->
	Onliers = ets_db:find(onliner, '_'),
	
	Names = [Name||{onliner,Name,Sock,State} <- Onliers],
	F = fun(Onliner,AccIn) ->[list_to_binary([<<Onliner:128>>])|AccIn] end,
	Body = packet_body(lists:foldr(F, [], Names),","),
	
	Loop_Send = fun(Onliner,AccIn) ->
						Header = #header{cmd=?FLUSH_USERS_CMD,state=?OK_STATE},
						Bin = packet(Header#header{sender=Onliner#onliner.name,receiver=?SERVER_NAME},Body),
						case Re:send(Onliner#onliner.socket,Bin) of
							ok ->
								ok;
							{error,Reason} ->
								ignore
						end
				end,
	lists:foldl(Loop_Send, [], Onliers).
	
	

set_header(Header,BodySize,State) ->
	NewHeader = #header{sender=Header#header.receiver,receiver=Header#header.sender,size=BodySize,state=State}.
	
packet(Header,Body) ->
	Hbin = packet_header(Header,size(Body)),
	list_to_binary([Hbin|Body]).

packet_body(List) ->
	packet_body(List,[],[]).

packet_body(List,JoinChar) ->
	packet_body(List,JoinChar,[]).

packet_body([],JoinChar,List) ->
	list_to_binary(lists:reverse(List));

packet_body(List,JoinChar,RustList) ->
	[H|T] = List,
	NewH = case length(List) of
			   1 ->
				   binary_to_list(H);
			   _ ->
				   binary_to_list(H) ++ JoinChar
		   end,
	packet_body(T,JoinChar,[NewH|RustList]).

packet_header(Header,BodySize) ->
	Cmd = Header#header.cmd,
	State = Header#header.state,
	Sender = Header#header.receiver,
	Receiver = Header#header.sender,
	<<Cmd:8,State:8,BodySize:32,Sender:128,Receiver:128>>.
	
	
	
test() ->
	AllOnliers = [141445691200451659370898004438182526976],
	F = fun(Onliner,AccIn) ->[list_to_binary([<<Onliner:128>>])|AccIn] end,
	List = lists:foldr(F, [], AllOnliers),
	Body = packet_body(List,","),
	Body.
	%mathc_test().

%% === test ==
pack_test() ->
    Header = #header{cmd=23,state=24,size=566,sender=784512,receiver=561234852},
	Body = <<102,35,0,0,0,38,0,0,0,0,0,0,0,0,100,100,100,100,101,101,101,101,0,0,0,0,0,0,0,0,102,
			102,102,102,103,103,103,103>>,
	Bin = packet(Header,Body),
	if
		size(Bin) =:= 76 ->
			io:format("pack ok");
		true ->
			io:format("pack failed")
	end.

mathc_test() ->
	ets_db:init(),
	ets_db:new_tab(onl, #onliner{}),
	ets_db:add(onl, #onliner{name=23,socket=12,state=0}),
	ets_db:add(onl, #onliner{name=224,socket=12,state=0}),
	io:format("fff ~p",[ets_db:find(onl,224)]),
	List = ets_db:match(onl, {onliner,'$1','_',0}),
	io:format("lll  ~p",[List]),
	ets_db:stop().
	