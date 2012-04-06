%%% -------------------------------------------------------------------
%%% Author  : jias
%%% Description :
%%%
%%% Created : 2010-11-28
%%% -------------------------------------------------------------------
-module(smplog).
-include("../include/smplog.hrl").
-created('2010-11-28').
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([run/0,
		 stop/0,
		 change_level/1,
		 debug/1,
		 debug/2,
		 info/1,
		 info/2,
		 warn/1,
		 warn/2,
		 error/1,
		 error/2,
		 fatal/1,
		 fatal/2
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(Newline,"\r\n").
-record(state, {flog=#filelogger{},clog=#consolelogger{},io}).

%% ====================================================================
%% External functions
%% ====================================================================
run() ->
	gen_server:start({local,?MODULE}, ?MODULE, [], []).

debug(_Msg)->
	debug("~p",[_Msg]).

debug(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {debug,_Fmat,_Msg}).

info(_Msg)->
	info("~p",[_Msg]).

info(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {info,_Fmat,_Msg}).	

warn(_Msg)->
	warn("~p",[_Msg]).

warn(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {warn,_Fmat,_Msg}).	

error(_Msg)->
	error("~p",[_Msg]).
error(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {error,_Fmat,_Msg}).

fatal(_Msg)->
	fatal("~p",[_Msg]).

fatal(_Fmat,_Msg) ->
	gen_server:cast(?MODULE, {fatal,_Fmat,_Msg}).

%% change_level({Type,Level})
%% Type = console | file
%% Level = debug | info | warn | error| fatal
change_level({Type,Level}) ->
	gen_server:cast(?MODULE, {change_level,Type,Level}).
			

%% error(_Fmat,_Msg) when _Fmat == []->
%% 	%Fmat = unicode:characters_to_binary(_Fmat, utf8),
%% 	%Msg = unicode:characters_to_binary(_Msg, utf8),	
%% 	gen_server:cast(?MODULE, {error,_Msg});



stop() ->
	gen_server:call(?MODULE, stop).

%% ====================================================================
%% Server functions
%% ====================================================================

get_time() ->
	{{Y,M,D},{H,Mi,S}} = calendar:local_time(),
	Time = integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ 
	integer_to_list(D) ++ "  " ++ integer_to_list(H) ++ "-" ++ 
	integer_to_list(Mi) ++ "-" ++ integer_to_list(S),
	list_to_atom(Time).
	

init([]) ->
	State = #filelogger{},
	AbsPath = filename:join(State#filelogger.dir, State#filelogger.file),
	case file:open(AbsPath, [append,binary]) of
		{ok,Io} ->
			NewState = #state{io=Io},
			{ok,NewState};
		{error,Rea} ->
			io:format("Filelogger init faild!!~n"),
			{ok,State}
	end.


handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({change_level,Type,Level},State) ->
	NewState = case Type of
		file ->
			Fstate = State#state.flog,
			Newlevel = get_level(Level),
			NewFstate = Fstate#filelogger{level=Newlevel},
			State#state{flog=NewFstate};
		console ->
			Cstate = State#state.clog,
			Newlevel = get_level(Level),
			NewCstate = Cstate#consolelogger{level=Newlevel},
			State#state{clog=NewCstate};
		_ ->
			State#state{}
	end,
	{noreply, NewState};

handle_cast({Level,Fmat,Msg}, State) ->
	Num_level = get_level(Level),
	Cstate = State#state.clog,
	Fstate = State#state.flog,
	if 
		Num_level =< Cstate#consolelogger.level ->
			try print_to_console(Level,Fmat,Msg)
			catch
				error:_ ->
					true
			end;
		true->
			true
	end,
	if 
		Num_level =< Fstate#filelogger.level ->
			try print_to_file(Level,Fmat,Msg,State)
			catch
				error:_ ->
					true
			end;
		true ->
			true
	end,
    {noreply, State}.





handle_info(Info, State) ->
    {noreply, State}.


terminate(Reason, State) ->
    case file:close(State#state.io) of
		ok ->
			done;
		{error,Reason} ->
			io:format("close iodevide faild by: ~p",[Reason])
	end,
	ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

print_to_console(Level,Fmat,Msg) ->
	_Fmat = "[~p] ~p " ++ Fmat ++ "~n",
	io:format(_Fmat,[Level,get_time()|Msg]).

print_to_file(Level,Fmat,Msg,State) ->
	_Fmat = "[~p] ~p " ++ Fmat ++ ?Newline,
	Data = io_lib:format(_Fmat, [Level,get_time()|Msg]),
	case file:write(State#state.io,Data) of
		ok ->
			true;
		{error,Rea} ->
			io:format("Write log faild ~p ~n",[Rea])
	end.

get_level(Level) ->
	case Level of
		debug ->
			4;
		info ->
			3;
		warn ->
			2;
		error ->
			1;
		fatal ->
			0
	end.