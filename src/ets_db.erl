

%% ts_db: transmission server data base
%% 
-module(ets_db).
-export([new_tab/2,
		 add/2,
		 init/0,
		 del_all_tab/0,
		 stop/0,
		 get_tabs/0,
		 del/1,
		 del/2,
		 update/2,
		 find/2,
		 match/2,
		 get_tabbytag/1
		 ]).
-author(jias.chao).
-email(lino.chao@gmail.com).
-create('2010-11-24').

-define(KEYPOS,2).
-define(MEMTAB,'dydb@lnc').
-define(TIMEOUT,10000).
-record(table,{name,  %% table name
			   rtag,  %% table tag,is also mean tuple first element
			   esize=0,%% element number of tuple
			   rsize=0 %% the number of record in table
			  }).
-type exception() :: noreplay|{replay,timeout}.

%%init memtab and registe this db named 'dydb',and
%%new a table when initialization,this table is a
%%core table what record all table who create after
%%core table keep the followed table's id,tag,record size and
%%the record number,core table attribute is set and public,this
%%mean it's record unique and can be read write by other process
init() ->
	case whereis(?MEMTAB) of
		undefined ->
			Rootid = ets:new(root, [set,public,{keypos,?KEYPOS}]),	
			register(?MEMTAB,spawn(fun() ->
								   process_flag(trap_exit,true),
								   loop(Rootid) 
						   end)
			);
		Pid ->
			{error,already_init}
	end.

loop(Rootid) ->
	receive
		{terminate,_From} ->
			smplog:info("[terminate] stop dynamic db"),
			replay(_From,{ok,terminate}),
			exit(normal);
		{'EXIT',Pid,Reason} ->
			smplog:warn("[exit] from <~p> by <~p> ",[Pid,Reason]),
			loop(Rootid);
		{new,TabName,Rcd,_From} ->
			smplog:info("[new] table: <~p> format: <~p>",[TabName,Rcd]),
			Re = do_new(Rootid ,TabName,Rcd),
			replay(_From,Re),
			loop(Rootid);
		{add,TabName, Rcd,_From} ->
			smplog:info("[add] record: <~p> to table:<~p>",[Rcd,TabName]),
			Re = do_add(Rootid ,TabName,Rcd),
			replay(_From,Re),
			loop(Rootid);
		{find,TabName,Key,_From} ->
			smplog:info("[find] from table:<~p> by key:<~p>",[TabName,Key]),
			Re = do_find(Rootid,TabName,Key),
			replay(_From,Re),
			loop(Rootid);
		{match,TabName,Pattren,_From} ->
			smplog:info("[match] from table:<~p> by key:<~p>",[TabName,Pattren]),
			Re = do_match(Rootid,TabName,Pattren),
			replay(_From,Re),
			loop(Rootid);		
		{del,alltab,_From} ->
			smplog:info("[del] all table in dydb"),
			Re = removeAlltab(Rootid),
			replay(_From,Re),
			loop(Rootid);		
		{del,TabName,_From} ->
			smplog:info("[del] table: <~p>",[TabName]),
			Re = do_del(Rootid,TabName),
			replay(_From,Re),
			loop(Rootid);
		{del,TabName,Key,_From} ->
			smplog:info("[del] table: <~p> by key: <~p>",[TabName,Key]),
			Re = do_del(Rootid,TabName,Key),
			replay(_From,Re),
			loop(Rootid);
		{update,TabName,Rcd,_From} ->
			smplog:info("[update] record:<~p> in table:~p",[Rcd,TabName]),
			Re = do_update(Rootid,TabName,Rcd),
			replay(_From,Re),
			loop(Rootid);
		{get,all_tab_name,_From} ->
			smplog:info("[get] all table name in dydb"),
			Re = do_get(Rootid,all_tab_name),
			replay(_From,Re),
			loop(Rootid);
		{get,tab_name_bytag,Tag,_From} ->
			smplog:info("[get] table name by tag:<~p>",[Tag]),
			Re = do_get(Rootid,Tag),
			replay(_From,Re),
			loop(Rootid)
	end.
			

do_new(RootTab,TabName,Rcd) ->
	case ets:info(TabName) of
		undefined ->
				ets:new(TabName, [protected,set,named_table,{keypos,?KEYPOS}]),
				try ets:insert(RootTab,#table{name=TabName,rtag=element(1,Rcd),esize=size(Rcd)}) of
					_ ->
						ok
				catch
					error:_Ex ->
						{error,_Ex}
				end;
		Other ->
			{error,alreay_exist}
	end.

do_add(RootTab,TabName,Rcd) ->
	case chek_fat(RootTab,TabName,Rcd) of
		true ->
			case ets:lookup(TabName, element(?KEYPOS,Rcd)) of
				[] ->
					ets:insert(TabName,Rcd),
					try ets:update_element(RootTab, TabName,{5,ets:info(TabName, size)}) of
						_ ->
							ok
					catch
						error:_Ex ->
							{error,_Ex}
					end;
				_O ->
					{error,id_exist}
			end;
		_Ex ->
			_Ex
	end.
		
chek_fat(RootTab,TabName,Rcd) ->
	case ets:lookup(RootTab, TabName) of
		[] ->
			{error,table_noexist};
		_Other ->
			T = hd(_Other),
			if 
				T#table.rtag =/= element(1,Rcd) ->
					{error,ement_tag_unmatch};
				T#table.esize =/= size(Rcd) ->
					{error,ement_size_unmatch};
				true ->
					true
			end
	end.
			

do_find(RootTab,TabName,Key) ->
	case ets:lookup(RootTab, TabName) of
		[] ->
			{error,table_noexist};
		Other ->
			if 
				Key =:= '_' ->
					ets:tab2list(TabName);
				true ->
					ets:lookup(TabName, Key)
			end
	end.

do_match(RootTab,TabName,Pattren) ->
	case ets:lookup(RootTab, TabName) of
		[] ->
			{error,table_noexist};
		Other ->
			List = ets:match(TabName, Pattren),
			lists:flatten(List)
	end.

do_del(RootTab,TabName) ->
	case ets:lookup(RootTab, TabName) of
		[] ->
			{error,table_noexist};
		Other ->
			case ets:delete(TabName) of
				true ->
					case ets:delete(RootTab, TabName) of
						true ->
							ok;
						_In1 ->
							{error,_In1}
					end;
				_In2 ->
					{error,_In2}
			end
	end.

do_del(RootTab,TabName,Key) ->
	case ets:lookup(RootTab, TabName) of
		[] ->
			{error,table_noexist};
		Other ->
			case ets:delete(TabName, Key) of
				true ->
					ok;
				_In ->
					{error,_In}
			end
	end.

do_update(RootTab,TabName,Rcd) ->
	case chek_fat(RootTab,TabName,Rcd) of
		true ->
			case ets:lookup(TabName, element(?KEYPOS,Rcd)) of
				[] ->
					{error,id_noexist};
				_I ->
					case ets:insert(TabName, Rcd) of
						true ->
							ok;
						_EX ->
							{error,_EX}
					end
			end;
		_Ex ->
			_Ex
	end.

do_get(RootTab,Option) when Option =:= 'all_tab_name' ->
	L = ets:tab2list(RootTab),
	[Name || {table,Name,_,_,_} <- L];

do_get(RooTab,Tag) ->
	L = ets:tab2list(RooTab),
	[Name || {table,Name,Ta,_,_} <- L,Tag =:= Ta].

removeAlltab(RootTab) ->
	L = [TabName ||{table,TabName,_,_,_} <- ets:tab2list(RootTab)],
	F = fun(TabName) ->
				case ets:delete(TabName) of
					true ->
						true,
						ets:delete(RootTab, TabName);
					_Ex ->
						_Ex
				end
			end,
	[F(TN) || TN <- L],
	VA = ets:info(RootTab, size),
	if 
		VA == 0 ->
			ok;
		true ->
			{error,undefined}
	end.

%%def=======export function==============
%% create a new table

-spec new_tab(atom(),tuple())->ok | {error,term()} |exception().
new_tab(TabName,Rcd) ->
	do_search({new,TabName,Rcd,self()}).


%% add record to a table

-spec add(atom(),tuple())->ok|{error,term()}|exception().
add(TabName,Rcd) ->
	do_search({add,TabName, Rcd,self()}).

%%if key equal '_',will return all record in this tab
-spec find(atom(),atom)->[any()]|{error,term()}|exception().
find(TabName,Key) ->
	do_search({find,TabName,Key,self()}).

-spec match(atom(),tuple())->[any()]|{error,term()}|exception().
match(TabName,Parten) ->
	do_search({match,TabName,Parten,self()}).

-spec update(atom(),tuple())->ok|{error,term()}|exception().
update(TabName,Record) ->
	do_search({update,TabName,Record,self()}).

-spec del(atom(),atom()) ->ok|{error,term()}|exception().
del(TabName,Key) ->
	do_search({del,TabName,Key,self()}).

-spec del(atom()) -> ok|{error,term()}|exception().
del(TabName) ->
	do_search({del,TabName,self()}).

-spec get_tabs() -> [atom()]|exception().
get_tabs() ->
	do_search({get,all_tab_name,self()}).

-spec get_tabbytag(atom()) -> [atom()]|exception().
get_tabbytag(Tag) ->
	do_search({get,tab_name_bytag,Tag,self()}).

-spec del_all_tab() -> ok|{error,term()}|exception().
del_all_tab() ->
	do_search({del,alltab,self()}).

-spec stop() -> {ok,terminate}|exception().
stop() ->
	do_search({terminate,self()}).

%%end =======export function==============

replay(_To,Replay) ->
	_To ! {replay,Replay}.


response() ->
	receive
		{replay,_Any} ->
			_Any;
		_ ->
			noreplay
	after ?TIMEOUT->
			{replay,timout}
	end.

do_search(Request) ->
	case whereis(?MEMTAB) of
		undefined ->
			{error,memtab_uninit};
		_Pid ->
			_Pid ! Request,
			response()
	end.


