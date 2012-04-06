

-define(PORT_DEFAULT,8888).
-define(TIMEOUT_DEFAULT,10000).
-define(LENGTH_HEADER,38).
-define(OPTIONS_TCP,[binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%cmd
-define(SOCKET_CLOSE_CMD,-1).
-define(NORMAL_CMD,100).
-define(LOGIN_CMD,101).
-define(REGISTER_CMD,102).
-define(TRANSLATE_FILE_CMD,103).
-define(CONNECT_SERVER_CMD,104).
-define(CHANGE_STATE_CMD,105).
-define(UPDATE_VERSION_CMD,106).
-define(FLUSH_USERS_CMD,108).

-define(OK_STATE,200).
-define(ERROR_STATE,201).
-define(ONLINE_STATE,202).
-define(OFFLINE_STATE,203).
-define(LEAVE_STATE,204).

-define(ZERO,0).
-define(ONLINE,0).
-define(BUSSY,1).
-define(LEAVE,2).
-define(HIDDEN,3).

-define(BYTE, 8/unsigned-big-integer).
-define(WORD,16/unsigned-big-integer).
-define(DWORD,32/unsigned-big-integer).

-define(UNIT,unsigned-big-integer-unit:8).

-define(SOCK_TAB,t_sock).
-define(ONLINER_TAB,t_onliner).
-define(USER_TAB,t_user).

-define(SERVER_NAME,4017).

-record(user,{name,ip,port,fileport,state,permission}).
-record(header,{cmd,state,size,sender,receiver}).
-record(onliner,{name,socket,state}).



