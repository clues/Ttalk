
-record(line,{cmd='undefined',data=[],state=0}).

-define(CMD_HELP,"help").%%-help
-define(CMD_LOGIN,"login").%%-login

-define(CODE_CMD_FORMAT_ERROR,1).