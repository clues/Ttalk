

-define(FATAL,0).
-define(ERROR,1).
-define(WARN,2).
-define(INFO,3).
-define(DEBUG,4).

-define(DEFAULT_FORMATTER,null).
-define(FORMATTER,null).

-record(filelogger,{level=?WARN,dir=".",file="smplog.txt",size=2048,rotation=5,format}).
-record(consolelogger,{level=?DEBUG,format}).