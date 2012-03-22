%% use by the client to create the request
-record(myclient_request, {
          type,
          arith,
          data             % may be a string or two numbers
         }).

%% 
-record(myclient_dyndata, 
        { 
          none
         }
       ).

%% unused
-record(myclient, 
        { 
          fixme
         }
       ).

%%% Supported byte code instructions
-define(ECHO, 0).
-define(ADD, 1).
-define(SUB, 2).

