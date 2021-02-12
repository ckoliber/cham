-module(cham_turn).
-author("koliber").
-export([
  turn_spawn/0
]).

-spec turn_spawn() -> tuple().
turn_spawn() ->
  try
    {ok,SOCKET} = gen_udp:open(0),
    {ok,TURN_PORT} = inet:port(SOCKET),
    ok = gen_udp:close(SOCKET),
    {ok,NODE_ATOM} = cham_scylla_driver:turn_get(),
    {ok,[{TURN_HOST,_,_}|_]} = rpc:call(NODE_ATOM,application,get_env,[cham_app,cham_public_host]),
    spawn(NODE_ATOM,fun() ->
      {ok} = cham_scylla_driver:turn_set(NODE_ATOM,1),
      os:cmd("./cham_turn/ChaMTurn "++integer_to_list(TURN_PORT)),
      {ok} = cham_scylla_driver:turn_set(NODE_ATOM,-1)
                      end),
    {ok,inet:parse_address(TURN_HOST),TURN_PORT}
  catch
    A:B  ->
      io:fwrite("~nCHAM TURN ERROR ~p : ~p~n",[A,B]),
      {no}
  end.