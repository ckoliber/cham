-module(cham_stun).
-author("koliber").
-define(ETS_STUN,ets_stun).
-define(UDP_OPTIONS, [binary,{active, false}]).
-export([
  stun_start/0
]).

-spec stun_start() -> tuple().
stun_start() ->
  try
    spawn(fun() ->
      {ok} = stun_heartbeat(),
      {ok,SOCKET} = gen_udp:open(cham_utils:get_env(cham,cham_stun_port),?UDP_OPTIONS),
      true = ets:insert(?ETS_STUN,{?ETS_STUN,SOCKET}),
      cham_log:log("Node "++atom_to_list(node())++" is Listening on port 1419 !"),
      stun_loop(SOCKET)
          end),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec stun_nat(port(),any(),integer()) -> atom().
stun_nat(SOCKET,HOST,PORT) ->
  %% restrictedcone,portrestrictedcone,symmetric
  try
    {ok,SOCKET_PING} = gen_udp:open(0,?UDP_OPTIONS),
    ok = gen_udp:send(SOCKET_PING,HOST,PORT,<<"PING">>),
    inet:setopts(SOCKET_PING, [{active, once}]),
    receive
      {udp,SOCKET_PING,HOST,PORT,<<"PONG">>} -> gen_udp:close(SOCKET_PING), restrictedcone
    after
      2000 ->
        {ok,{_,PORT_PING}} = inet:sockname(SOCKET_PING),
        gen_udp:send(SOCKET,HOST,PORT,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"ADD_MAPPER">>,
          <<"HOST">> => list_to_binary(cham_utils:get_env(cham,cham_public_host)),
          <<"PORT">> => integer_to_binary(PORT_PING)
        })),
        inet:setopts(SOCKET, [{active, once}]),
        receive
          {udp, SOCKET,HOST,PORT,DATA_BIN} ->
            DATA_BIN = jiffy:encode(#{
              <<"DATA_TYPE">> => <<"ADD_MAPPER">>,
              <<"RESULT">> => <<"OK">>
            }),
            ok = gen_udp:send(SOCKET_PING,HOST,PORT,<<"PING">>),
            receive
              {udp,SOCKET_PING,HOST,PORT,<<"PONG">>} -> gen_udp:close(SOCKET_PING), portrestrictedcone
            after
              2000 -> gen_udp:close(SOCKET_PING), symmetric
            end
        after
          2000 -> gen_udp:close(SOCKET_PING), symmetric
        end
    end
  catch
    _:_  -> symmetric
  end.

-spec stun_parse(byte()) -> tuple().
stun_parse(MESSAGE) ->
  try
    MESSAGE_MAP = jiffy:decode(MESSAGE,[return_maps]),
    DATA_TYPE = maps:get(<<"DATA_TYPE">>,MESSAGE_MAP),
    {ok,DATA_TYPE,MESSAGE_MAP}
  catch
    _:_  -> {no}
  end.

-spec stun_loop(port()) -> port().
stun_loop(SOCKET) ->
  try
    inet:setopts(SOCKET, [{active, once}]),
    receive
      {udp,SOCKET,HOST,PORT,<<"HEART">>} ->
        stun_update(HOST,PORT);
      {udp,SOCKET,HOST,PORT,MESSAGE} ->
        {ok,DATA_TYPE,MESSAGE_MAP} = stun_parse(MESSAGE),
        stun_receive(SOCKET,HOST,PORT,DATA_TYPE,MESSAGE_MAP)
    end
  catch
    A:B  -> io:fwrite("~nCHAM STUN ERROR ~p : ~p ~n",[A,B])
  end,
  stun_loop(SOCKET).

-spec stun_update(any(),integer()) -> tuple().
stun_update(HOST,PORT) ->
  try
    [{_,NAT,HEARTS}] = ets:lookup(?ETS_STUN,stun_address(HOST,PORT)),
    true = ets:insert(?ETS_STUN,{stun_address(HOST,PORT),NAT,if HEARTS > 9 -> 10 ; true -> HEARTS+1 end}),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec stun_clear(list()) -> tuple().
stun_clear([{PEER_ADDRESS,PEER_NAT,PEER_HEARTS}|Tail])->
  try
    true = ets:insert(?ETS_STUN,{PEER_ADDRESS,PEER_NAT,PEER_HEARTS-1}),
    true = PEER_HEARTS < 1,
    [HOST,PORT] = string:tokens(atom_to_list(PEER_ADDRESS),"_"),
    cham_scylla_driver:stun_delete(cham_utils:parse_address(HOST),list_to_integer(PORT)),
    ets:delete(?ETS_STUN,PEER_ADDRESS),
    if
      [] =/= Tail -> stun_clear(Tail);
      true -> {ok}
    end
  catch
    _:_  ->
      if
        [] =/= Tail -> stun_clear(Tail);
        true -> {ok}
      end
  end.

-spec stun_heartbeat() -> tuple().
stun_heartbeat() ->
  try
    spawn(fun() ->
      LOOP = fun(LOOP) ->
        try
          timer:sleep(1000),
          stun_clear(ets:tab2list(?ETS_STUN))
        catch
          _:_  -> {no}
        end,
        LOOP(LOOP)
             end,
      LOOP(LOOP)
          end),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec stun_address(any(),integer()) -> atom().
stun_address(HOST,PORT) ->
  list_to_atom(inet:ntoa(HOST)++"_"++integer_to_list(PORT)).

-spec stun_cast(port(),any(),integer(),map()) -> tuple().
stun_cast(SOCKET,HOST,PORT,MESSAGE_MAP) ->
  try
    ID = maps:get(<<"ID">>,MESSAGE_MAP),
    SCODE = maps:get(<<"SCODE">>,MESSAGE_MAP),
    PEER_ID = maps:get(<<"PEER_ID">>,MESSAGE_MAP),
    {ok} = cham_scylla_driver:peer_is(ID,SCODE),
    NAT = stun_nat(SOCKET,HOST,PORT),
    true = ets:insert(?ETS_STUN,{stun_address(HOST,PORT),10}),
    {ok} = cham_scylla_driver:stun_set(ID,HOST,PORT,node(),NAT,PEER_ID),
    case cham_scylla_driver:stun_get(ID,PEER_ID) of
      {ok,PEER_HOST,PEER_PORT,PEER_NODE,PEER_NAT} ->
        if
          ((NAT =:= 'restrictedcone') orelse (PEER_NAT =:= 'restrictedcone') orelse ((NAT =:= 'portrestrictedcone') andalso (PEER_NAT =:= 'portrestrictedcone'))) ->
            % self   -> 1 -> requestor  -> handshake to target
            % target -> 2 -> waiter     -> another wait -> handshake from requestor
            spawn(PEER_NODE,fun() ->
              [{_,PEER_SOCKET}] = ets:lookup(?ETS_STUN,?ETS_STUN),
              ok = gen_udp:send(PEER_SOCKET,PEER_HOST,PEER_PORT,jiffy:encode(#{
                <<"DATA_TYPE">> => <<"CAST">>,
                <<"RESULT">> => <<"OK">>,
                <<"MODE">> => <<"2">>,
                <<"HOST">> => list_to_binary(inet:ntoa(HOST)),
                <<"PORT">> => integer_to_binary(PORT)
              }))
                                   end),
            ok = gen_udp:send(SOCKET,HOST,PORT,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"CAST">>,
              <<"RESULT">> => <<"OK">>,
              <<"MODE">> => <<"1">>,
              <<"HOST">> => list_to_binary(inet:ntoa(PEER_HOST)),
              <<"PORT">> => integer_to_binary(PEER_PORT)
            })),
            {ok} = cham_scylla_driver:stun_delete(ID,PEER_ID),
            {ok} = cham_scylla_driver:stun_delete(PEER_ID,ID),
            {ok};
          true ->
            {ok,TURN_HOST,TURN_PORT} = cham_turn:turn_spawn(),
            spawn(PEER_NODE,fun() ->
              [{_,PEER_SOCKET}] = ets:lookup(?ETS_STUN,?ETS_STUN),
              ok = gen_udp:send(PEER_SOCKET,PEER_HOST,PEER_PORT,jiffy:encode(#{
                <<"DATA_TYPE">> => <<"CAST">>,
                <<"RESULT">> => <<"OK">>,
                <<"MODE">> => <<"2">>,
                <<"HOST">> => list_to_binary(inet:ntoa(TURN_HOST)),
                <<"PORT">> => integer_to_binary(TURN_PORT)
              }))
                                   end),
            ok = gen_udp:send(SOCKET,HOST,PORT,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"CAST">>,
              <<"RESULT">> => <<"OK">>,
              <<"MODE">> => <<"1">>,
              <<"HOST">> => list_to_binary(inet:ntoa(TURN_HOST)),
              <<"PORT">> => integer_to_binary(TURN_PORT)
            })),
            {ok} = cham_scylla_driver:stun_delete(ID,PEER_ID),
            {ok} = cham_scylla_driver:stun_delete(PEER_ID,ID),
            {ok} %% turn needed
        end;
      _ ->
        {ok}
      % target is offline -> wait !!!
    end
  catch
    _:_  ->
      try
        gen_udp:send(SOCKET,HOST,PORT,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"CAST">>,
          <<"RESULT">> => <<"NO">>,
          <<"LOG">> => <<"SECURITY_ERROR">>
        })),
        {no}
      catch
        _:_  -> {no}
      end
  end.

-spec stun_receive(port(),any(),integer(),byte(),map()) -> atom().
stun_receive(SOCKET,HOST,PORT,DATA_TYPE,MESSAGE_MAP) ->
  try
    case DATA_TYPE of
      <<"CAST">> -> spawn(fun() -> stun_cast(SOCKET,HOST,PORT,MESSAGE_MAP) end);
      _ -> {ok}
    end
  catch
    _:_ -> {ok}
  end.
