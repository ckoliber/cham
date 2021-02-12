-module(cham_scylla_driver).
-author("koliber").
-record(cham_socket,{socket,peer_id}).
-export([
  driver_connect/0,
  driver_disconnect/0,
  socket_cast/2,
  captcha_get/1,
  captcha_set/2,
  link_get/2,
  link_set/2,
  link_remove/1,
  peer_cast/2,
  peer_peer/2,
  peer_is/2,
  peer_phone/2,
  peer_create/3,
  peer_delete/1,
  peer_load/6,
  peer_set/6,
  connection_cast/2,
  connection_peer/2,
  connection_history/2,
  connection_create/2,
  connection_delete/2,
  connection_set/6,
  message_set/8,
  message_delete/3,
  message_edit/6,
  message_read/3,
  stun_set/6,
  stun_get/2,
  stun_delete/2,
  turn_set/2,
  turn_get/0
]).

driver_connect() ->
  try
    ok = application:set_env(erlcass,cluster_options,[
      {contact_points, cham_utils:get_env(cham,scylla_points)},
      {protocol_version, 3},{latency_aware_routing, true},
      {token_aware_routing, true},
      {number_threads_io, 4},
      {queue_size_io, 128000},
      {max_connections_host, 5},
      {pending_requests_high_watermark, 128000},
      {tcp_nodelay, true},
      {tcp_keepalive, {true, 1800}}
    ]),
    {ok,_} = application:ensure_all_started(erlcass),
    {ok} = driver_init(),
    {ok}
  catch
    A:B  -> io:fwrite("~nSCYLLA DRIVER_CONNECT ERROR ~p : ~p ~n",[A,B]), {no}
  end.

driver_init() ->
  try
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Every Peer have :
    %   id name         type  pn  permissions
    %   1. ID           TEXT  -
    %   2. SCODE        TEXT  -
    %   3. PHONE        TEXT  A   [0:hidden,1:visible,2:visible&change]
    %   4. LINK         TEXT  B   [0:hidden,1:visible,2:visible&change]
    %   5. NAME         TEXT  C   [0:hidden,1:visible,2:visible&change]
    %   6. BIO          TEXT  D   [0:hidden,1:visible,2:visible&change]
    %   7. STATE        TEXT  E   [0:hidden,1:visible,2:visible&change]
    %   8. PICTURES     MAP{[date : link]}                    F   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
    %   9. PEERS  MAP{[peer_id : permission]}                 G   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
    %     (get function : contains : range type : _ : any)
    %  10. CONNECTIONS  MAP{[connection_id : date]}           H   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
    %  11. SOCKETS      MAP{[socket:node]}                    -   have no permission to see or change
    %  PERMISSION : ABCDEFGH
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Every Connection have :
    %   id name         type  pn  permissions
    %   1. ID           TEXT  -
    %   2. LINK         TEXT  A   [0:hidden,1:visible,2:visible&change]
    %   3. NAME         TEXT  B   [0:hidden,1:visible,2:visible&change]
    %   4. BIO          TEXT  C   [0:hidden,1:visible,2:visible&change]
    %   5. PICTURES     MAP{[date : link]}                    D   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
    %   6. PEERS        MAP{[peer_id : permission+F]}         E   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove] // add : boss !!!,have a default peer : * -> default permission for join if have'nt -> can't join !!!
    %   7. HISTORIES    MAP{[peer_id : date]}                 -   have no permission to see or change,when user removed from PEERS add date to this field for message loading
    %      MESSAGES                                           F   [0:noread_nowrite,1:read_nowrite,2:noread_write,3:read_write,4:read_write_delete]
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ok = erlcass:query(<<"CREATE KEYSPACE IF NOT EXISTS cham WITH REPLICATION={'class':'SimpleStrategy','replication_factor':'1'}">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.CAPTCHA(PHONE TEXT,CAPTCHA TEXT,DATE TEXT,PRIMARY KEY(PHONE))">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.PEERS(ID TEXT,SCODE TEXT,PHONE TEXT,LINK TEXT,NAME TEXT,BIO TEXT,STATE TEXT,PICTURES MAP<TEXT,TEXT>,PEERS MAP<TEXT,TEXT>,CONNECTIONS MAP<TEXT,TEXT>,SOCKETS MAP<TEXT,BLOB>,PRIMARY KEY(ID,PHONE))">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.CONNECTIONS(ID TEXT,LINK TEXT,NAME TEXT,BIO TEXT,PICTURES MAP<TEXT,TEXT>,PEERS MAP<TEXT,TEXT>,HISTORIES MAP<TEXT,TEXT>,PRIMARY KEY(ID))">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.LINKS(ID TEXT,LINK TEXT,PRIMARY KEY(ID,LINK))">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.MESSAGES(PEER_ID TEXT,CONNECTION_ID TEXT,MODE TEXT,MESSAGE_TYPE TEXT,DATE TEXT,FORWARD_ID TEXT,REPLY_DATE TEXT,DATA_1 TEXT,DATA_2 TEXT,PRIMARY KEY(CONNECTION_ID,DATE,PEER_ID)) WITH CLUSTERING ORDER BY (DATE DESC,PEER_ID DESC)">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.STUN(ID TEXT,HOST TEXT,PORT TEXT,NODE TEXT,NAT TEXT,PEER_ID TEXT,PRIMARY KEY(ID,PEER_ID))">>),
    ok = erlcass:query(<<"CREATE TABLE IF NOT EXISTS cham.TURN(NODE TEXT,CONNECTIONS INT,PRIMARY KEY(NODE))">>),
    {ok}
  catch
    A:B  -> io:fwrite("~nSCYLLA DRIVER_INIT ERROR ~p : ~p ~n",[A,B]), {no}
  end.

driver_disconnect() ->
  try
    ok = application:stop(erlcass),
    {ok}
  catch
    A:B  -> io:fwrite("~nSCYLLA DRIVER_DISCONNECT ERROR ~p : ~p ~n",[A,B]), {no}
  end.

-spec socket_cast(byte(),byte()) -> tuple().
socket_cast(ID,MESSAGE) ->
  CASTER = fun
           (_,[]) -> {ok};
           (CASTER,[{NODE,SOCKET}|TAIL]) ->
             spawn(binary_to_atom(NODE,utf8),fun() -> cham_server:send(#cham_socket{socket = binary_to_term(SOCKET),peer_id = ID},MESSAGE) end),
             CASTER(CASTER,TAIL)
         end,
  try
    {ok,_,[[SOCKETS]]} = erlcass:query(iolist_to_binary(["SELECT SOCKETS FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
    {ok} = CASTER(CASTER,SOCKETS),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec captcha_set(byte(),byte()) -> tuple().
captcha_set(PHONE,CAPTCHA) ->
  try
    ok = erlcass:query(iolist_to_binary(["DELETE FROM cham.CAPTCHA WHERE PHONE='",PHONE,"'"])),
    ok = erlcass:query(iolist_to_binary(["INSERT INTO cham.CAPTCHA(PHONE,CAPTCHA,DATE) VALUES('",PHONE,"','",CAPTCHA,"','",cham_utils:chrono_date(),"')"])),
    {ok}
  catch
    A:B  -> io:fwrite("~nSCYLLA CAPTCHA_SET ERROR ~p : ~p ~n",[A,B]), {no}
  end.

-spec captcha_get(byte()) -> tuple().
captcha_get(PHONE) ->
  try
    case erlcass:query(iolist_to_binary(["SELECT CAPTCHA,DATE FROM cham.CAPTCHA WHERE PHONE='",PHONE,"'"])) of
      {ok,_,[[CAPTCHA,DATE]]} ->
        {ok,NOW_NUM_INT} = cham_utils:chdate_to_num(cham_utils:chrono_date()),
        {ok,DATE_NUM_INT} = cham_utils:chdate_to_num(DATE),
        if
          ( (NOW_NUM_INT - DATE_NUM_INT) < 300000000 ) -> % 5 min's
            {ok,CAPTCHA};
          true ->
            {no,expire}
        end;
      _ ->
        {no,incorrect}
    end
  catch
    A:B  -> io:fwrite("~nSCYLLA CAPTCHA_GET ERROR ~p : ~p ~n",[A,B]), {no}
  end.

-spec link_get(byte(),byte()) -> tuple().
link_get(KEY,TYPE) ->
  try
    true = ((TYPE =:= <<"LINK">>) orelse (TYPE =:= <<"ID">>)),
    {ok,_,[[ID,LINK]]} = erlcass:query(iolist_to_binary(["SELECT ID,LINK FROM cham.LINKS WHERE ",TYPE,"='",KEY,"' ALLOW FILTERING"])),
    {ok,ID,LINK}
  catch
    _:_  -> {no}
  end.

-spec link_set(byte(),byte()) -> tuple().
link_set(ID,LINK) ->
  try
    {ok} = link_remove(ID),
    ok = erlcass:query(iolist_to_binary(["INSERT INTO cham.LINKS(ID,LINK) VALUES('",ID,"','",LINK,"')"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec link_remove(byte()) -> tuple().
link_remove(ID) ->
  try
    ok = erlcass:query(iolist_to_binary([<<"DELETE FROM cham.LINKS WHERE ID='",ID,"'">>])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec peer_cast(byte(),byte()) -> tuple().
peer_cast(ID,MESSAGE) ->
  CASTER = fun
           (_,[]) -> {ok};
           (CASTER,[{CONNECTION_ID,_}|TAIL]) ->
             connection_cast(CONNECTION_ID,MESSAGE),
             CASTER(CASTER,TAIL)
         end,
  try
    {ok,_,[[CONNECTIONS]]} = erlcass:query(iolist_to_binary(["SELECT CONNECTIONS FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
    {ok} = CASTER(CASTER,CONNECTIONS),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec peer_peer(byte(),byte()) -> tuple().
peer_peer(PEER_ID,ID) ->
  % PERMISSION
  SEARCHER = fun
               (_,[],RESULT,_) -> {ok,RESULT};
               (SEARCHER,[{TEMP_ID,TEMP_PERMISSION}|TAIL],RESULT,RANK) ->
                 TEMP_RANK = (length(PEER_ID) - length(PEER_ID--TEMP_ID)),
                 if
                   (TEMP_RANK > RANK)->
                     SEARCHER(SEARCHER,TAIL,{
                       binary:at(TEMP_PERMISSION,0) - 48,
                       binary:at(TEMP_PERMISSION,1) - 48,
                       binary:at(TEMP_PERMISSION,2) - 48,
                       binary:at(TEMP_PERMISSION,3) - 48,
                       binary:at(TEMP_PERMISSION,4) - 48,
                       binary:at(TEMP_PERMISSION,5) - 48,
                       binary:at(TEMP_PERMISSION,6) - 48,
                       binary:at(TEMP_PERMISSION,7) - 48
                     },TEMP_RANK);
                   true ->
                     SEARCHER(SEARCHER,TAIL,RESULT,RANK)
                 end

             end,
  try
    {ok,_,[[PEERS]]} = erlcass:query(iolist_to_binary(["SELECT PEERS FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
    {PHONE_PERMISSION,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,STATE_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,CONNECTIONS_PERMISSION} = SEARCHER(SEARCHER,PEERS,{},0),
    {ok,PHONE_PERMISSION,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,STATE_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,CONNECTIONS_PERMISSION}
  catch
    _:_ -> {no}
  end.

-spec peer_is(byte(),byte()) -> tuple().
peer_is(ID,SCODE) ->
  try
    {ok,_,[[SCODE]]} = erlcass:query(iolist_to_binary(["SELECT SCODE FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec peer_phone(byte(),byte()) -> tuple().
peer_phone(KEY,TYPE) ->
  try
    true = ((TYPE =:= <<"PHONE">>) orelse (TYPE =:= <<"ID">>)),
    {ok,_,[[ID,SCODE,PHONE]]} = erlcass:query(iolist_to_binary(["SELECT ID,SCODE,PHONE FROM cham.PEERS WHERE ",TYPE,"='",KEY,"' ALLOW FILTERING"])),
    {ok,ID,SCODE,PHONE}
  catch
    _:_  -> {no}
  end.

-spec peer_create(byte(),byte(),byte()) -> tuple().
peer_create(ID,SCODE,PHONE) ->
  try
    ok = erlcass:query(iolist_to_binary(["INSERT INTO cham.PEERS(ID,SCODE,PHONE,LINK,NAME,BIO,STATE,PICTURES,PEERS,CONNECTIONS,SOCKETS) VALUES('",ID,"','",SCODE,"','",PHONE,"','','','','offline',{},{'",ID,"':'222224444','_':'111111000'},{},{})"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec peer_delete(byte()) -> tuple().
peer_delete(ID) ->
  DELETER = fun
              (_,[]) ->
                {ok};
              (DELETER,[{CONNECTION_ID,_}|TAIL]) ->
                connection_set(ID,CONNECTION_ID,<<"PEERS">>,ID,ok,<<"REMOVE">>),
                DELETER(DELETER,TAIL)
            end,
  try
    % left from any group,make anything invisible and remove anything
    {ok,_,_,PHONE} = peer_phone(ID,<<"ID">>),
    {ok,_,[[CONNECTIONS]]} = erlcass:query(iolist_to_binary(["SELECT CONNECTIONS FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
    {ok} = DELETER(DELETER,CONNECTIONS),
    link_remove(ID),
    ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET PHONE='',LINK='',STATE='delete',PEERS={'_':'000000000'}  WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec peer_load(byte(),byte(),byte(),byte(),byte(),byte()) -> tuple().
peer_load(PEER_ID,ID,TYPE,MODE,PARAM_1,PARAM_2) ->
  try
    true = (
        TYPE =:= <<"PEER_INFOS">> orelse
        TYPE =:= <<"PEER_PICTURES">> orelse
        TYPE =:= <<"PEER_PEERS">> orelse
        TYPE =:= <<"PEER_CONNECTIONS">> orelse

        TYPE =:= <<"CONNECTION_INFOS">> orelse
        TYPE =:= <<"CONNECTION_PICTURES">> orelse
        TYPE =:= <<"CONNECTION_PEERS">> orelse
        TYPE =:= <<"MESSAGES">>
    ),
    case TYPE of
      <<"PEER_INFOS">> ->
        PICTURER = fun
                    (_,[],{_,LINK}) -> LINK;
                    (PICTURER,[{DATE,LINK}|TAIL],{DATE_TEMP,LINK_TEMP}) ->
                      PICTURER(PICTURER,TAIL,if TAIL,DATE > DATE_TEMP -> {DATE,LINK}; true -> {DATE_TEMP,LINK_TEMP} end)
                  end,
        LOADER = fun
                   (_,[],RESULT) -> RESULT;
                   (LOADER,[{_,0,_}|TAIL],RESULT) -> LOADER(LOADER,TAIL,RESULT);
                   (LOADER,[{ITEM_KEY,_,ITEM_VALUE}|TAIL],RESULT) -> LOADER(LOADER,TAIL,RESULT++[{ITEM_KEY,ITEM_VALUE}])
                 end,
        {ok,PHONE_PERMISSION,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,STATE_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,CONNECTIONS_PERMISSION} = peer_peer(PEER_ID,ID),
        {ok,_,[[PHONE,LINK,NAME,BIO,STATE,PICTURES]]} = erlcass:query(iolist_to_binary(["SELECT PHONE,LINK,NAME,BIO,STATE,PICTURES FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({
          LOADER(LOADER,[{<<"PHONE">>,PHONE_PERMISSION,PHONE},{<<"LINK">>,LINK_PERMISSION,LINK},{<<"NAME">>,NAME_PERMISSION,NAME},{<<"BIO">>,BIO_PERMISSION,BIO},{<<"STATE">>,STATE_PERMISSION,STATE},{<<"PICTURE">>,PICTURES_PERMISSION,PICTURER(PICTURES)}],[])++
          [{<<"PERMISSION">>,iolist_to_binary([PHONE_PERMISSION+48,LINK_PERMISSION+48,NAME_PERMISSION+48,BIO_PERMISSION+48,STATE_PERMISSION+48,PICTURES_PERMISSION+48,PEERS_PERMISSION+48,CONNECTIONS_PERMISSION+48])}]
        })};
      <<"PEER_PICTURES">> ->
        {ok,_,_,_,_,_,PICTURES_PERMISSION,_,_} = peer_peer(PEER_ID,ID),
        true = (PICTURES_PERMISSION =:= 1 orelse PICTURES_PERMISSION =:= 2 orelse PICTURES_PERMISSION =:= 3 orelse PICTURES_PERMISSION =:= 4),
        {ok,_,[[PICTURES]]} = erlcass:query(iolist_to_binary(["SELECT PICTURES FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({PICTURES})};
      <<"PEER_PEERS">> ->
        {ok,_,_,_,_,_,_,PEERS_PERMISSION,_} = peer_peer(PEER_ID,ID),
        true = (PEERS_PERMISSION =:= 1 orelse PEERS_PERMISSION =:= 2 orelse PEERS_PERMISSION =:= 3 orelse PEERS_PERMISSION =:= 4),
        {ok,_,[[PEERS]]} = erlcass:query(iolist_to_binary(["SELECT PEERS FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({PEERS})};
      <<"PEER_CONNECTIONS">> ->
        {ok,_,_,_,_,_,_,_,CONNECTIONS_PERMISSION} = peer_peer(PEER_ID,ID),
        true = (CONNECTIONS_PERMISSION =:= 1 orelse CONNECTIONS_PERMISSION =:= 2 orelse CONNECTIONS_PERMISSION =:= 3 orelse CONNECTIONS_PERMISSION =:= 4),
        {ok,_,[[CONNECTIONS]]} = erlcass:query(iolist_to_binary(["SELECT CONNECTIONS FROM cham.PEERS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({CONNECTIONS})};
      <<"CONNECTION_INFOS">> ->
        PICTURER = fun
                     (_,[],{_,LINK}) -> LINK;
                     (PICTURER,[{DATE,LINK}|TAIL],{DATE_TEMP,LINK_TEMP}) ->
                       PICTURER(PICTURER,TAIL,if TAIL,DATE > DATE_TEMP -> {DATE,LINK}; true -> {DATE_TEMP,LINK_TEMP} end)
                   end,
        LOADER = fun
                   (_,[],RESULT) -> RESULT;
                   (LOADER,[{_,0,_}|TAIL],RESULT) -> LOADER(LOADER,TAIL,RESULT);
                   (LOADER,[{ITEM_KEY,_,ITEM_VALUE}|TAIL],RESULT) -> LOADER(LOADER,TAIL,RESULT++[{ITEM_KEY,ITEM_VALUE}])
                 end,
        {ok,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,MESSAGES_PERMISSION} = connection_peer(PEER_ID,ID),
        {ok,_,[[LINK,NAME,BIO,PICTURES]]} = erlcass:query(iolist_to_binary(["SELECT LINK,NAME,BIO,PICTURES FROM cham.CONNECTIONS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({
          LOADER(LOADER,[{<<"LINK">>,LINK_PERMISSION,LINK},{<<"NAME">>,NAME_PERMISSION,NAME},{<<"BIO">>,BIO_PERMISSION,BIO},{<<"PICTURE">>,PICTURES_PERMISSION,PICTURER(PICTURES)}],[])++
            [{<<"PERMISSION">>,iolist_to_binary([LINK_PERMISSION+48,NAME_PERMISSION+48,BIO_PERMISSION+48,PICTURES_PERMISSION+48,PEERS_PERMISSION+48,MESSAGES_PERMISSION+48])}]
        })};
      <<"CONNECTION_PICTURES">> ->
        {ok,_,_,_,PICTURES_PERMISSION,_,_} = connection_peer(PEER_ID,ID),
        true = (PICTURES_PERMISSION =:= 1 orelse PICTURES_PERMISSION =:= 2 orelse PICTURES_PERMISSION =:= 3 orelse PICTURES_PERMISSION =:= 4),
        {ok,_,[[PICTURES]]} = erlcass:query(iolist_to_binary(["SELECT PICTURES FROM cham.CONNECTIONS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({PICTURES})};
      <<"CONNECTION_PEERS">> ->
        {ok,_,_,_,_,PEERS_PERMISSION,_} = connection_peer(PEER_ID,ID),
        true = (PEERS_PERMISSION =:= 1 orelse PEERS_PERMISSION =:= 2 orelse PEERS_PERMISSION =:= 3 orelse PEERS_PERMISSION =:= 4),
        {ok,_,[[PEERS]]} = erlcass:query(iolist_to_binary(["SELECT PEERS FROM cham.CONNECTIONS WHERE ID='",ID,"' ALLOW FILTERING"])),
        {ok,jiffy:encode({PEERS})};
      _ ->
        % MODE :
        %   0 :
        %     PARAM_1 : START_DATE
        %     PARAM_2 : END_DATE
        %   1 :
        %     PARAM_1 : START_DATE
        %   2 :
        %     PARAM_1 : END_DATE
        %   3 :
        %     PARAM_1 : CENTER_DATE
        %     PARAM_2 : GET_COUNT ( date <= item_date )
        %   4 :
        %     PARAM_1 : CENTER_DATE
        %     PARAM_2 : GET_COUNT ( date > item_date )
        {ok,_,_,_,_,_,MESSAGES_PERMISSION} = connection_peer(PEER_ID,ID),
        if
          (MESSAGES_PERMISSION =:= 1 orelse MESSAGES_PERMISSION =:= 3 orelse MESSAGES_PERMISSION =:= 4) ->
            case MODE of
              <<"0">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE>'",PARAM_1,"' AND DATE<='",PARAM_2,"' ORDER BY DATE DESC,PEER_ID DESC ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"1">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE>'",PARAM_1,"' ORDER BY DATE DESC,PEER_ID DESC ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"2">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE<='",PARAM_1,"' ORDER BY DATE DESC,PEER_ID DESC ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"3">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE>'",PARAM_1,"' ORDER BY DATE DESC,PEER_ID DESC LIMIT ",PARAM_2," ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"4">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE<='",PARAM_1,"' ORDER BY DATE ASC,PEER_ID ASC LIMIT ",PARAM_2," ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              _ -> {no}
            end;
          true ->
            {ok,PEER_HISTORY} = connection_history(PEER_ID,ID),
            case MODE of
              <<"0">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE>'",PARAM_1,"' AND DATE<='",if PARAM_2 < PEER_HISTORY -> PARAM_2; true -> PEER_HISTORY end,"' ORDER BY DATE DESC,PEER_ID DESC ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"1">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE>'",PARAM_1,"' AND DATE<='",PEER_HISTORY,"' ORDER BY DATE DESC,PEER_ID DESC ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"2">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE<='",if PARAM_1 < PEER_HISTORY -> PARAM_1; true -> PEER_HISTORY end,"' ORDER BY DATE DESC,PEER_ID DESC ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"3">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE>'",PARAM_1,"' AND DATE<='",PEER_HISTORY,"' ORDER BY DATE DESC,PEER_ID DESC LIMIT ",PARAM_2," ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              <<"4">> ->
                {ok,_,MESSAGES} = erlcass:query(iolist_to_binary(["SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2 FROM cham.MESSAGES WHERE CONNECTION_ID='",ID,"' AND DATE<='",if PARAM_1 < PEER_HISTORY -> PARAM_1; true -> PEER_HISTORY end,"' ORDER BY DATE ASC,PEER_ID ASC LIMIT ",PARAM_2," ALLOW FILTERING"])),
                {ok,[jiffy:encode(#{
                  <<"PEER_ID">> => MESSAGE_PEER_ID,
                  <<"CONNECTION_ID">> => MESSAGE_CONNECTION_ID,
                  <<"MODE">> => MESSAGE_MODE,
                  <<"MESSAGE_TYPE">> => MESSAGE_MESSAGE_TYPE,
                  <<"DATE">> => MESSAGE_DATE,
                  <<"FORWARD_ID">> => MESSAGE_FORWARD_ID,
                  <<"REPLY_DATE">> => MESSAGE_REPLY_DATE,
                  <<"DATA_1">> => MESSAGE_DATA_1,
                  <<"DATA_2">> => MESSAGE_DATA_2
                }) || [MESSAGE_PEER_ID,MESSAGE_CONNECTION_ID,MESSAGE_MODE,MESSAGE_MESSAGE_TYPE,MESSAGE_DATE,MESSAGE_FORWARD_ID,MESSAGE_REPLY_DATE,MESSAGE_DATA_1,MESSAGE_DATA_2] <- MESSAGES]};
              _ -> {no}
            end
        end
    end
  catch
    _:_ -> {no}
  end.

-spec peer_set(byte(),byte(),byte(),byte(),byte(),byte()) -> tuple().
peer_set(PEER_ID,ID,KEY,OPTION,VALUE,OPERATOR) ->
  try
    {ok,_,_,PHONE} = peer_phone(ID,<<"ID">>),
    {ok,PHONE_PERMISSION,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,STATE_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,CONNECTIONS_PERMISSION} = peer_peer(PEER_ID,ID),
    case KEY of
      <<"PHOME">> ->
        %%% have some code
        {no};
      <<"LINK">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (LINK_PERMISSION =:= 2),
        case VALUE of
          <<"">> ->
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET LINK='' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok} = link_remove(ID),
            {ok};
          _ ->
            {no} = link_get(VALUE,<<"LINK">>),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET LINK='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok} = link_set(ID,VALUE),
            {ok}
        end;
      <<"NAME">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (NAME_PERMISSION =:= 2),
        ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET NAME='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
        {ok};
      <<"BIO">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (BIO_PERMISSION =:= 2),
        ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET BIO='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
        {ok};
      <<"STATE">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (STATE_PERMISSION =:= 2),
        ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET STATE='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
        {ok};
      <<"PICTURES">> ->
        true = (OPERATOR =:= <<"ADD">> orelse OPERATOR =:= <<"REMOVE">>),
        case OPERATOR of
          <<"ADD">> ->
            true = (PICTURES_PERMISSION =:= 2 orelse PICTURES_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET PICTURES['",OPTION,"']='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok};
          _ ->
            true = (PICTURES_PERMISSION =:= 3 orelse PICTURES_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET PICTURES['",OPTION,"']=null WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok}
        end;
      <<"PEERS">> ->
        true = (OPERATOR =:= <<"ADD">> orelse OPERATOR =:= <<"REMOVE">>),
        case OPERATOR of
          <<"ADD">> ->
            true = (PEERS_PERMISSION =:= 2 orelse PEERS_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET PEERS['",OPTION,"']='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok};
          _ ->
            true = (PEERS_PERMISSION =:= 3 orelse PEERS_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET PEERS['",OPTION,"']=null WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok}
        end;
      <<"CONNECTIONS">> ->
        true = (OPERATOR =:= <<"ADD">> orelse OPERATOR =:= <<"REMOVE">>),
        case OPERATOR of
          <<"ADD">> ->
            true = (CONNECTIONS_PERMISSION =:= 2 orelse CONNECTIONS_PERMISSION =:= 4),
            {ok} = connection_set(PEER_ID,OPTION,<<"PEERS">>,PEER_ID,VALUE,<<"ADD">>),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET CONNECTIONS['",OPTION,"']='",VALUE,"' WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok};
          _ ->
            {ok} = connection_set(PEER_ID,OPTION,<<"PEERS">>,PEER_ID,VALUE,<<"REMOVE">>),
            true = (CONNECTIONS_PERMISSION =:= 3 orelse CONNECTIONS_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.PEERS SET CONNECTIONS['",OPTION,"']=null WHERE ID='",ID,"' AND PHONE='",PHONE,"'"])),
            {ok}
        end;
      _ -> {no}
    end
  catch
    _:_ -> {no}
  end.

-spec connection_cast(byte(),byte()) -> tuple().
connection_cast(ID,MESSAGE) ->
  CASTER = fun
           (_,[]) ->
             {ok};
           (CASTER,[{PEER_ID,PEER_PERMISSION}|TAIL]) ->
             if
               byte_size(PEER_PERMISSION) =:= 6 ->
                 case ((binary:at(PEER_PERMISSION,5) - 48) =:= 1 orelse (binary:at(PEER_PERMISSION,5) - 48) =:= 3 orelse (binary:at(PEER_PERMISSION,5) - 48) =:= 4) of
                   true ->
                     socket_cast(PEER_ID,MESSAGE),
                     CASTER(CASTER,TAIL);
                   _ ->
                     CASTER(CASTER,TAIL)
                 end;
               true ->
                 CASTER(CASTER,TAIL)
             end
         end,
  try
    {ok,_,[[PEERS]]} = erlcass:query(iolist_to_binary(["SELECT PEERS FROM cham.CONNECTIONS WHERE ID='",ID,"' ALLOW FILTERING"])),
    {ok} = CASTER(CASTER,PEERS),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec connection_peer(byte(),byte()) -> tuple().
connection_peer(PEER_ID,ID) ->
  SEARCHER = fun
               (_,[],RESULT,_) -> {ok,RESULT};
               (SEARCHER,[{TEMP_ID,TEMP_PERMISSION}|TAIL],RESULT,RANK) ->
                 TEMP_RANK = (length(PEER_ID) - length(PEER_ID--TEMP_ID)),
                 if
                   (TEMP_RANK > RANK)->
                     SEARCHER(SEARCHER,TAIL,{
                       binary:at(TEMP_PERMISSION,0) - 48,
                       binary:at(TEMP_PERMISSION,1) - 48,
                       binary:at(TEMP_PERMISSION,2) - 48,
                       binary:at(TEMP_PERMISSION,3) - 48,
                       binary:at(TEMP_PERMISSION,4) - 48,
                       binary:at(TEMP_PERMISSION,5) - 48
                     },TEMP_RANK);
                   true ->
                     SEARCHER(SEARCHER,TAIL,RESULT,RANK)
                 end

             end,
  try
    {ok,_,[[PEERS]]} = erlcass:query(iolist_to_binary(["SELECT PEERS FROM cham.CONNECTIONS WHERE ID='",ID,"'"])),
    {LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,MESSAGES_PERMISSION} = SEARCHER(SEARCHER,PEERS,{},0),
    {ok,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,MESSAGES_PERMISSION}
  catch
    _:_  -> {no}
  end.

-spec connection_history(byte(),byte()) -> tuple().
connection_history(PEER_ID,ID) ->
  try
    {ok,_,[[HISTORIES]]} = erlcass:query(iolist_to_binary(["SELECT HISTORIES FROM cham.CONNECTIONS WHERE ID='",ID,"'"])),
    case proplists:get_value(PEER_ID,HISTORIES) of
      undefined -> {no};
      PEER_DATE -> {ok,PEER_DATE}
    end
  catch
    _:_  -> {no}
  end.

-spec connection_create(byte(),byte()) -> tuple().
connection_create(PEER_ID,ID) ->
  try
    ok = erlcass:query(iolist_to_binary(["INSERT INTO cham.CONNECTIONS(ID,LINK,NAME,BIO,PICTURES,PEERS,HISTORIES) VALUES('",ID,"','','','',{},{'",PEER_ID,"':'222444','*':'111111','_':'111100'},{})"])), % * -> default join permission,_ -> any other peers not in connection
    {ok}
  catch
      _:_  -> {no}
  end.

-spec connection_delete(byte(),byte()) -> tuple().
connection_delete(PEER_ID,ID) ->
  DELETER = fun
             (_,[],_) ->
               {ok};
             (DELETER,[{OLD_ID,_}|TAIL],DATE) ->
               erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET HISTORIES['",OLD_ID,"']='",DATE,"' WHERE ID='",ID,"'"])),
               DELETER(DELETER,TAIL,DATE)
           end,
  try
    {ok,2,2,2,4,4,4} = connection_peer(PEER_ID,ID),
    {ok,_,[[PEERS]]} = erlcass:query(iolist_to_binary(["SELECT PEERS FROM cham.CONNECTIONS WHERE ID='",ID,"'"])),
    {ok} = DELETER(DELETER,PEERS,cham_utils:chrono_date()),
    link_remove(ID),
    ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET LINK='',NAME='',BIO='',PICTURES={},PEERS={'_':'000000'} WHERE ID='",ID,"'"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec connection_set(byte(),byte(),byte(),byte(),byte(),byte()) -> tuple().
connection_set(PEER_ID,ID,KEY,OPTION,VALUE,OPERATOR) ->
  try
    {ok,LINK_PERMISSION,NAME_PERMISSION,BIO_PERMISSION,PICTURES_PERMISSION,PEERS_PERMISSION,_} = connection_peer(PEER_ID,ID),
    case KEY of
      <<"LINK">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (LINK_PERMISSION =:= 2),
        case VALUE of
          <<"">> ->
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET LINK='' WHERE ID='",ID,"'"])),
            {ok} = link_remove(ID),
            {ok};
          _ ->
            {no} = link_get(VALUE,<<"LINK">>),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET LINK='",VALUE,"' WHERE ID='",ID,"'"])),
            {ok} = link_set(ID,VALUE),
            {ok}
        end;
      <<"NAME">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (NAME_PERMISSION =:= 2),
        ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET NAME='",VALUE,"' WHERE ID='",ID,"'"])),
        {ok};
      <<"BIO">> ->
        true = (OPERATOR =:= <<"SET">>),
        true = (BIO_PERMISSION =:= 2),
        ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET BIO='",VALUE,"' WHERE ID='",ID,"'"])),
        {ok};
      <<"PICTURES">> ->
        true = (OPERATOR =:= <<"ADD">> orelse OPERATOR =:= <<"REMOVE">>),
        case OPERATOR of
          <<"ADD">> ->
            true = (PICTURES_PERMISSION =:= 2 orelse PICTURES_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET PICTURES['",OPTION,"']='",VALUE,"' WHERE ID='",ID,"'"])),
            {ok};
          _ ->
            true = (PICTURES_PERMISSION =:= 3 orelse PICTURES_PERMISSION =:= 4),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET PICTURES['",OPTION,"']=null WHERE ID='",ID,"'"])),
            {ok}
        end;
      <<"PEERS">> ->
        true = (OPERATOR =:= <<"ADD">> orelse OPERATOR =:= <<"REMOVE">>),
        case OPERATOR of
          <<"ADD">> ->
            if
              (PEERS_PERMISSION =:= 2 orelse PEERS_PERMISSION =:= 4) ->
                {ok} = peer_set(PEER_ID,OPTION,<<"CONNECTIONS">>,ID,<<"1">>,<<"ADD">>),
                ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET PEERS['",OPTION,"']='",VALUE,"' WHERE ID='",ID,"'"])),
                {ok};
              true ->
                true = (OPTION =:= PEER_ID),
                %% get default add if exists add self -> *
                {ok} = peer_set(PEER_ID,OPTION,<<"CONNECTIONS">>,ID,<<"1">>,<<"ADD">>),
                {ok,DEFAULT_LINK_PERMISSION,DEFAULT_NAME_PERMISSION,DEFAULT_BIO_PERMISSION,DEFAULT_PICTURES_PERMISSION,DEFAULT_PEERS_PERMISSION,DEFAULT_MESSAGES_PERMISSION} = connection_peer(<<"*">>,ID),
                ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET PEERS['",OPTION,"']='",iolist_to_binary([DEFAULT_LINK_PERMISSION+48,DEFAULT_NAME_PERMISSION+48,DEFAULT_BIO_PERMISSION+48,DEFAULT_PICTURES_PERMISSION+48,DEFAULT_PEERS_PERMISSION+48,DEFAULT_MESSAGES_PERMISSION+48]),"' WHERE ID='",ID,"'"])),
                {ok}
            end;
          _ ->
            true = (PEERS_PERMISSION =:= 3 orelse PEERS_PERMISSION =:= 4 orelse OPTION =:= PEER_ID),
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET PEERS['",OPTION,"']=null WHERE ID='",ID,"'"])),
            % add date,id to history
            ok = erlcass:query(iolist_to_binary(["UPDATE cham.CONNECTIONS SET HISTORIES['",OPTION,"']='",cham_utils:chrono_date(),"' WHERE ID='",ID,"'"])),
            {ok}
        end
    end
  catch
    _:_  -> {no}
  end.

-spec message_set(byte(),byte(),byte(),byte(),byte(),byte(),byte(),byte()) -> tuple().
message_set(PEER_ID,CONNECTION_ID,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2) ->
  try
    {ok,_,_,_,_,_,MESSAGES_PERMISSION} = connection_peer(PEER_ID,CONNECTION_ID),
    true = (MESSAGES_PERMISSION =:= 2 orelse MESSAGES_PERMISSION =:= 3 orelse MESSAGES_PERMISSION =:= 4),
    ok = erlcass:query(iolist_to_binary(["INSERT INTO cham.MESSAGES(PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2) VALUES('",PEER_ID,"','",CONNECTION_ID,"','1','",MESSAGE_TYPE,"','",DATE,"','",FORWARD_ID,"','",REPLY_DATE,"','",DATA_1,"','",DATA_2,"')"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec message_delete(byte(),byte(),byte()) -> tuple().
message_delete(PEER_ID,CONNECTION_ID,DATE) ->
  try
    % message should be mine -> if im admin -> arg -> peerid
    {ok,_,[[SEND_PEER_ID]]} = erlcass:query(iolist_to_binary(["SELECT PEER_ID FROM cham.MESSAGES WHERE CONNECTION_ID='",CONNECTION_ID,"' AND DATE='",DATE,"' LIMIT 1 ALLOW FILTERING"])),
    case SEND_PEER_ID of
      PEER_ID ->
        ok = erlcass:query(iolist_to_binary(["DELETE FROM cham.MESSAGES WHERE CONNECTION_ID='",CONNECTION_ID,"' AND DATE='",DATE,"' AND PEER_ID='",PEER_ID,"'"])),
        {ok};
      _ ->
        {ok,_,_,_,_,_,MESSAGES_PERMISSION} = connection_peer(PEER_ID,CONNECTION_ID),
        true = (MESSAGES_PERMISSION =:= 4),
        ok = erlcass:query(iolist_to_binary(["DELETE FROM cham.MESSAGES WHERE CONNECTION_ID='",CONNECTION_ID,"' AND DATE='",DATE,"' AND PEER_ID='",PEER_ID,"'"])),
        {ok}
    end
  catch
    _:_ -> {no}
  end.

-spec message_edit(byte(),byte(),byte(),byte(),byte(),byte()) -> tuple().
message_edit(PEER_ID,CONNECTION_ID,DATE,DATA_1,DATA_2,REPLY_DATE) ->
  try
    % message should be mine
    ok = erlcass:query(iolist_to_binary(["UPDATE cham.MESSAGES SET DATA_1='",DATA_1,"',DATA_2='",DATA_2,"',REPLY_DATE='",REPLY_DATE,"' WHERE CONNECTION_ID='",CONNECTION_ID,"' AND DATE='",DATE,"' AND PEER_ID='",PEER_ID,"'"])),
    {ok}
  catch
    _:_ -> {no}
  end.

-spec message_read(byte(),byte(),byte()) -> tuple().
message_read(PEER_ID,CONNECTION_ID,DATE) ->
  try
    ok = erlcass:query(iolist_to_binary(["UPDATE cham.MESSAGES SET MODE='2' WHERE CONNECTION_ID='",CONNECTION_ID,"' AND DATE='",DATE,"' AND PEER_ID='",PEER_ID,"'"])),
    {ok}
  catch
    _:_ -> {no}
  end.

-spec stun_set(byte(),any(),integer(),atom(),atom(),byte()) -> tuple().
stun_set(PEER_ID,PEER_HOST,PEER_PORT,PEER_NODE,PEER_NAT,ID) ->
  try
    ok = erlcass:query(iolist_to_binary(["INSERT INTO cham.STUN(ID,HOST,PORT,NODE,NAT,PEER_ID) VALUES('",PEER_ID,"','",list_to_binary(inet:ntoa(PEER_HOST)),"','",integer_to_list(PEER_PORT),"','",atom_to_binary(PEER_NODE,utf8),"','",atom_to_binary(PEER_NAT,utf8),"','",ID,"')"])),
    {ok}
  catch
    _:_  ->  {no}
  end.

-spec stun_get(byte(),byte()) -> tuple().
stun_get(PEER_ID,ID) ->
  try
    {ok,_,[[HOST,PORT,NODE,NAT]]} = erlcass:query(iolist_to_binary(["SELECT HOST,PORT,NODE,NAT FROM cham.STUN WHERE ID='",ID,"' AND PEER_ID='",PEER_ID,"'"])),
    {ok, cham_utils:parse_address(binary_to_list(HOST)), binary_to_integer(PORT), binary_to_atom(NODE,utf8), binary_to_atom(NAT,utf8)}
  catch
    _:_  -> {no}
  end.

-spec stun_delete(byte(),byte()) -> tuple().
stun_delete(PEER_ID,ID) ->
  try
    ok = erlcass:query(iolist_to_binary(["DELETE FROM cham.STUN WHERE ID='",PEER_ID,"' AND PEER_ID='",ID,"'"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec turn_set(atom(),integer()) -> tuple().
turn_set(NODE,COUNT) ->
  try
    ok = erlcass:query(iolist_to_binary(["UPDATE cham.TURN SET CONNECTIONS=CONNECTIONS+'",integer_to_list(COUNT),"' WHERE NODE='",atom_to_binary(NODE,utf8),"'"])),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec turn_get() -> tuple().
turn_get() ->
  MINNER = fun
          (_,[],RESULT) -> RESULT;
          (MINNER,[[NODE,CONNECTIONS]|TAIL],{RESULT_NODE,RESULT_CONNECTIONS}) ->
            if
              CONNECTIONS < RESULT_CONNECTIONS ->
                MINNER(MINNER,TAIL,{NODE,CONNECTIONS});
              true ->
                MINNER(MINNER,TAIL,{RESULT_NODE,RESULT_CONNECTIONS})
            end
        end,
  try
    {ok,_,[ITEMS]} = erlcass:query(<<"SELECT NODE,CONNECTIONS FROM cham.TURN">>),
    {MIN_NODE,_} = MINNER(MINNER,ITEMS),
    {ok,binary_to_atom(MIN_NODE,utf8)}
  catch
    _:_  -> {no}
  end.