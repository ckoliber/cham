-module(cham_socket).
-author("koliber").
-record(cham_socket,{socket,peer_id}).
-export([
  cham_open/1,
  cham_recv/2,
  cham_error/2,
  cham_close/1
]).

-spec cham_open(any()) -> any().
cham_open(STATE) -> STATE.

-spec cham_recv(any(),byte()) -> any().
cham_recv(STATE,DATA) ->
  PARSER = fun
             (_,TEMP_STATE,[],RESULT) -> {ok,TEMP_STATE,RESULT};
             (PARSER,TEMP_STATE,[HEAD|TAIL],RESULT) ->
               case cham_parse_message(HEAD) of
                 {ok,DATA_TYPE,MESSAGE} ->
                   case cham_message(STATE,DATA_TYPE,MESSAGE) of
                     {ok} ->
                       PARSER(PARSER,TEMP_STATE,TAIL,RESULT);
                     {ok,MESSAGE_RESULT} ->
                       PARSER(PARSER,TEMP_STATE,TAIL,RESULT++[MESSAGE_RESULT]);
                     {ok,NEW_STATE,MESSAGE_RESULT} ->
                       PARSER(PARSER,NEW_STATE,TAIL,RESULT++[MESSAGE_RESULT]);
                     {no,ERROR} ->
                       PARSER(PARSER,TEMP_STATE,TAIL,RESULT++[cham_log(HEAD,ERROR)])
                   end;
                 _ ->
                   PARSER(PARSER,TEMP_STATE,TAIL,RESULT++[cham_log(HEAD,<<"BADMSG">>)])
               end
           end,
  case cham_parse_data(DATA) of
    {no} ->
      cham_send(STATE,[cham_log(null,<<"BADMSG">>)]),
      STATE;
    {ok,MESSAGES} ->
      {ok,NEW_STATE,RESULT} = PARSER(PARSER,STATE,MESSAGES,[]),
      cham_send(NEW_STATE,RESULT),
      STATE
  end.

-spec cham_error(any(),byte()) -> any().
cham_error(STATE,DATA) ->
  io:fwrite("~nUNEXPECTED ERROR : ~p ~n",[DATA]),
  STATE.

-spec cham_close(any()) -> any().
cham_close(STATE) ->
  case STATE#cham_socket.peer_id of
    null ->
      STATE;
    PEER_ID ->
      {ok} = cham_scylla_driver:peer_set(PEER_ID,PEER_ID,<<"SOCKETS">>,atom_to_binary(node(),utf8),ok,<<"REMOVE">>),
      {ok} = cham_scylla_driver:peer_set(PEER_ID,PEER_ID,<<"STATE">>,ok,cham_utils:chrono_date(),<<"SET">>),
      % cast reload peer
      STATE
  end.

-spec cham_send(any(),list()) -> tuple().
cham_send(STATE,MESSAGES) when MESSAGES =/= [] ->
  cham_server:send(STATE,jiffy:encode(MESSAGES)).

-spec cham_log(byte(),byte()) -> byte().
cham_log(MESSAGE,ERROR) ->
  try
    MESSAGE_MAP = jiffy:decode(MESSAGE,[return_maps]),
    jiffy:encode(#{
      <<"DATA_TYPE">> => maps:get(<<"DATA_TYPE">>,MESSAGE_MAP),
      <<"TOKEN">> => maps:get(<<"TOKEN">>,MESSAGE_MAP),
      <<"ERROR">> => ERROR
    })
  catch
    _:_  ->
      jiffy:encode(#{
        <<"DATA_TYPE">> => <<"ERROR">>,
        <<"TOKEN">> => <<"AUTO_TOKEN">>,
        <<"ERROR">> => ERROR
      })
  end.

-spec cham_parse_message(byte()) -> tuple().
cham_parse_message(MESSAGE) ->
  try
    MESSAGE_MAP = jiffy:decode(MESSAGE,[return_maps]),
    DATA_TYPE = maps:get(<<"DATA_TYPE">>,MESSAGE_MAP),
    {ok,DATA_TYPE,MESSAGE_MAP}
  catch
    _:_ -> {no}
  end.

-spec cham_parse_data(byte()) -> tuple().
cham_parse_data(DATA) ->
  try
    MESSAGES = jiffy:decode(DATA,[return_maps]),
    true = is_list(MESSAGES),
    {ok,MESSAGES}
  catch
    _:_ -> {no}
  end.

-spec cham_message(any(),byte(),map()) -> tuple().
cham_message(STATE,DATA_TYPE,MESSAGE) ->
  case DATA_TYPE of
    <<"PEER_CAPTCHA">> -> peer_captcha(STATE,MESSAGE);
    <<"PEER_LOGIN">> -> peer_login(STATE,MESSAGE);
    <<"PEER_LOGOUT">> -> peer_logout(STATE,MESSAGE);
    <<"PEER_CREATE">> -> peer_create(STATE,MESSAGE);
    <<"PEER_DELETE">> -> peer_delete(STATE,MESSAGE);
    <<"PEER_LOAD">> -> peer_load(STATE,MESSAGE);
    <<"PEER_SET">> -> peer_set(STATE,MESSAGE);
    <<"PEER_ID">> -> peer_id(STATE,MESSAGE);
    <<"CONNECTION_CREATE">> -> connection_create(STATE,MESSAGE);
    <<"CONNECTION_DELETE">> -> connection_delete(STATE,MESSAGE);
    <<"CONNECTION_SET">> -> connection_set(STATE,MESSAGE);
    <<"CONNECTION_ID">> -> connection_id(STATE,MESSAGE);
    <<"MESSAGE_SET">> -> message_set(STATE,MESSAGE);
    <<"MESSAGE_HANDLE">> -> message_handle(STATE,MESSAGE);
    _ ->
      {no,<<"BADMSG">>}
  end.

-spec peer_captcha(any(),map()) -> tuple().
peer_captcha(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =:= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    PHONE = maps:get(<<"PHONE">>,MESSAGE),
    TYPE = maps:get(<<"TYPE">>,MESSAGE),
    true = ((TYPE =:= <<"SMS">>) orelse (TYPE =:= <<"CALL">>)),
    {ok,CAPTCHA} = cham_utils:generate_captcha(),
    case cham_scylla_driver:captcha_set(PHONE,CAPTCHA) of
      {ok} ->
        case TYPE of
          <<"SMS">> ->
            case cham_utils:sms_message(PHONE,CAPTCHA) of
              {ok} ->
                {ok,STATE,jiffy:encode(#{
                  <<"DATA_TYPE">> => <<"PEER_CAPTCHA">>,
                  <<"TOKEN">> => TOKEN
                })};
              _ ->
                {no,<<"BADPEERSMS">>}
            end;
          _ ->
            case cham_utils:call_message(PHONE,CAPTCHA) of
              {ok} ->
                {ok,STATE,jiffy:encode(#{
                  <<"DATA_TYPE">> => <<"PEER_CAPTCHA">>,
                  <<"TOKEN">> => TOKEN
                })};
              _ ->
                {no,<<"BADPEERCALL">>}
            end
        end;
      _ ->
        {no,<<"BADPEERCAPTCHA">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_login(any(),map()) -> tuple().
peer_login(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =:= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    ID = maps:get(<<"ID">>,MESSAGE),
    SCODE = maps:get(<<"SCODE">>,MESSAGE),
    PLATFORM = maps:get(<<"PLATFORM">>,MESSAGE),
    VERSTION = maps:get(<<"VERSION">>,MESSAGE),
    case cham_scylla_driver:peer_is(ID,SCODE) of
      {ok} ->
        case cham_utils:platform_version(PLATFORM,VERSTION) of
          {ok} ->
            {ok} = cham_scylla_driver:peer_set(ID,ID,<<"SOCKETS">>,atom_to_binary(node(),utf8),term_to_binary(STATE#cham_socket.socket),<<"ADD">>),
            {ok} = cham_scylla_driver:peer_set(ID,ID,<<"STATE">>,ok,<<"online">>,<<"SET">>),
            % cast reload profile
            {ok,STATE#cham_socket{peer_id = ID},jiffy:encode(#{
              <<"DATA_TYPE">> => <<"PEER_LOGIN">>,
              <<"TOKEN">> => TOKEN
            })};
          {no,LINK} ->
            {no,jiffy:encode(#{<<"ERROR">> => <<"OLDVSN">>,<<"LINK">> => LINK})};
          _ ->
            {no,<<"BADPEERLOGIN">>}
        end;
      _ ->
        {no,<<"BADPEER">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_logout(any(),map()) -> tuple().
peer_logout(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    {ok,STATE#cham_socket{peer_id = null},jiffy:encode(#{
      <<"DATA_TYPE">> => <<"PEER_LOGOUT">>,
      <<"TOKEN">> => TOKEN
    })}
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_create(any(),map()) -> tuple().
peer_create(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =:= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    PHONE = maps:get(<<"PHONE">>,MESSAGE),
    CAPTCHA = maps:get(<<"CAPTCHA">>,MESSAGE),
    case cham_scylla_driver:captcha_get(PHONE) of
      {ok,CAPTCHA} ->
        case cham_scylla_driver:peer_phone(PHONE,<<"PHONE">>) of
          {ok,PEER_ID,PEER_SCODE,_} ->
            {ok,STATE,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"PEER_CREATE">>,
              <<"TOKEN">> => TOKEN,
              <<"ID">> => PEER_ID,
              <<"SCODE">> => PEER_SCODE
            })};
          _ ->
            {ok,ID,SCODE} = cham_utils:generate_peer(),
            {ok} = cham_scylla_driver:peer_create(ID,SCODE,PHONE),
            {ok,STATE,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"PEER_CREATE">>,
              <<"TOKEN">> => TOKEN,
              <<"ID">> => ID,
              <<"SCODE">> => SCODE
            })}
        end;
      {no,expire} ->
        {no,<<"BADEXPIRECAPTCHA">>};
      _ ->
        {no,<<"BADINCORRECTCAPTCHA">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_delete(any(),map()) -> tuple().
peer_delete(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    case cham_scylla_driver:peer_delete(STATE#cham_socket.peer_id) of
      {ok} ->
        {ok,STATE,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"PEER_DELETE">>,
          <<"TOKEN">> => TOKEN
        })};
      {no} ->
        {no,<<"BADPEERDELETE">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_load(any(),map()) -> tuple().
peer_load(STATE,MESSAGE) ->
  CASTER = fun
           (_,[],_) ->
             {ok};
           (CASTER,[VALUE|TAIL],TOKEN) ->
             cham_server:send(STATE,jiffy:encode(#{
               <<"DATA_TYPE">> => <<"PEER_LOAD">>,
               <<"TOKEN">> => TOKEN,
               <<"VALUE">> => VALUE
             })),
             CASTER(CASTER,TAIL,TOKEN)
         end,

  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    ID = maps:get(<<"ID">>,MESSAGE),
    TYPE = maps:get(<<"TYPE">>,MESSAGE),
    MODE = maps:get(<<"MODE">>,MESSAGE),
    PARAM_1 = maps:get(<<"PARAM_1">>,MESSAGE),
    PARAM_2 = maps:get(<<"PARAM_2">>,MESSAGE),
    case cham_scylla_driver:peer_load(STATE#cham_socket.peer_id,ID,TYPE,MODE,PARAM_1,PARAM_2) of
      {ok,VALUES} ->
        {ok} = CASTER(CASTER,VALUES,TOKEN),
        {ok,STATE,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"PEER_LOAD">>,
          <<"TOKEN">> => TOKEN
        })};
      _ ->
        {no,<<"BADPEERLOAD">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_set(any(),map()) -> tuple().
peer_set(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    ID = maps:get(<<"ID">>,MESSAGE),
    KEY = maps:get(<<"KEY">>,MESSAGE),
    OPTION = maps:get(<<"OPTION">>,MESSAGE),
    VALUE = maps:get(<<"VALUE">>,MESSAGE),
    OPERATOR = maps:get(<<"OPERATOR">>,MESSAGE),
    case cham_scylla_driver:peer_set(STATE#cham_socket.peer_id,ID,KEY,OPTION,VALUE,OPERATOR) of
      {ok} ->
        {ok} = message_handle(STATE#cham_socket{peer_id = ID},#{
          <<"TOKEN">> => TOKEN,
          <<"CONNECTION_ID">> => <<"">>,
          <<"HANDLE">> => <<"PEER_REFRESH">>
        }),
        {ok,STATE,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"PEER_SET">>,
          <<"TOKEN">> => TOKEN
        })};
      {no} ->
        {no,<<"BADPEERSET">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec peer_id(any(),map()) -> tuple().
peer_id(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    KEY = maps:get(<<"KEY">>,MESSAGE),
    TYPE = maps:get(<<"TYPE">>,MESSAGE),
    true = ((TYPE =:= <<"PHONE">>) orelse (TYPE =:= <<"LINK">>)),
      case TYPE of
        <<"PHONE">> ->
          case cham_scylla_driver:peer_phone(KEY,TYPE) of
            {ok,ID,_,_} ->
              {ok,STATE,jiffy:encode(#{
                <<"DATA_TYPE">> => <<"PEER_ID">>,
                <<"TOKEN">> => TOKEN,
                <<"ID">> => ID
              })};
            {no} ->
              {no,<<"BADPEERID">>}
          end;
        _ ->
          case cham_scylla_driver:link_get(KEY,TYPE) of
            {ok,ID,_} ->
              {ok,STATE,jiffy:encode(#{
                <<"DATA_TYPE">> => <<"PEER_ID">>,
                <<"TOKEN">> => TOKEN,
                <<"ID">> => ID
              })};
            {no} ->
              {no,<<"BADPEERID">>}
          end
      end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec connection_create(any(),map()) -> tuple().
connection_create(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    case cham_utils:generate_connection() of
      {ok,ID} ->
        case cham_scylla_driver:connection_create(STATE#cham_socket.peer_id,ID) of
          {ok} ->
            {ok,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"CONNECTION_CREATE">>,
              <<"TOKEN">> => TOKEN,
              <<"ID">> => ID
            })};
          _ ->
            {no,<<"BADCONNECTIONCREATE">>}
        end;
      {no} ->
        {ok,<<"BADCONNECTIONCREATE">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec connection_delete(any(),map()) -> tuple().
connection_delete(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    ID = maps:get(<<"ID">>,MESSAGE),
    case cham_scylla_driver:connection_delete(STATE#cham_socket.peer_id,ID) of
      {ok} ->
        {ok,STATE,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"CONNECTION_DELETE">>,
          <<"TOKEN">> => TOKEN
        })};
      {no} ->
        {no,<<"BADCONNECTIONDELETE">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec connection_set(any(),map()) -> tuple().
connection_set(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    ID = maps:get(<<"ID">>,MESSAGE),
    KEY = maps:get(<<"KEY">>,MESSAGE),
    OPTION = maps:get(<<"OPTION">>,MESSAGE),
    VALUE = maps:get(<<"VALUE">>,MESSAGE),
    OPERATOR = maps:get(<<"OPERATOR">>,MESSAGE),
    case cham_scylla_driver:connection_set(STATE#cham_socket.peer_id,ID,KEY,OPTION,VALUE,OPERATOR) of
      {ok} ->
        {ok} = message_handle(STATE,#{
          <<"TOKEN">> => TOKEN,
          <<"CONNECTION_ID">> => ID,
          <<"HANDLE">> => <<"CONNECTION_REFRESH">>
        }),
        {ok,STATE,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"CONNECTION_SET">>,
          <<"TOKEN">> => TOKEN
        })};
      {no} ->
        {no,<<"BADCONNECTIONSET">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec connection_id(any(),map()) -> tuple().
connection_id(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    KEY = maps:get(<<"KEY">>,MESSAGE),
    TYPE = maps:get(<<"TYPE">>,MESSAGE),
    true = ((TYPE =:= <<"LINK">>)),
    case TYPE of
      <<"LINK">> ->
        case cham_scylla_driver:link_get(KEY,TYPE) of
          {ok,ID,_} ->
            {ok,STATE,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"CONNECTION_ID">>,
              <<"TOKEN">> => TOKEN,
              <<"ID">> => ID
            })};
          {no} ->
            {no,<<"BADCONNECTIONID">>}
        end;
      _ ->
        {no,<<"BADMSG">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec message_set(any(),map()) -> tuple().
message_set(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    CONNECTION_ID = maps:get(<<"CONNECTION_ID">>,MESSAGE),
    MESSAGE_TYPE = maps:get(<<"MESSAGE_TYPE">>,MESSAGE),
    FORWARD_ID = maps:get(<<"FORWARD_ID">>,MESSAGE),
    REPLY_DATE = maps:get(<<"REPLY_DATE">>,MESSAGE),
    DATA_1 = maps:get(<<"DATA_1">>,MESSAGE),
    DATA_2 = maps:get(<<"DATA_2">>,MESSAGE),
    true = (
        (MESSAGE_TYPE =:= <<"TEXT">>) orelse
          (MESSAGE_TYPE =:= <<"THEME">>) orelse
          (MESSAGE_TYPE =:= <<"STICKER">>) orelse
          (MESSAGE_TYPE =:= <<"CONTACT">>) orelse
          (MESSAGE_TYPE =:= <<"LOCATION">>) orelse
          (MESSAGE_TYPE =:= <<"STREAM">>) orelse
          (MESSAGE_TYPE =:= <<"LOG">>) orelse
          (MESSAGE_TYPE =:= <<"EVENT">>)
    ),
    DATE = cham_utils:chrono_date(),
    case cham_scylla_driver:message_set(STATE#cham_socket.peer_id,CONNECTION_ID,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2) of
      {ok} ->
        case MESSAGE_TYPE of
          <<"EVENT">> ->
            {ok} = message_event(STATE#cham_socket.peer_id,CONNECTION_ID,REPLY_DATE,DATA_1,DATA_2),
            {ok} = cham_scylla_driver:connection_cast(CONNECTION_ID,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"MESSAGE_SET">>,
              <<"TOKEN">> => TOKEN,
              <<"MESSAGE_TYPE">> => MESSAGE_TYPE,
              <<"PEER_ID">> => STATE#cham_socket.peer_id,
              <<"CONNECTION_ID">> => CONNECTION_ID,
              <<"MODE">> => <<"1">>,
              <<"DATE">> => DATE,
              <<"FORWARD_ID">> => FORWARD_ID,
              <<"REPLY_DATE">> => REPLY_DATE,
              <<"DATA_1">> => DATA_1,
              <<"DATA_2">> => DATA_2
            })),
            {ok};
          _ ->
            {ok} = cham_scylla_driver:connection_cast(CONNECTION_ID,jiffy:encode(#{
              <<"DATA_TYPE">> => <<"MESSAGE_SET">>,
              <<"TOKEN">> => TOKEN,
              <<"MESSAGE_TYPE">> => MESSAGE_TYPE,
              <<"PEER_ID">> => STATE#cham_socket.peer_id,
              <<"CONNECTION_ID">> => CONNECTION_ID,
              <<"MODE">> => <<"1">>,
              <<"DATE">> => DATE,
              <<"FORWARD_ID">> => FORWARD_ID,
              <<"REPLY_DATE">> => REPLY_DATE,
              <<"DATA_1">> => DATA_1,
              <<"DATA_2">> => DATA_2
            })),
            {ok}
        end;
      _ ->
        {no,<<"BADMESSAGESET">>}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.

-spec message_event(byte(),byte(),byte(),byte(),byte()) -> tuple().
message_event(PEER_ID,CONNECTION_ID,REPLY_DATE,DATA_1,DATA_2) ->
  try
    case DATA_1 of
      <<"MESSAGE_DELETE">> ->
        [DELETE_ID,DELETE_DATE] = jiffy:decode(DATA_2),
        {ok} = cham_scylla_driver:message_delete(DELETE_ID,CONNECTION_ID,DELETE_DATE),
        {ok};
      <<"MESSAGE_EDIT">> ->
        [EDIT_DATE,EDIT_DATA_1,EDIT_DATA_2] = jiffy:decode(DATA_2),
        {ok} = cham_scylla_driver:message_edit(PEER_ID,CONNECTION_ID,EDIT_DATE,EDIT_DATA_1,EDIT_DATA_2,REPLY_DATE),
        {ok};
      <<"MESSAGE_READ">> ->
        [READ_ID,READ_DATE] = jiffy:decode(DATA_2),
        {ok} = cham_scylla_driver:message_read(READ_ID,CONNECTION_ID,READ_DATE),
        {ok};
      _ ->
        {no}
    end
  catch
    _:_ -> {no}
  end.

-spec message_handle(any(),map()) -> tuple().
message_handle(STATE,MESSAGE) ->
  try
    true = (STATE#cham_socket.peer_id =/= null),
    TOKEN = maps:get(<<"TOKEN">>,MESSAGE),
    CONNECTION_ID = maps:get(<<"CONNECTION_ID">>,MESSAGE),
    HANDLE = maps:get("HANDLE",MESSAGE),
    DATE = cham_utils:chrono_date(),
    case CONNECTION_ID of
      <<"">> ->
        {ok} = cham_scylla_driver:peer_cast(STATE#cham_socket.peer_id,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"MESSAGE_HANDLE">>,
          <<"TOKEN">> => TOKEN,
          <<"PEER_ID">> => STATE#cham_socket.peer_id,
          <<"CONNECTION_ID">> => CONNECTION_ID,
          <<"DATE">> => DATE,
          <<"HANDLE">> => HANDLE
        })),
        {ok};
      _ ->
        {ok} = cham_scylla_driver:connection_cast(CONNECTION_ID,jiffy:encode(#{
          <<"DATA_TYPE">> => <<"MESSAGE_HANDLE">>,
          <<"TOKEN">> => TOKEN,
          <<"PEER_ID">> => STATE#cham_socket.peer_id,
          <<"CONNECTION_ID">> => CONNECTION_ID,
          <<"DATE">> => DATE,
          <<"HANDLE">> => HANDLE
        })),
        {ok}
    end
  catch
    _:_ -> {no,<<"BADMSG">>}
  end.