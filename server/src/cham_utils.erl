%%% Utils -> Crypto - BinToHex (Convertor) - Date
-module(cham_utils).
-author("koliber").
-export([
  platform_version/2,
  get_env/2,
  parse_address/1,
  start_ets/1,
  ip_to_integer/1,
  integer_to_ip/1,
  public_host/0,
  hash/2,
  encrypt/2,
  decrypt/2,
  chrono_date/0,
  chdate_to_num/1,
  num_to_chdate/1,
  generate_peer/0,
  generate_connection/0,
  generate_captcha/0,
  sms_message/2,
  call_message/2
]).

-spec platform_version(byte(),byte()) -> byte().
platform_version(PLATFORM,VERSION) ->
  case PLATFORM of
    <<"ANDROID">> ->
      case list_to_binary(get_env(cham,platform_android_version)) of
        VERSION -> {ok};
        _ -> {no,list_to_binary(get_env(cham,platform_android_link))}
      end;
    <<"DESKTOP">> ->
      case list_to_binary(get_env(cham,platform_desktop_version)) of
        VERSION -> {ok};
        _ -> {no,list_to_binary(get_env(cham,platform_desktop_link))}
      end;
    <<"IOS">> ->
      case list_to_binary(get_env(cham,platform_ios_version)) of
        VERSION -> {ok};
        _ -> {no,list_to_binary(get_env(cham,platform_ios_link))}
      end;
    _ -> {no}
  end.

-spec get_env(atom(),atom()) -> list().
get_env(Application,Key) ->
  {ok,Value} = application:get_env(Application,Key),
  Value.

-spec parse_address(list()) -> any().
parse_address(HOST) ->
  try
    {ok,HOST_ADDRESS} = inet:parse_address(HOST),
    HOST_ADDRESS
  catch
    _:_  -> no
  end.

-spec start_ets(atom()) -> tuple().
start_ets(NAME) ->
  try
    spawn(fun() ->
      NAME = ets:new(NAME,[set,public,named_table]),
      LOOP = fun(LOOP) ->
        receive
          _ -> LOOP(LOOP)
        end
                end,
      LOOP(LOOP)
          end),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec ip_to_integer(list()) -> tuple().
ip_to_integer(IP) ->
  try
    Sagments_List = string:tokens(IP,"."),
    IP_Decimal = (( list_to_integer(lists:nth(1,Sagments_List)) * 16777216 ) + ( list_to_integer(lists:nth(2,Sagments_List)) * 65536 ) + ( list_to_integer(lists:nth(3,Sagments_List)) * 256 ) + ( list_to_integer(lists:nth(4,Sagments_List)) )),
    {ok,IP_Decimal}
  catch
    _:_  -> {no}
  end.

-spec integer_to_ip(integer()) -> tuple().
integer_to_ip(INTEGER) ->
  try
    SagmentA = round(INTEGER / 16777216),
    SagmentB = round( (INTEGER rem 16777216) / 65536 ),
    SagmentC = round( (INTEGER rem 65536) / 256 ),
    SagmentD = round( (INTEGER rem 256) ),
    {ok,integer_to_list(SagmentA)++"."++integer_to_list(SagmentB)++"."++integer_to_list(SagmentC)++"."++integer_to_list(SagmentD)}
  catch
    _:_  -> {no}
  end.

-spec public_host() -> tuple().
public_host() ->
  try
    {ok, {{_,_,_},_,HOST}} = httpc:request(get, {"http://api.ipify.org", []}, [], []),
    {ok,HOST}
  catch
      _:_  -> {no}
  end.

%%  State = crypto:stream_init(rc4,"PASS"),

-spec hash(atom(),byte()) -> tuple().
hash(ALGORITHM,DATA) ->
  %% Algorythms -> md5 - sha254 - sha1 ...
  try
    {ok,list_to_binary(hex:bin_to_hexstr(crypto:hash(ALGORITHM,DATA)))}
  catch
    _:_  -> {no}
  end.

-spec encrypt(any(),list()) -> tuple().
encrypt(STATE,TEXT) ->
  try
    {_,CIPHER} = crypto:stream_encrypt(STATE,TEXT),
    {ok,list_to_binary(hex:bin_to_hexstr(CIPHER))}
  catch
    _:_  -> {no}
  end.

-spec decrypt(any(),list()) -> tuple().
decrypt(STATE,CIPHER) ->
  try
    {_,TEXT} = crypto:stream_decrypt(STATE,hex:hexstr_to_bin(binary_to_list(CIPHER))),
    {ok,TEXT}
  catch
    _:_  -> {no}
  end.

-spec chrono_date() -> byte().
chrono_date() ->
  {Mega,Sec,Micro} = erlang:now(),
  integer_to_binary((Mega*1000000+Sec)*1000000+Micro).

-spec chdate_to_num(byte()) -> tuple().
chdate_to_num(CHDATE_BIN) ->
  try
    {ok,binary_to_integer(CHDATE_BIN)}
  catch
    _:_  -> {no}
  end.

-spec num_to_chdate(integer()) -> tuple().
num_to_chdate(NUMBER) ->
  try
    integer_to_binary(NUMBER)
  catch
    _:_  -> {no}
  end.

-spec generate_peer() -> tuple().
generate_peer() ->
  try
    ID = list_to_binary(zuuid:string(zuuid:v1())),
    SCODE = list_to_binary(zuuid:string(zuuid:v4())),
    {ok,ID,SCODE}
  catch
    _:_  -> {no}
  end.

-spec generate_connection() -> tuple().
generate_connection() ->
  try
    ID = list_to_binary(zuuid:string(zuuid:v4())),
    {ok,ID}
  catch
    _:_  -> {no}
  end.

-spec generate_captcha() -> tuple().
generate_captcha() ->
  try
    {_,_,TIME_INT} = erlang:now(),
    TIME_LIST = integer_to_list(TIME_INT),
    LEN_INT =  6 - string:len(TIME_LIST),
    if
      LEN_INT > 0 -> {ok,integer_to_list(list_to_integer(float_to_list(math:pow(10,LEN_INT+1) - 1)))++TIME_LIST};
      true -> {ok,list_to_binary(TIME_LIST)}
    end
  catch
    _:_  -> {no}
  end.

-spec sms_message(byte(),byte()) -> tuple().
sms_message(PHONE,CAPTCHA) ->
  try
    {ok, {{_,_,_},_,RESULT_LIST}} = httpc:request(post, {"http://37.130.202.188/api/select", [], "application/json", jiffy:encode(#{
      <<"uname">> => list_to_binary(cham_utils:get_env(cham,sms_user)),
      <<"pass">> => list_to_binary(cham_utils:get_env(cham,sms_pass)),
      <<"from">> => list_to_binary(cham_utils:get_env(cham,sms_number)),
      <<"message">> => iolist_to_binary(["ChaM Security Code : ",CAPTCHA]),
      <<"op">> => <<"send">>,
      <<"to">> => list_to_binary(lists:nth(2,string:tokens(binary_to_list(PHONE),"_")))
    })}, [], []),
     [0|_] = jiffy:decode(RESULT_LIST,[]),
    {ok}
  catch
    _:_  -> {no}
  end.

-spec call_message(byte(),byte()) -> tuple().
call_message(PHONE,CAPTCHA) ->
  try
    {ok, {{_,_,_},_,RESULT_LIST}} = httpc:request(post, {"http://37.130.202.188/api/select", [], "application/json", jiffy:encode(#{
      <<"uname">> => list_to_binary(cham_utils:get_env(cham,sms_user)),
      <<"pass">> => list_to_binary(cham_utils:get_env(cham,sms_pass)),
      <<"from">> => list_to_binary(cham_utils:get_env(cham,sms_number)),
      <<"message">> => iolist_to_binary(["ChaM Security Code : ",CAPTCHA]),
      <<"op">> => <<"send">>,
      <<"to">> => list_to_binary(lists:nth(2,string:tokens(binary_to_list(PHONE),"_")))
    })}, [], []),
    [0|_] = jiffy:decode(RESULT_LIST,[]),
    {ok}
  catch
    _:_  -> {no}
  end.