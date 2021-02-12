-module(cham_server).
-author("koliber").
-behavior(gen_server).
-record(cham_socket,{socket,user_id}).
-export([start_link/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,send/2,close/1]).

start_link(SOCKET) ->
  gen_server:start_link(?MODULE, #cham_socket{socket = SOCKET, user_id = null},[]).

init(STATE) ->
  gen_server:cast(self(), accept),
  {ok,STATE}.

handle_call(_,_,STATE) ->
  {noreply, STATE}.

handle_cast(accept, STATE) ->
  {ok, SOCKET} = gen_tcp:accept(STATE#cham_socket.socket),
  cham_sup:start_socket(),
  {noreply, cham_socket:cham_open(#cham_socket{socket = SOCKET,user_id = null})}.

handle_info({tcp,_,DATA}, STATE) ->
  {noreply, cham_socket:cham_recv(STATE,DATA)};
handle_info({tcp_closed,_}, STATE) ->
  {stop,normal,cham_socket:cham_close(STATE)};
handle_info({tcp_error,_,_}, STATE) ->
  {stop, normal,cham_socket:cham_close(STATE)};
handle_info(DATA, STATE) ->
  {noreply, cham_socket:cham_error(STATE,DATA)}.

code_change(_OldVsn, STATE, _Extra) ->
  {ok, STATE}.

terminate(normal,_STATE) ->
  ok;
terminate(REASON,_STATE) ->
  io:fwrite("~nTerminate reason: ~p~n", [REASON]),
  cham_log:log("Node "++atom_to_list(node())++" stop Listening on port 1418 !").

send(STATE, DATA) ->
  gen_tcp:send(STATE#cham_socket.socket, DATA),
  gen_tcp:send(STATE#cham_socket.socket, <<"\r\n">>),
  inet:setopts(STATE#cham_socket.socket, [{active, once}]).

close(STATE) ->
  gen_tcp:close(STATE#cham_socket.socket),
  cham_socket:cham_close(STATE).