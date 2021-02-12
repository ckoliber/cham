-module(cham_sup).
-behaviour(supervisor).
-define(ETS_STUN,ets_stun).
-define(TCP_OPTIONS,[binary,{packet,line},{active,once},{buffer,16384},{reuseaddr,true}]).
-export([start_link/0,init/1,start_socket/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    [] = os:cmd("epmd -daemon"),
    {ok,_} = net_kernel:start([list_to_atom(cham_utils:get_env(cham,cham_kernel_sname)++"@"++cham_utils:get_env(cham,cham_local_host)),longnames]),
    true = erlang:set_cookie(node(),list_to_atom(cham_utils:get_env(cham,cham_kernel_cookie))),
%%    true = net_kernel:connect(list_to_atom(?KERNEL_SNAME++"@"++"127.0.0.1")), % connect to one node of cluster
    ok = ssl:start(),
    application:start(inets),
    ok = zuuid:start(),
    {ok,HOST} = cham_utils:public_host(),
    application:set_env(cham,cham_public_host,HOST),
    {ok} = cham_utils:start_ets(?ETS_STUN),
    {ok} = cham_scylla_driver:driver_connect(),
    {ok, SOCKET} = gen_tcp:listen(cham_utils:get_env(cham,cham_socket_port), ?TCP_OPTIONS),
    cham_log:log("Node "++atom_to_list(node())++" is Listening on port 1418 !"),
    %% start 20 acceptor !!
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
        [{cham_server,
            {cham_server, start_link, [SOCKET]},
            temporary, 1000, worker, [cham_server]}
        ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.

