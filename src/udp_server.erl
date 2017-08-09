-module(udp_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).
-export([send/1]).

-behaviour(gen_server).
-include("ErlangTanks.hrl").
-include("data.hrl").
-define(SERVER_PORT, 4000).
-define(CLIENT_PORT, 18001).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(_Params) ->
  process_flag(trap_exit, true),
  {ok, Sock} = gen_udp:open(?SERVER_PORT),
  io:format("Init Sock ~p~n", [Sock]),
  io:format("UDP Server online ~n"),
  F = fun() ->
    mnesia:write(#udp_state{socket = Sock,connections = []}) end,
  mnesia:transaction(F),
  {ok, {[], Sock}}.

terminate(_Reason, {Connections, Sock}) ->
  io:format("UDP Server terminated~n"),
  lists:foreach(fun(A) ->
    gen_udp:send(Sock, A, ?SERVER_PORT, list_to_binary("exit"))
                end, Connections),
  lists:foreach(fun({Ip, _Pid}) ->

    gen_udp:send(Sock, Ip, ?SERVER_PORT, list_to_binary("exit"))
                end, ets:tab2list(ids)),

  gen_udp:close(Sock).

handle_cast({exit,Ip}, {Connections,Sock}) ->
  NewConnections = lists:delete(Ip,Connections),
  gen_udp:send(Sock, Ip, ?SERVER_PORT, list_to_binary("exit")),
  {noreply, {NewConnections,Sock}};
handle_cast({new_ip,Ip}, {Connections,Sock}) ->
  {A,B,C,D} = (local_ip_v4()),
  Address = "reco " ++ integer_to_list(A)++"."++integer_to_list(B)++"."++integer_to_list(C)++"."++integer_to_list(D)++".",
  io:format("~nsending: ~p~n",[Address]),
  gen_udp:send(Sock, Ip, ?SERVER_PORT, list_to_binary(Address)),
  {noreply, {Connections,Sock}}.

%Receives data packets from the players android devices
handle_info({udp, _Client, Ip, _Port, Msg}, {Connections,Sock}) ->
  Temp = re:split(Msg, "[ ]",[{return,list}]),
  case Temp of
    [_PlayerName, "connection","successful"] ->
      NewConnections = [Ip|Connections],
      F = fun() ->
        mnesia:write(#udp_state{socket = Sock,connections = NewConnections}) end,
      mnesia:transaction(F);
    ["exit"]  ->
      NewConnections = lists:delete(Ip,Connections),
      io:format("~p has left the room~n", [Ip]),
      F = fun() ->
        mnesia:write(#udp_state{socket = Sock,connections = NewConnections}) end,
      mnesia:transaction(F);
    _A -> NewConnections = Connections
  end,
  gen_server:cast(game_manager, {Sock,Ip,Msg}),
  {noreply, {NewConnections,Sock}};

handle_info(Msg, LoopData) ->
  io:format("receive info ~p~n", [Msg]),
  {noreply, LoopData}.

send(Msg) ->
  gen_server:call(?MODULE, {message, Msg}).

handle_call(exit, _From, {Connections,Sock}) ->
  lists:foreach(fun(A) ->
    gen_udp:send(Sock, A, ?SERVER_PORT, list_to_binary("exit"))
                end, Connections),
  {reply, ok, {[],Sock}}.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

local_ip_v4() ->
  {ok, Addrs} = inet:getifaddrs(),
  hd([Addr || {_, Opts} <- Addrs,
    {addr, Addr} <- Opts,
    size(Addr) == 4, Addr =/= {127,0,0,1}]).