-module(udp_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([send/1]).

-behaviour(gen_server).

-define(SERVER_PORT, 4000).
-define(CLIENT_PORT, 18001).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(_Params) ->
  {ok, Sock} = gen_udp:open(?SERVER_PORT),
  io:format("Init Sock ~p~n", [Sock]),
  io:format("UDP Server online ~n"),
  {ok, {[], Sock}}.

terminate(_Reason, {_Connections, Sock}) ->
  io:format("UDP Server terminated ~n"),
  gen_udp:close(Sock).

handle_cast(stop, {Connections, Sock}) ->
  {stop, normal, {Connections, Sock}}.

handle_info({udp, _Client, Ip, _Port, Msg}, {Connections,Sock}) ->
  Temp = re:split(Msg, "[ ]",[{return,list}]),
  io:format("~p ~n",[Temp]),
  case Temp of
    [_PlayerName, "connection","successful"] ->
      NewConnections = [Ip|Connections];
    ["exit"]  ->
      NewConnections = lists:delete(Ip,Connections),
      io:format("~n ~p has left the room~n", [Ip]);
    _A -> NewConnections = Connections
  end,
  gen_server:call(game_manager, {Sock,Ip,Msg}),
  {noreply, {NewConnections,Sock}};

handle_info(Msg, LoopData) ->
  io:format("receive info ~p~n", [Msg]),
  {noreply, LoopData}.

send(Msg) ->

  gen_server:call(?MODULE, {message, Msg}).

handle_call(exit, _From, {Connections,Sock}) ->
  %io:format(gen_udp:send(Sock, "192.168.14.166", 4000,  list_to_binary("exit"))),
  lists:foreach(fun(A) ->
    gen_udp:send(Sock, A, ?SERVER_PORT, list_to_binary("exit"))
                end, Connections),
  {reply, ok, {[],Sock}};

handle_call({exit,Ip}, _From, {Connections,Sock}) ->
  io:format("trying to send kill"),
  NewConnections = lists:delete(Ip,Connections),
  gen_udp:send(Sock, Ip, ?SERVER_PORT, list_to_binary("exit")),
  {reply, ok, {NewConnections,Sock}}.
