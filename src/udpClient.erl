%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jun 2017 19:43
%%%-------------------------------------------------------------------
-module(udpClient).
-export([start/1, client/1]).

start(ServerPid) ->
  spawn(fun() -> server(4000, ServerPid) end).

server(Port, ServerPid) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  io:format("server opened socket:~p~n",[Socket]),
  loop(Socket, ServerPid).

loop(Socket, ServerPid) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, Host, Port, Bin} ->
      gen_server:call(ServerPid, Bin),
      %io:format("server received:~p~n",[Bin]),
      gen_udp:send(Socket, Host, Port, Bin),
      loop(Socket, ServerPid)
  end.

% Client code
client(N) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  io:format("client opened socket=~p~n",[Socket]),
  ok = gen_udp:send(Socket, "192.168.1.7", 4000, N),
  Value = receive
            {udp, Socket, _, _, Bin} ->
              io:format("client received:~p~n",[Bin])
          after 2000 ->
      0
          end,
  gen_udp:close(Socket),
  Value.